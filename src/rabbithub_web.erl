%% @doc Web server for rabbithub.

-module(rabbithub_web).

-export([start/1, stop/0, loop/2]).

-define(APPLICATION_XSLT, "/static/application.xsl.xml").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    {Path, Query, _Fragment} = mochiweb_util:urlsplit_path(Req:get(raw_path)),
    ParsedQuery = mochiweb_util:parse_qs(Query),
    case re:split(Path, "/", [{parts, 4}]) of
        [<<>>, <<"static">> | _] ->
            handle_static(Path, DocRoot, Req);
        [<<>>, <<>>] ->
            handle_static(Path, DocRoot, Req);
        [<<>>, Facet, ResourceType, Name] ->
            case check_resource_type(ResourceType) of
                {ok, ResourceTypeAtom} ->
                    handle_request(ResourceTypeAtom,
                                   binary_to_list(Facet),
                                   binary_to_list(Name),
                                   ParsedQuery,
                                   Req);
                {error, invalid_resource_type} ->
                    Req:not_found()
            end
    end.

%% Internal API

check_resource_type(<<"x">>) -> {ok, exchange};
check_resource_type(<<"q">>) -> {ok, queue};
check_resource_type(_) -> {error, invalid_resource_type}.

check_facet('POST', "endpoint", "", _) -> {auth_required, [write]};
check_facet('PUT', "endpoint", "", queue) -> auth_not_required; %% special case: see implementation
check_facet('PUT', "endpoint", "", _) -> {auth_required, [configure]};
check_facet('DELETE', "endpoint", "", _) -> {auth_required, [configure]};
check_facet('GET', "endpoint", "", _) -> auth_not_required;
check_facet('GET', "endpoint", "generate_token", _) -> {auth_required, [write]};
check_facet('GET', "endpoint", "subscribe", _) -> auth_not_required;
check_facet('GET', "endpoint", "unsubscribe", _) -> auth_not_required;
check_facet('POST', "subscribe", "subscribe", _) -> {auth_required, [read]};
check_facet('POST', "subscribe", "unsubscribe", _) -> {auth_required, []};
check_facet(Method, Facet, HubMode, ResourceType) ->
    error_logger:warning_report({check_facet, invalid_operation,
                                 {Method, Facet, HubMode, ResourceType}}),
    invalid_operation.

handle_static("/" ++ StaticFile, DocRoot, Req) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            Req:serve_file(StaticFile, DocRoot);
        'POST' ->
            case StaticFile of
                "" -> handle_hub_post(Req);
                _ -> Req:not_found()
            end;
        _ ->
            Req:respond({501, [], "Invalid HTTP method"})
    end;
handle_static(_OtherPath, _DocRoot, Req) ->
    Req:respond({400, [], "Invalid path"}).

param(ParsedQuery, Key, DefaultValue) ->
    case lists:keysearch(Key, 1, ParsedQuery) of
        {value, {_Key, Value}} ->
            Value;
        false ->
            DefaultValue
    end.

check_auth(Req, Resource, PermissionsRequired, Fun) ->
    rabbithub_auth:check_authentication
      (Req, fun (Username) ->
                    rabbithub_auth:check_authorization
                      (Req, Resource, Username, PermissionsRequired,
                       fun () ->
                               Fun(Username)
                       end)
            end).

handle_request(ResourceType, Facet, Name, ParsedQuery, Req) ->
    HubMode = param(ParsedQuery, "hub.mode", ""),
    Method = Req:get(method),
    Resource = rabbithub:r(ResourceType, Name),
    case check_facet(Method, Facet, HubMode, ResourceType) of
        {auth_required, PermissionsRequired} ->
            check_auth(Req, Resource, PermissionsRequired,
                       fun (_Username) ->
                               perform_request(Method,
                                               list_to_atom(Facet),
                                               list_to_atom(HubMode),
                                               ResourceType,
                                               Resource,
                                               ParsedQuery,
                                               Req)
                       end);
        auth_not_required ->
            perform_request(Method,
                            list_to_atom(Facet),
                            list_to_atom(HubMode),
                            ResourceType,
                            Resource,
                            ParsedQuery,
                            Req);
        invalid_operation ->
            Req:respond({400, [], "Invalid operation"})
    end.

request_host(Req) ->
    case Req:get_header_value("host") of
        undefined ->
            rabbithub:canonical_host();
        V ->
            V
    end.

canonical_url(Req, Path) ->
    mochiweb_util:urlunsplit({rabbithub:canonical_scheme(),
                              request_host(Req),
                              Path,
                              "",
                              ""}).

application_descriptor(Name, Class, Parameters, Facets) ->
    {application, [{name, [Name]},
                   {class, [Class]},
                   {parameters, [{parameter, [{name, N}, {value, V}], []}
                                 || {N, V} <- Parameters]},
                   {facets, Facets}]}.

desc_action(HubMode, HttpMethod, Name, Description, Params) ->
    {action, [{'hub.mode', HubMode}, {'http.method', HttpMethod}, {name, Name}],
     [{description, [Description]} | Params]}.

desc_param(Name, Location, Attrs, Description) ->
    {parameter, [{name, Name}, {location, Location} | Attrs],
     [{description, [Description]}]}.

endpoint_facet(Req, Path) ->
    {facet, [{href, canonical_url(Req, Path)}],
     [{description, ["Facet permitting delivery of pubsub messages into the application."]},
      desc_action("", "POST", "deliver",
                  "Deliver a message to the endpoint.",
                  [desc_param("hub.topic", "query", [{defaultvalue, ""}],
                              "The routing key to use for the delivery."),
                   desc_param("content-type", "headers", [],
                              "The content-type of the body to deliver."),
                   desc_param("body", "body", [],
                              "The body of the HTTP request is used as the message to deliver.")]),
      desc_action("", "PUT", "create",
                  "Create an endpoint.",
                  [desc_param("amqp.exchange_type", "query", [{defaultvalue, "fanout"},
                                                              {optional, "true"}],
                              "(When creating an exchange) Specifies the AMQP exchange type.")]),
      desc_action("", "DELETE", "destroy",
                  "Destroy the endpoint.",
                  []),
      desc_action("", "GET", "info",
                  "Retrieve a description of the endpoint.",
                  [])]}.

%% FIXME use the #resource record or do something else:
declare_queue(Resource = {resource, _VHost, queue, QueueNameBin}, _ParsedQuery, Req) ->
    check_auth(Req, Resource, [configure],
               fun (_Username) ->
                       case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
                           {ok, _} ->
                               Req:respond({204, [], []});
                           {error, not_found} ->
                               rabbithub:rabbit_call(rabbit_amqqueue, declare,
                                                     [Resource, true, false, []]),
                               QN = binary_to_list(QueueNameBin),
                               Req:respond({201,
                                            [{"Location",
                                              canonical_url(Req, "/endpoint/q/" ++ QN)}],
                                            []})
                       end
               end).

extract_message(ExchangeResource, ParsedQuery, Req) ->
    RoutingKey = param(ParsedQuery, "hub.topic", ""),
    ContentTypeBin = case Req:get_header_value("content-type") of
                         undefined -> undefined;
                         S -> list_to_binary(S)
                     end,
    Body = Req:recv_body(),
    %% FIXME: doing things this way causes the body to travel across
    %% the network three times!! Twice for the rabbit_basic:message
    %% call, once for the publish or delivery itself.
    rabbithub:rabbit_call(rabbit_basic, message,
                          [ExchangeResource, list_to_binary(RoutingKey), ContentTypeBin, Body]).

perform_request('POST', endpoint, '', exchange, Resource, ParsedQuery, Req) ->
    Msg = extract_message(Resource, ParsedQuery, Req),
    case rabbithub:rabbit_call(rabbit_basic, publish, [false, false, none, Msg]) of
        {ok, _, _} ->
            Req:respond({202, [], []});
        {error, not_found} ->
            Req:not_found()
    end;

perform_request('POST', endpoint, '', queue, Resource, ParsedQuery, Req) ->
    Msg = extract_message(rabbithub:r(exchange, ""), ParsedQuery, Req),
    IsMandatory = case param(ParsedQuery, "amqp.mandatory", "") of
                      "true" -> true;
                      _ -> false
                  end,
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        %% TODO use the record or think of something else other than this ugly solution:
        {ok, {amqqueue, _Name, _Durable, _AutoDelete, _Arguments, QPid}} ->
            true = rabbithub:rabbit_call(rabbit_amqqueue, deliver,
                                         [IsMandatory, false, none, Msg, QPid]),
            Req:respond({case IsMandatory of true -> 204; false -> 202 end, [], []});
        {error, not_found} ->
            Req:not_found()
    end;

perform_request('PUT', endpoint, '', exchange, Resource, ParsedQuery, Req) ->
    ExchangeTypeBin = list_to_binary(param(ParsedQuery, "amqp.exchange_type", "fanout")),
    case catch rabbithub:rabbit_call(rabbit_exchange, check_type, [ExchangeTypeBin]) of
        {'EXIT', _} ->
            Req:respond({400, [], "Invalid exchange type"});
        ExchangeType ->
            case rabbithub:rabbit_call(rabbit_exchange, lookup, [Resource]) of
                {ok, _} ->
                    Req:respond({204, [], []});
                {error, not_found} ->
                    rabbithub:rabbit_call(rabbit_exchange, declare,
                                          [Resource, ExchangeType, true, false, []]),
                    Req:respond({201, [{"Location", canonical_url(Req, Req:get(path))}], []})
            end
    end;

perform_request('PUT', endpoint, '', queue, UncheckedResource, ParsedQuery, Req) ->
    case UncheckedResource of
        %% FIXME either use the record or do something else:
        {resource, _VHost, queue, <<>>} ->
            V = rabbithub:r(queue, rabbithub:binstring_guid("amq.gen.http")),
            declare_queue(V, ParsedQuery, Req);
        %% FIXME should use rabbit_channel:check_name/2, but that's not exported:
        {resource, _Vhost, queue, <<"amq.", _/binary>>} ->
            Req:respond({400, [], "Invalid queue name"});
        V ->
            declare_queue(V, ParsedQuery, Req)
    end;

perform_request('DELETE', endpoint, '', exchange, Resource, _ParsedQuery, Req) ->
    case Resource of
        %% FIXME either use the record or do something else:
        {resource, _VHost, exchange, <<>>} ->
            Req:respond({403, [], "Deleting the default exchange is not permitted"});
        _ ->
            case rabbithub:rabbit_call(rabbit_exchange, delete, [Resource, false]) of
                {error, not_found} ->
                    Req:not_found();
                ok ->
                    Req:respond({204, [] ,[]})
            end
    end;

perform_request('DELETE', endpoint, '', queue, Resource, _ParsedQuery, Req) ->
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        {error, not_found} ->
            Req:not_found();
        {ok, Q} ->
            {ok, _PurgedMessageCount} = rabbithub:rabbit_call(rabbit_amqqueue, delete,
                                                              [Q, false, false]),
            Req:respond({204, [] ,[]})
    end;

perform_request('GET', endpoint, '', exchange, Resource, _ParsedQuery, Req) ->
    case rabbithub:rabbit_call(rabbit_exchange, lookup, [Resource]) of
        {error, not_found} ->
            Req:not_found();
        %% FIXME use the records or do something else:
        {ok, {exchange, {resource, _VHost, exchange, XNameBin}, Type,
              _Durable, _AutoDelete, _Arguments}} ->
            XN = binary_to_list(XNameBin),
            Xml = application_descriptor(XN, "amqp.exchange",
                                         [{"amqp.exchange_type", atom_to_list(Type)}],
                                         [endpoint_facet(Req, "/endpoint/x/" ++ XN)]),
            rabbithub:respond_xml(Req, 200, [], ?APPLICATION_XSLT, Xml)
    end;

perform_request('GET', endpoint, '', queue, Resource, _ParsedQuery, Req) ->
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        {error, not_found} ->
            Req:not_found();
        %% FIXME use the records or do something else:
        {ok, {amqqueue, {resource, _VHost, queue, QNameBin},
              _Durable, _AutoDelete, _Arguments, _QPid}} ->
            QN = binary_to_list(QNameBin),
            Xml = application_descriptor(QN, "amqp.queue",
                                         [],
                                         [endpoint_facet(Req, "/endpoint/q/" ++ QN)]),
            rabbithub:respond_xml(Req, 200, [], ?APPLICATION_XSLT, Xml)
    end;

perform_request(Method, Facet, HubMode, _ResourceType, Resource, ParsedQuery, Req) ->
    Xml = {root, [{method, [atom_to_list(Method)]},
                  {facet, [atom_to_list(Facet)]},
                  {hubmode, [atom_to_list(HubMode)]},
                  {resource, [rabbithub:rs(Resource)]},
                  {querystr, [io_lib:format("~p", [ParsedQuery])]}]},
    error_logger:warning_report({perform_request, unknown, Xml}),
    rabbithub:respond_xml(Req, 200, [], none, Xml).

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
