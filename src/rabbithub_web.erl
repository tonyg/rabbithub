%% @doc Web server for rabbithub.

-module(rabbithub_web).

-export([start/1, stop/0, loop/2]).

-include("rabbithub.hrl").
-include("rabbit.hrl").

-define(APPLICATION_XSLT, (rabbithub:canonical_basepath() ++ "static/application.xsl.xml")).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    ok = rabbithub_subscription:start_subscriptions(),
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
                                   rabbithub:r(ResourceTypeAtom, binary_to_list(Name)),
                                   ParsedQuery,
                                   Req);
                {error, invalid_resource_type} ->
                    Req:not_found()
            end;
        _ ->
            Req:not_found()
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
check_facet('GET', "subscribe", "", _) -> auth_not_required;
check_facet('POST', "subscribe", "", _) -> auth_not_required; %% special case: see implementation
check_facet('POST', "subscribe", "subscribe", _) -> {auth_required, [read]};
check_facet('POST', "subscribe", "unsubscribe", _) -> {auth_required, []};
check_facet(_Method, _Facet, _HubMode, _ResourceType) -> invalid_operation.

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

handle_request(ResourceTypeAtom, Facet, Resource, ParsedQuery, Req) ->
    HubMode = param(ParsedQuery, "hub.mode", ""),
    Method = Req:get(method),
    case check_facet(Method, Facet, HubMode, ResourceTypeAtom) of
        {auth_required, PermissionsRequired} ->
            check_auth(Req, Resource, PermissionsRequired,
                       fun (_Username) ->
                               perform_request(Method,
                                               list_to_atom(Facet),
                                               list_to_atom(HubMode),
                                               ResourceTypeAtom,
                                               Resource,
                                               ParsedQuery,
                                               Req)
                       end);
        auth_not_required ->
            perform_request(Method,
                            list_to_atom(Facet),
                            list_to_atom(HubMode),
                            ResourceTypeAtom,
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

self_url(Req) ->
    canonical_url(Req, Req:get(path)).

canonical_url(Req, Path) ->
    mochiweb_util:urlunsplit({rabbithub:canonical_scheme(),
                              request_host(Req),
                              Path,
                              "",
                              ""}).

application_descriptor(Name, Description, Class, Parameters, Facets) ->
    {application, [{name, [Name]},
                   {description, [Description]},
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

facet_descriptor(Name, Description, Actions) ->
    {facet, [{name, Name}],
     [{description, [Description]},
      {actions, Actions}]}.

endpoint_facet() ->
    facet_descriptor
      ("endpoint",
       "Facet permitting delivery of pubsub messages into the application.",
       [desc_action("", "PUT", "create",
                    "Create an endpoint.",
                    [desc_param("amqp.exchange_type", "query", [{defaultvalue, "fanout"},
                                                                {optional, "true"}],
                                "(When creating an exchange) Specifies the AMQP exchange type.")]),
        desc_action("", "DELETE", "destroy",
                    "Destroy the endpoint.",
                    []),
        desc_action("", "POST", "deliver",
                    "Deliver a message to the endpoint.",
                    [desc_param("hub.topic", "query", [{defaultvalue, ""}],
                                "The routing key to use for the delivery."),
                     desc_param("content-type", "headers", [],
                                "The content-type of the body to deliver."),
                     desc_param("body", "body", [],
                                "The body of the HTTP request is used as the message to deliver.")]),
        desc_action("", "GET", "info",
                    "Retrieve a description of the application.",
                    []),
        desc_action("generate_token", "GET", "generate_token",
                    "Generate a verify_token for use in subscribing this application to (or unsubscribing this application from) some other application's message stream.",
                    [desc_param("hub.intended_use", "query", [],
                                "Either 'subscribe' or 'unsubscribe', depending on the intended use of the token."),
                     desc_param("rabbithub.data", "query", [{defaultvalue, ""}],
                                "Additional data to be checked during the verification stage.")])]).

subscribe_facet() ->
    facet_descriptor
      ("subscribe",
       "Facet permitting subscription to and unsubscription from pubsub messages generated by the application.",
       [desc_action("", "GET", "info",
                    "Retrieve a description of the application.",
                    []),
        desc_action("subscribe", "POST", "subscribe",
                    "Subscribe to pubsub messages from the application.",
                    [desc_param("hub.callback", "query", [],
                                "The URL to post each message to as it arrives."),
                     desc_param("hub.topic", "query", [],
                                "A filter for selecting a subset of messages. Each kind of hub interprets this parameter differently."),
                     desc_param("hub.verify", "query", [],
                                "Either 'sync' or 'async'; the subscription verification mode for this request. See the PubSubHubBub spec."),
                     desc_param("hub.verify_token", "query", [{optional, "true"}],
                                "Subscriber-provided opaque token. See the PubSubHubBub spec.")]),
        desc_action("unsubscribe", "POST", "unsubscribe",
                    "Unsubscribe from the application.",
                    [desc_param("hub.callback", "query", [],
                                "The URL that was used to subscribe."),
                     desc_param("hub.topic", "query", [],
                                "The filter that was used to subscribe."),
                     desc_param("hub.verify", "query", [],
                                "Either 'sync' or 'async'; the subscription verification mode for this request. See the PubSubHubBub spec."),
                     desc_param("hub.verify_token", "query", [{optional, "true"}],
                                "Subscriber-provided opaque token. See the PubSubHubBub spec.")])]).

declare_queue(Resource = #resource{kind = queue, name = QueueNameBin}, _ParsedQuery, Req) ->
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

generate_and_send_token(Req, Resource, IntendedUse, ExtraData) ->
    SignedTerm = rabbithub:sign_term({Resource, IntendedUse, ExtraData}),
    Req:respond({200,
                 [{"Content-type", "application/x-www-form-urlencoded"}],
                 ["hub.verify_token=", rabbithub:b64enc(SignedTerm)]}),
    ok.

decode_and_verify_token(EncodedParam) ->
    case catch decode_and_verify_token1(EncodedParam) of
        {'EXIT', _Reason} ->
            {error, crash};
        Other ->
            Other
    end.

decode_and_verify_token1(EncodedParam) ->
    SignedTerm = rabbithub:b64dec(EncodedParam),
    rabbithub:verify_term(SignedTerm).

check_token(Req, ActualResource, ActualUse, ParsedQuery) ->
    EncodedParam = param(ParsedQuery, "hub.verify_token", ""),
    case decode_and_verify_token(EncodedParam) of
        {error, _Reason} ->
            Req:respond({400, [], "Bad hub.verify_token"});
        {ok, {IntendedResource, IntendedUse, _ExtraData}} ->
            if
                IntendedUse =:= ActualUse andalso IntendedResource =:= ActualResource ->
                    Req:respond({204, [], []});
                true ->
                    Req:respond({400, [], "Intended use or resource does not match actual use or resource."})
            end
    end.

can_shortcut(#resource{kind = exchange}, #resource{kind = queue}) ->
    true;
can_shortcut(_, _) ->
    false.

do_validate(Callback, Topic, ActualUse, VerifyToken) ->
    QuerySuffix0 =
        lists:flatten(io_lib:format("hub.mode=~s&hub.topic=~s", [atom_to_list(ActualUse), Topic])),
    QuerySuffix =
        case VerifyToken of
            none -> QuerySuffix0;
            _ -> QuerySuffix0 ++ "&hub.verify_token=" ++ VerifyToken
        end,
    case simple_httpc:req("GET", Callback, QuerySuffix, [], []) of
        {ok, StatusCode, _StatusText, _Headers, _Body}
          when StatusCode >= 200 andalso StatusCode < 300 ->
            ok;
        {ok, StatusCode, _StatusText, _Headers, _Body} ->
            {error, {request_status, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

invoke_sub_fun_and_respond(Req, Fun, Callback, Topic, MaybeShortcut) ->
    case Fun(Callback, Topic, MaybeShortcut) of
        ok ->
            Req:respond({204, [], []});
        {error, {status, StatusCode}} ->
            Req:respond({StatusCode, [], []})
    end.

validate_subscription_request(Req, ParsedQuery, SourceResource, ActualUse, Fun) ->
    Callback = param(ParsedQuery, "hub.callback", missing),
    Topic = param(ParsedQuery, "hub.topic", missing),
    VerifyModes = string:tokens(param(ParsedQuery, "hub.verify", missing), ","),
    VerifyToken = param(ParsedQuery, "hub.verify_token", none),
    case lists:member(missing, [Callback, Topic, VerifyModes]) of
        true ->
            Req:respond({400, [], "Missing required parameter"});
        false ->
            case decode_and_verify_token(VerifyToken) of
                {ok, {TargetResource, IntendedUse, _ExtraData}} ->
                    %% OMG it's one of ours! It could be possible to
                    %% shortcut.
                    case IntendedUse of
                        ActualUse ->
                            case can_shortcut(SourceResource, TargetResource) of
                                true ->
                                    invoke_sub_fun_and_respond(Req, Fun, Callback, Topic,
                                                               TargetResource);
                                false ->
                                    invoke_sub_fun_and_respond(Req, Fun, Callback, Topic,
                                                               no_shortcut)
                            end;
                        _ ->
                            Req:respond({400, [], "Shortcut token has wrong hub.mode"})
                    end;
                {error, _} ->
                    %% Either it's not ours, or it's corrupted in some
                    %% way. No short-cuts are possible. Treat it as a
                    %% regular subscription request and invoke the
                    %% verification callback.

                    %% FIXME: honour subscriber's preferred order of VerifyModes!
                    case lists:member("async", VerifyModes) of
                        true ->
                            Req:respond({202, [], []}),
                            spawn(fun () ->
                                          case do_validate(Callback, Topic,
                                                           ActualUse, VerifyToken) of
                                              ok ->
                                                  Fun(Callback, Topic, no_shortcut);
                                              {error, _} ->
                                                  ignore
                                          end
                                  end);
                        false ->
                            case lists:member("sync", VerifyModes) of
                                true ->
                                    case do_validate(Callback, Topic,
                                                     ActualUse, VerifyToken) of
                                        ok ->
                                            invoke_sub_fun_and_respond(Req, Fun, Callback, Topic,
                                                                       no_shortcut);
                                        {error, Reason} ->
                                            Req:respond
                                              ({400, [],
                                                io_lib:format("Request verification failed: ~p",
                                                              [Reason])})
                                    end;
                                false ->
                                    Req:respond({400, [], "No supported hub.verify modes listed"})
                            end
                    end
            end
    end.

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
        {ok, #amqqueue{pid = QPid}} ->
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
                    Req:respond({201, [{"Location", self_url(Req)}], []})
            end
    end;

perform_request('PUT', endpoint, '', queue, UncheckedResource, ParsedQuery, Req) ->
    case UncheckedResource of
        #resource{kind = queue, name = <<>>} ->
            V = rabbithub:r(queue, rabbithub:binstring_guid("amq.gen.http")),
            declare_queue(V, ParsedQuery, Req);
        %% FIXME should use rabbit_channel:check_name/2, but that's not exported:
        #resource{kind = queue, name = <<"amq.", _/binary>>} ->
            Req:respond({400, [], "Invalid queue name"});
        V ->
            declare_queue(V, ParsedQuery, Req)
    end;

perform_request('DELETE', endpoint, '', exchange, Resource, _ParsedQuery, Req) ->
    case Resource of
        #resource{kind = exchange, name = <<>>} ->
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

perform_request('GET', Facet, '', exchange, Resource, _ParsedQuery, Req) ->
    case rabbithub:rabbit_call(rabbit_exchange, lookup, [Resource]) of
        {error, not_found} ->
            Req:not_found();
        {ok, #exchange{name = #resource{kind = exchange, name = XNameBin},
                       type = Type}} ->
            XN = binary_to_list(XNameBin),
            Xml = application_descriptor(XN,
                                         "AMQP " ++ rabbithub:rs(Resource),
                                         "amqp.exchange",
                                         [{"amqp.exchange_type", atom_to_list(Type)}],
                                         [case Facet of
                                              endpoint -> endpoint_facet();
                                              subscribe -> subscribe_facet()
                                          end]),
            rabbithub:respond_xml(Req, 200, [], ?APPLICATION_XSLT, Xml)
    end;

perform_request('GET', Facet, '', queue, Resource, _ParsedQuery, Req) ->
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        {error, not_found} ->
            Req:not_found();
        {ok, #amqqueue{name = #resource{kind = queue, name = QNameBin}}} ->
            QN = binary_to_list(QNameBin),
            Xml = application_descriptor(QN,
                                         "AMQP " ++ rabbithub:rs(Resource),
                                         "amqp.queue",
                                         [],
                                         [case Facet of
                                              endpoint -> endpoint_facet();
                                              subscribe -> subscribe_facet()
                                          end]),
            rabbithub:respond_xml(Req, 200, [], ?APPLICATION_XSLT, Xml)
    end;

perform_request('GET', endpoint, generate_token, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    ExtraData = param(ParsedQuery, "rabbithub.data", ""),
    case param(ParsedQuery, "hub.intended_use", undefined) of
        "subscribe" ->
            ok = generate_and_send_token(Req, Resource, subscribe, ExtraData);
        "unsubscribe" ->
            ok = generate_and_send_token(Req, Resource, unsubscribe, ExtraData);
        _ ->
            Req:respond({400, [], "Missing or bad hub.intended_use parameter"})
    end;

perform_request('GET', endpoint, subscribe, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    check_token(Req, Resource, subscribe, ParsedQuery);

perform_request('GET', endpoint, unsubscribe, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    check_token(Req, Resource, unsubscribe, ParsedQuery);

perform_request('POST', subscribe, '', ResourceTypeAtom, Resource, _ParsedQuery, Req) ->
    %% Pulls new query parameters out of the body, and loops around to
    %% handle_request again, which performs authentication and
    %% authorization checks as appropriate to the new query
    %% parameters.

    IsContentTypeOk = case Req:get_header_value("content-type") of
                          undefined -> true; %% permit people to omit it
                          "application/x-www-form-urlencoded" -> true;
                          _ -> false
                      end,
    if
        IsContentTypeOk ->
            BodyQuery = mochiweb_util:parse_qs(Req:recv_body()),
            handle_request(ResourceTypeAtom, "subscribe", Resource, BodyQuery, Req);
        true ->
            Req:respond({400, [], "Bad content-type; expected application/x-www-form-urlencoded"})
    end;

perform_request('POST', subscribe, subscribe, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    validate_subscription_request(Req, ParsedQuery, Resource, subscribe,
                                  fun (Callback, Topic, no_shortcut) ->
                                          Sub = #rabbithub_subscription{resource = Resource,
                                                                        topic = Topic,
                                                                        callback = Callback},
                                          _IgnoredResult = rabbithub_subscription:create(Sub),
                                          ok;
                                      (_Callback, Topic, TargetResource) ->
                                          case rabbithub:rabbit_call(rabbit_exchange,
                                                                     add_binding,
                                                                     [Resource, TargetResource,
                                                                      list_to_binary(Topic), []]) of
                                              ok ->
                                                  ok;
                                              {error, _} ->
                                                  {error, {status, 404}}
                                          end
                                  end);

perform_request('POST', subscribe, unsubscribe, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    validate_subscription_request(Req, ParsedQuery, Resource, unsubscribe,
                                  fun (Callback, Topic, no_shortcut) ->
                                          Sub = #rabbithub_subscription{resource = Resource,
                                                                        topic = Topic,
                                                                        callback = Callback},
                                          ok = rabbithub_subscription:delete(Sub),
                                          ok;
                                      (_Callback, Topic, TargetResource) ->
                                          case rabbithub:rabbit_call(rabbit_exchange,
                                                                     delete_binding,
                                                                     [Resource, TargetResource,
                                                                      list_to_binary(Topic), []]) of
                                              ok ->
                                                  ok;
                                              {error, _} ->
                                                  {error, {status, 404}}
                                          end
                                  end);

perform_request(Method, Facet, HubMode, _ResourceType, Resource, ParsedQuery, Req) ->
    Xml = {debug_request_echo, [{method, [atom_to_list(Method)]},
                                {facet, [atom_to_list(Facet)]},
                                {hubmode, [atom_to_list(HubMode)]},
                                {resource, [rabbithub:rs(Resource)]},
                                {querystr, [io_lib:format("~p", [ParsedQuery])]}]},
    error_logger:error_report({perform_request, unknown, Xml}),
    rabbithub:respond_xml(Req, 200, [], none, Xml).

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
