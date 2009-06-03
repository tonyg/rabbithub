%% @doc Web server for rabbithub.

-module(rabbithub_web).

-export([start/1, stop/0, loop/2]).

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

check_facet('POST', "subscribe", "subscribe") -> {auth_required, [read]};
check_facet('POST', "subscribe", "unsubscribe") -> {auth_required, []};
check_facet('GET', "endpoint", "subscribe") -> auth_not_required;
check_facet('GET', "endpoint", "unsubscribe") -> auth_not_required;
check_facet('GET', "endpoint", "generate_token") -> {auth_required, [write]};
check_facet('GET', "endpoint", "") -> auth_not_required;
check_facet('POST', "endpoint", "") -> {auth_required, [write]};
check_facet('PUT', "endpoint", "") -> {auth_required, [configure]};
check_facet('DELETE', "endpoint", "") -> {auth_required, [configure]};
check_facet(Method, Facet, HubMode) ->
    error_logger:warning_report({check_facet, invalid_operation, {Method, Facet, HubMode}}),
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

handle_request(ResourceType, Facet, Name, ParsedQuery, Req) ->
    HubMode = param(ParsedQuery, "hub.mode", ""),
    Method = Req:get(method),
    Resource = rabbithub:r(ResourceType, Name),
    case check_facet(Method, Facet, HubMode) of
        {auth_required, PermissionsRequired} ->
            rabbithub_auth:check_authentication
              (Req, fun (Username) ->
                            rabbithub_auth:check_authorization
                              (Req, Resource, Username, PermissionsRequired,
                               fun () ->
                                       perform_request(Method,
                                                       list_to_atom(Facet),
                                                       list_to_atom(HubMode),
                                                       Username,
                                                       ResourceType,
                                                       Resource,
                                                       ParsedQuery,
                                                       Req)
                               end)
                    end);
        auth_not_required ->
            perform_request(Method,
                            list_to_atom(Facet),
                            list_to_atom(HubMode),
                            none,
                            ResourceType,
                            Resource,
                            ParsedQuery,
                            Req);
        invalid_operation ->
            Req:respond({400, [], "Invalid operation"})
    end.

extract_message(ExchangeResource, ParsedQuery, Req) ->
    RoutingKey = param(ParsedQuery, "hub.topic", ""),
    ContentTypeBin = case Req:get_header_value("content-type") of
                         undefined -> undefined;
                         S -> list_to_binary(S)
                     end,
    Body = Req:recv_body(),
    rabbithub:rabbit_call(rabbit_basic, message,
                          [ExchangeResource, list_to_binary(RoutingKey), ContentTypeBin, Body]).

perform_request('POST', endpoint, '', _Username, exchange, Resource, ParsedQuery, Req) ->
    Msg = extract_message(Resource, ParsedQuery, Req),
    case rabbithub:rabbit_call(rabbit_basic, publish, [false, false, none, Msg]) of
        {ok, _, _} ->
            Req:respond({202, [], []});
        {error, not_found} ->
            Req:respond({404, [], "No such exchange"})
    end;

perform_request('POST', endpoint, '', _Username, queue, Resource, ParsedQuery, Req) ->
    Msg = extract_message(rabbithub:r(exchange, ""), ParsedQuery, Req),
    IsMandatory = case param(ParsedQuery, "amqp.mandatory", "") of
                      "true" -> true;
                      _ -> false
                  end,
    error_logger:info_report({queue_post, Resource, Msg}),
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        %% TODO use the record or think of something else other than this ugly solution:
        {ok, {amqqueue, _Name, _Durable, _AutoDelete, _Arguments, QPid}} ->
            true = rabbithub:rabbit_call(rabbit_amqqueue, deliver,
                                         [IsMandatory, false, none, Msg, QPid]),
            Req:respond({case IsMandatory of true -> 204; false -> 202 end, [], []});
        {error, not_found} ->
            Req:respond({404, [], "No such queue"})
    end;

perform_request(Method, Facet, HubMode, Username, _ResourceType, Resource, ParsedQuery, Req) ->
    Xml = {root, [{method, [atom_to_list(Method)]},
                  {facet, [atom_to_list(Facet)]},
                  {hubmode, [atom_to_list(HubMode)]},
                  {username, [case Username of none -> ""; _ -> Username end]},
                  {resource, [rabbithub:rs(Resource)]},
                  {querystr, [io_lib:format("~p", [ParsedQuery])]}]},
    error_logger:warning_report({perform_request, unknown, Xml}),
    rabbithub:respond_xml(Req, 200, [], none, Xml).

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
