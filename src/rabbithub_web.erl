-module(rabbithub_web).

-export([start/0, handle_req/1, listener/0]).

-include("rabbithub.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-define(APPLICATION_XSLT, ("/" ++ rabbithub:canonical_basepath() ++ "/static/application.xsl.xml")).

-define(DEFAULT_SUBSCRIPTION_LEASE_SECONDS, (30 * 86400)).
-define(SUBSCRIPTION_LEASE_LIMIT, 1000 * 365 * 86400). %% Around a thousand years

start() ->
    Listener = listener(),
    rabbit_web_dispatch:register_context_handler(rabbithub:canonical_basepath(), Listener, "",
                                             fun (Req) -> ?MODULE:handle_req(Req) end,
                                             "RabbitHub").

listener() ->
    {ok, Listener} = application:get_env(rabbithub, listener),
    Listener.

split_path("", _) ->
    [<<>>];
split_path(Path, 1) ->
    [list_to_binary(Path)];
split_path(Path, N) ->
    case string:str(Path, "/") of
        0 ->
            [list_to_binary(Path)];
        Pos ->
            [list_to_binary(string:substr(Path, 1, Pos - 1))
             | split_path(string:substr(Path, Pos + 1), N - 1)]
    end.

%% handle_req(AliasPrefix, Req) ->
handle_req(Req) ->
    {FullPath, Query, _Fragment} = mochiweb_util:urlsplit_path(Req:get(raw_path)),
    %% plus one for the "/", plus one for the 1-based indexing for substr:
    Path = FullPath,
    ParsedQuery = mochiweb_util:parse_qs(Query),
    %% When we get to drop support for R12B-3, we can start using
    %% re:split(Path, "/", [{parts, 4}]) again.

    %% case split_path(Path, 4) of
    %% BRC
    case split_path(Path, 5) of
        [<<>>, <<"static">> | _] ->
            handle_static(Path, Req);
        [<<>>, <<>>] ->
            handle_static(Path, Req);
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
        %% BRC
        [<<>>, VHost, Facet, ResourceType, Name] ->
            case check_resource_type(ResourceType) of
                {ok, ResourceTypeAtom} ->
                    handle_request(ResourceTypeAtom,
                                   binary_to_list(Facet),
                                   rabbithub:r(VHost, ResourceTypeAtom, binary_to_list(Name)),
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

handle_static("/" ++ StaticFile, Req) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            {file, Here} = code:is_loaded(?MODULE),
            ModuleRoot = filename:dirname(filename:dirname(Here)),
            DocRoot = filename:join(ModuleRoot, "priv/www"),
            Req:serve_file(StaticFile, DocRoot);
        'POST' ->
            case StaticFile of
                "" -> handle_hub_post(Req);
                _ -> Req:not_found()
            end;
        _ ->
            Req:respond({501, [], "Invalid HTTP method"})
    end;
handle_static(_OtherPath, Req) ->
    Req:respond({400, [], "Invalid path"}).

param(ParsedQuery, Key, DefaultValue) ->
    case lists:keysearch(Key, 1, ParsedQuery) of
        {value, {_Key, Value}} ->
            Value;
        false ->
            DefaultValue
    end.

params(ParsedQuery, Key) ->
    [V || {K, V} <- ParsedQuery, K =:= Key].

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
    HubModeAttr = case HubMode of
                      undefined -> [];
                      _ -> [{'hub.mode', HubMode}]
                  end,
    {action, HubModeAttr ++ [{'http.method', HttpMethod}, {name, Name}],
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
        desc_action(undefined, "POST", "deliver",
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
        desc_action("subscribe", "GET", "verify_subscription",
                    "Ensure that an earlier-generated token is valid and intended for use as a subscription token.",
                    [desc_param("hub.challenge", "query", [],
                                "Token to echo to the caller."),
                     desc_param("hub.lease_seconds", "query", [],
                                "Number of seconds that the subscription will remain active before expiring."),
                     desc_param("hub.verify_token", "query", [],
                                "The token to validate.")]),
        desc_action("unsubscribe", "GET", "verify_unsubscription",
                    "Ensure that an earlier-generated token is valid and intended for use as an unsubscription token.",
                    [desc_param("hub.challenge", "query", [],
                                "Token to echo to the caller."),
                     desc_param("hub.lease_seconds", "query", [{optional, "true"}],
                                "Number of seconds that the subscription will remain active before expiring."),
                     desc_param("hub.verify_token", "query", [],
                                "The token to validate.")]),
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
                                "Subscriber-provided opaque token. See the PubSubHubBub spec."),
                     desc_param("hub.lease_seconds", "query", [{optional, "true"}],
                                "Subscriber-provided lease duration request, in seconds. See the PubSubHubBub spec.")]),
        desc_action("unsubscribe", "POST", "unsubscribe",
                    "Unsubscribe from the application.",
                    [desc_param("hub.callback", "query", [],
                                "The URL that was used to subscribe."),
                     desc_param("hub.topic", "query", [],
                                "The filter that was used to subscribe."),
                     desc_param("hub.verify", "query", [],
                                "Either 'sync' or 'async'; the subscription verification mode for this request. See the PubSubHubBub spec."),
                     desc_param("hub.verify_token", "query", [{optional, "true"}],
                                "Subscriber-provided opaque token. See the PubSubHubBub spec."),
                     desc_param("hub.lease_seconds", "query", [{optional, "true"}],
                                "Subscriber-provided lease duration request, in seconds. See the PubSubHubBub spec.")])]).

declare_queue(Resource = #resource{kind = queue, name = QueueNameBin}, _ParsedQuery, Req) ->
    check_auth(Req, Resource, [configure],
               fun (_Username) ->
                       case rabbit_amqqueue:lookup(Resource) of
                           {ok, _} ->
                               Req:respond({204, [], []});
                           {error, not_found} ->
                               rabbit_amqqueue:declare(Resource, true, false, [], none),
                               QN = binary_to_list(QueueNameBin),
                               Req:respond({201,
                                            [{"Location",
                                              canonical_url(Req, "/endpoint/q/" ++ QN)}],
                                            []})
                       end
               end).

resource_exists(R) ->
    ResourceMod = case R#resource.kind of
                      exchange -> rabbit_exchange;
                      queue -> rabbit_amqqueue
                  end,
    case ResourceMod:lookup(R) of
        {ok, _} ->
            true;
        {error, not_found} ->
            false
    end.

generate_and_send_token(Req, Resource, IntendedUse, ExtraData) ->
    case resource_exists(Resource) of
        true ->
            SignedTerm = rabbithub:sign_term({Resource, IntendedUse, ExtraData}),
            Req:respond({200,
                         [{"Content-type", "application/x-www-form-urlencoded"}],
                         ["hub.verify_token=", rabbithub:b64enc(SignedTerm)]});
        false ->
            Req:not_found()
    end,
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
    Challenge = param(ParsedQuery, "hub.challenge", ""), %% strictly speaking, mandatory
    case decode_and_verify_token(EncodedParam) of
        {error, _Reason} ->
            Req:respond({400, [], "Bad hub.verify_token"});
        {ok, {IntendedResource, IntendedUse, _ExtraData}} ->
            if
                IntendedUse =:= ActualUse andalso IntendedResource =:= ActualResource ->
                    Req:respond({200, [], Challenge});
                true ->
                    Req:respond({400, [], "Intended use or resource does not match actual use or resource."})
            end
    end.

can_shortcut(#resource{kind = exchange}, #resource{kind = queue}) ->
    true;
can_shortcut(_, _) ->
    false.

do_validate(Callback, Topic, LeaseSeconds, ActualUse, VerifyToken) ->
    Challenge = list_to_binary(rabbithub:b64enc(rabbit_guid:binary(rabbit_guid:gen(), "c"))),
    Params0 = [{'hub.mode', ActualUse},
               {'hub.topic', Topic},
               {'hub.challenge', Challenge},
               {'hub.lease_seconds', LeaseSeconds}],
    Params = case VerifyToken of
                 none -> Params0;
                 _ -> [{'hub.verify_token', VerifyToken} | Params0]
             end,
    case simple_httpc:req("GET", Callback, mochiweb_util:urlencode(Params), [], []) of
        {ok, StatusCode, _StatusText, _Headers, Body}
          when StatusCode >= 200 andalso StatusCode < 300 ->
            if
                Body =:= Challenge ->
                    ok;
                true ->
                    {error, challenge_mismatch}
            end;
        {ok, StatusCode, _StatusText, _Headers, _Body} ->
            {error, {request_status, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

invoke_sub_fun_and_respond(Req, Fun, Callback, Topic, LeaseSeconds, MaybeShortcut) ->
    case Fun(Callback, Topic, LeaseSeconds, MaybeShortcut) of
        ok ->
            Req:respond({204, [], []});
        {error, {status, StatusCode}} ->
            Req:respond({StatusCode, [], []})
    end.

first_acceptable(_Predicate, []) ->
    {error, none_acceptable};
first_acceptable(Predicate, [Candidate | Rest]) ->
    case Predicate(Candidate) of
        true ->
            ok;
        false ->
            first_acceptable(Predicate, Rest)
    end.

extract_verify_modes(ParsedQuery, ValueIfMissing) ->
    case lists:concat([string:tokens(V, ",") || V <- params(ParsedQuery, "hub.verify")]) of
        [] -> ValueIfMissing;
        Modes -> Modes
    end.

extract_lease_seconds(ParsedQuery) ->
    case catch list_to_integer(param(ParsedQuery, "hub.lease_seconds", "")) of
        {'EXIT', _Reason} ->
            ?DEFAULT_SUBSCRIPTION_LEASE_SECONDS;
        InvalidValue when InvalidValue =< 0 ->
            ?DEFAULT_SUBSCRIPTION_LEASE_SECONDS;
        InvalidValue when InvalidValue >= ?SUBSCRIPTION_LEASE_LIMIT ->
            ?SUBSCRIPTION_LEASE_LIMIT;
        Value ->
            Value
    end.

validate_subscription_request(Req, ParsedQuery, SourceResource, ActualUse, Fun) ->
    Callback = param(ParsedQuery, "hub.callback", missing),
    Topic = param(ParsedQuery, "hub.topic", missing),
    VerifyModes = extract_verify_modes(ParsedQuery, missing),
    VerifyToken = param(ParsedQuery, "hub.verify_token", none),
    LeaseSeconds = extract_lease_seconds(ParsedQuery),
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
                                    invoke_sub_fun_and_respond(Req, Fun,
                                                               Callback, Topic, LeaseSeconds,
                                                               TargetResource);
                                false ->
                                    invoke_sub_fun_and_respond(Req, Fun,
                                                               Callback, Topic, LeaseSeconds,
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
                    case first_acceptable(
                           fun ("async") ->
                                   Req:respond({202, [], []}),
                                   spawn(fun () ->
                                                 case do_validate(Callback, Topic, LeaseSeconds,
                                                                  ActualUse, VerifyToken) of
                                                     ok ->
                                                         Fun(Callback, Topic, LeaseSeconds,
                                                             no_shortcut);
                                                     {error, _} ->
                                                         ignore
                                                 end
                                         end),
                                   true;
                               ("sync") ->
                                   case do_validate(Callback, Topic, LeaseSeconds,
                                                    ActualUse, VerifyToken) of
                                       ok ->
                                           invoke_sub_fun_and_respond(Req, Fun,
                                                                      Callback, Topic, LeaseSeconds,
                                                                      no_shortcut);
                                       {error, Reason} ->
                                           Req:respond
                                             ({400, [],
                                               io_lib:format("Request verification failed: ~p",
                                                             [Reason])})
                                   end,
                                   true;
                               (_) ->
                                   false
                           end, VerifyModes) of
                        ok ->
                            ok;
                        {error, none_acceptable} ->
                            Req:respond({400, [], "No supported hub.verify modes listed"})
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
    rabbit_basic:message(ExchangeResource,
                         list_to_binary(RoutingKey),
                         [{'content_type', ContentTypeBin}],
                         Body).

perform_request('POST', endpoint, '', exchange, Resource, ParsedQuery, Req) ->
    Msg = extract_message(Resource, ParsedQuery, Req),
    Delivery = rabbit_basic:delivery(false, Msg, none),
    case rabbit_basic:publish(Delivery) of
        {ok, _, _} ->
            Req:respond({202, [], []});
        {error, not_found} ->
            Req:not_found()
    end;

perform_request('POST', endpoint, '', queue, Resource, ParsedQuery, Req) ->
    Msg = extract_message(rabbithub:r(exchange, ""), ParsedQuery, Req),
    Delivery = rabbit_basic:delivery(false, Msg, none),
    case rabbit_amqqueue:lookup([Resource]) of
        [Queue] ->
            {routed, _} = rabbit_amqqueue:deliver([Queue], Delivery),
            Req:respond({202, [], []});
        [] ->
            Req:not_found()
    end;

perform_request('PUT', endpoint, '', exchange, Resource, ParsedQuery, Req) ->
    ExchangeTypeBin = list_to_binary(param(ParsedQuery, "amqp.exchange_type", "fanout")),
    case catch rabbit_exchange:check_type(ExchangeTypeBin) of
        {'EXIT', _} ->
            Req:respond({400, [], "Invalid exchange type"});
        ExchangeType ->
            case rabbit_exchange:lookup(Resource) of
                {ok, _} ->
                    Req:respond({204, [], []});
                {error, not_found} ->
                    rabbit_exchange:declare(Resource, ExchangeType, true, false, false, []),
                    Req:respond({201, [{"Location", self_url(Req)}], []})
            end
    end;

perform_request('PUT', endpoint, '', queue, UncheckedResource, ParsedQuery, Req) ->
    case UncheckedResource of
        #resource{kind = queue, name = <<>>} ->
            V = rabbithub:r(queue, rabbit_guid:binary(rabbit_guid:gen(), "amq.gen.http")),
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
            case rabbit_exchange:delete(Resource, false) of
                {error, not_found} ->
                    Req:not_found();
                ok ->
                    Req:respond({204, [] ,[]})
            end
    end;

perform_request('DELETE', endpoint, '', queue, Resource, _ParsedQuery, Req) ->
    case rabbit_amqqueue:lookup(Resource) of
        {error, not_found} ->
            Req:not_found();
        {ok, Q} ->
            {ok, _PurgedMessageCount} = rabbit_amqqueue:delete(Q, false, false),
            Req:respond({204, [] ,[]})
    end;

perform_request('GET', Facet, '', exchange, Resource, _ParsedQuery, Req) ->
    case rabbit_exchange:lookup(Resource) of
        {error, not_found} ->
            Req:not_found();
        {ok, #exchange{name = #resource{kind = exchange, name = XNameBin},
                       type = Type}} ->
            XN = binary_to_list(XNameBin),
            Xml = application_descriptor(XN,
                                         "AMQP " ++ rabbit_misc:rs(Resource),
                                         "amqp.exchange",
                                         [{"amqp.exchange_type", atom_to_list(Type)}],
                                         [case Facet of
                                              endpoint -> endpoint_facet();
                                              subscribe -> subscribe_facet()
                                          end]),
            rabbithub:respond_xml(Req, 200, [], ?APPLICATION_XSLT, Xml)
    end;

perform_request('GET', Facet, '', queue, Resource, _ParsedQuery, Req) ->
    case rabbit_amqqueue:lookup(Resource) of
        {error, not_found} ->
            Req:not_found();
        {ok, #amqqueue{name = #resource{kind = queue, name = QNameBin}}} ->
            QN = binary_to_list(QNameBin),
            Xml = application_descriptor(QN,
                                         "AMQP " ++ rabbit_misc:rs(Resource),
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
                                  fun (Callback, Topic, LeaseSeconds, no_shortcut) ->
                                          Sub = #rabbithub_subscription{resource = Resource,
                                                                        topic = Topic,
                                                                        callback = Callback},
                                          case rabbithub_subscription:create(Sub, LeaseSeconds) of
                                              ok -> ok;
                                              {error, not_found} -> {error, {status, 404}};
                                              {error, _} -> {error, {status, 500}}
                                          end;
                                      (_Callback, Topic, _LeaseSeconds, TargetResource) ->
                                          case rabbit_binding:add(
                                                 #binding{source      = Resource,
                                                          destination = TargetResource,
                                                          key         = list_to_binary(Topic),
                                                          args        = []}) of
                                              ok ->
                                                  ok;
                                              {error, _} ->
                                                  {error, {status, 404}}
                                          end
                                  end);

perform_request('POST', subscribe, unsubscribe, _ResourceTypeAtom, Resource, ParsedQuery, Req) ->
    validate_subscription_request(Req, ParsedQuery, Resource, unsubscribe,
                                  fun (Callback, Topic, _LeaseSeconds, no_shortcut) ->
                                          Sub = #rabbithub_subscription{resource = Resource,
                                                                        topic = Topic,
                                                                        callback = Callback},
                                          ok = rabbithub_subscription:delete(Sub),
                                          ok;
                                      (_Callback, Topic, _LeaseSeconds, TargetResource) ->
                                          case rabbit_binding:remove(
                                                 #binding{source      = Resource,
                                                          destination = TargetResource,
                                                          key         = list_to_binary(Topic),
                                                          args        = []}) of
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
                                {resource, [rabbit_misc:rs(Resource)]},
                                {querystr, [io_lib:format("~p", [ParsedQuery])]}]},
    error_logger:error_report({perform_request, unknown, Xml}),
    rabbithub:respond_xml(Req, 200, [], none, Xml).

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).
