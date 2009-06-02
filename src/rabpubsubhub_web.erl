%% @doc Web server for rabpubsubhub.

-module(rabpubsubhub_web).

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
    case Req:get(path) of
        "/c/" ++ Capability ->
            handle_cap_request(Capability, Req);
        "/x/" ++ ExchangeName ->
            handle_exchange_request(ExchangeName, Req);
        "/q/" ++ QueueName ->
            handle_queue_request(QueueName, Req);
        "/u/" ->
            handle_user_request(Req);
        "/u" ->
            handle_user_request(Req);
        "/" ++ StaticFile ->
            case Req:get(method) of
                Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                    Req:serve_file(StaticFile, DocRoot);
                'POST' ->
                    case StaticFile of
                        "" ->
                            handle_hub_post(Req);
                        _ ->
                            Req:not_found()
                    end;
                _ ->
                    Req:respond({501, [], []})
            end;
        _ ->
            Req:not_found()
    end.

%% Internal API

handle_cap_request(Capability, Req) ->
    Req:respond({200, [], "You accessed " ++ Capability}).

handle_exchange_request(ExchangeName, Req) ->
    Req:respond({200, [], "You accessed " ++ ExchangeName}).

handle_queue_request(QueueName, Req) ->
    Req:respond({200, [], "You accessed " ++ QueueName}).

handle_user_request(Req) ->
    check_auth(Req,
               fun (Username) ->
                       Req:respond({200, [], "Congratulations, " ++ Username ++ "!"})
               end).

check_auth(Req, Fun) ->
    case Req:get_header_value("authorization") of
        undefined ->
            request_auth(Req);
        "Basic " ++ AuthInfo ->
            case check_auth_info(AuthInfo) of
                {ok, Username} ->
                    Fun(Username);
                {error, _Reason} ->
                    request_auth(Req)
            end
    end.

request_auth(Req) ->
    Req:respond({401, [{"WWW-Authenticate", "Basic realm=\"rabbitmq\""}],
                 "Authentication required."}).

check_auth_info(AuthInfo) ->
    {User, Pass} = case string:tokens(base64:decode_to_string(AuthInfo), ":") of
                       [U, P] -> {U, P};
                       [U] -> {U, ""}
                   end,
    case catch rabpubsubhub:rabbit_call(rabbit_access_control, user_pass_login,
                                        [list_to_binary(User),
                                         list_to_binary(Pass)]) of
        {'EXIT', {amqp, access_refused, _, _}} ->
            {error, access_refused};
        _ ->
            {ok, User}
    end.

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
