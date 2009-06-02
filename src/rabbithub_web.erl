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
    case Req:get(path) of
        "/c/" ++ Capability ->
            handle_cap_request(Capability, Req);
        "/x/" ++ ExchangeName ->
            handle_exchange_request(ExchangeName, Req);
        "/q/" ++ QueueName ->
            handle_queue_request(QueueName, Req);
        "/u/" ->
            rabbithub_user:handle_req(Req);
        "/u" ->
            rabbithub_user:handle_req(Req);
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

handle_hub_post(Req) ->
    Req:respond({200, [], "You posted!"}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
