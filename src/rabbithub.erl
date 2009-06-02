-module(rabbithub).
-export([start/0, stop/0]).
-export([rabbit_node/0, rabbit_call/3]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
start() ->
    rabbithub_deps:ensure(),
    ensure_started(crypto),
    application:start(rabbithub).

stop() ->
    Res = application:stop(rabbithub),
    application:stop(crypto),
    Res.

rabbit_node() ->
    {ok, N} = application:get_env(rabbitmq_node),
    N.

rabbit_call(M, F, A) ->
    case rpc:call(rabbit_node(), M, F, A) of
        {badrpc, {'EXIT', Reason}} ->
            exit(Reason);
        V ->
            V
    end.
