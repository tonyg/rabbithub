-module(rabpubsubhub).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
start() ->
    rabpubsubhub_deps:ensure(),
    ensure_started(crypto),
    application:start(rabpubsubhub).

stop() ->
    Res = application:stop(rabpubsubhub),
    application:stop(crypto),
    Res.
