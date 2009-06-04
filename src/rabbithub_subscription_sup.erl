-module(rabbithub_subscription_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{rabbithub_subscription, {rabbithub_subscription, start_link, []},
            transient, brutal_kill, worker, [rabbithub_subscription]}]}}.
