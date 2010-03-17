-module(rabbithub_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [{rabbithub_subscription_sup,
                                   {rabbithub_subscription_sup, start_link, []},
                                   permanent, 5000, supervisor, [rabbithub_subscription_sup]}]}}.
