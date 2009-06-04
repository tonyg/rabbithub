-module(rabbithub_subscription).

-export([start_subscriptions/0]).
-export([create/1, delete/1]).
-export([start_link/1, start/1]).
-export([register_subscription_pid/2, erase_subscription_pid/1]).

-include("rabbithub.hrl").
-include("rabbit.hrl").

start_subscriptions() ->
    {atomic, Subscriptions} =
        mnesia:transaction(fun () ->
                                   mnesia:foldl(fun (Sub, Acc) -> [Sub | Acc] end,
                                                [],
                                                rabbithub_subscription)
                           end),
    lists:foreach(fun start_subscription/1, Subscriptions).

start_subscription(Subscription) ->
    {ok, _Pid} = rabbithub_subscription:start(Subscription).

create(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(
          fun () ->
                  case mnesia:match_object(Subscription) of
                      [_] ->
                          %% Already exists.
                          ok;
                      [] ->
                          ok = mnesia:write(Subscription)
                  end
          end),
    start(Subscription).

delete(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(fun () -> mnesia:delete_object(Subscription) end),
    {atomic, Pids} =
        mnesia:transaction(
          fun () ->
                  Pids = mnesia:read({rabbithub_subscription_pid, Subscription}),
                  ok = mnesia:delete({rabbithub_subscription_pid, Subscription}),
                  Pids
          end),
    lists:foreach(fun (Pid) ->
                          gen_server:cast(Pid, shutdown)
                  end, Pids),
    ok.

start_link(Subscription =
           #rabbithub_subscription{resource = #resource{kind = ResourceTypeAtom}}) ->
    case ResourceTypeAtom of
        exchange ->
            gen_server:start_link(rabbithub_pseudo_queue, [Subscription], []);
        queue ->
            gen_server:start_link(rabbithub_consumer, [Subscription], [])
    end.

start(Subscription) ->
    supervisor:start_child(rabbithub_subscription_sup, [Subscription]).

register_subscription_pid(Subscription, Pid) ->
    {atomic, Result} =
        mnesia:transaction(
          fun () ->
                  case mnesia:read(rabbithub_subscription_pid, Subscription) of
                      [] ->
                          ok = mnesia:write({rabbithub_subscription_pid, Subscription, Pid});
                      [#rabbithub_subscription_pid{pid = ExistingPid}] ->
                          case is_process_alive(ExistingPid) of
                              true ->
                                  duplicate;
                              false ->
                                  ok = mnesia:write({rabbithub_subscription_pid, Subscription, Pid})
                          end
                  end
          end),
    Result.

erase_subscription_pid(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(fun () -> mnesia:delete({rabbithub_subscription_pid, Subscription}) end),
    ok.
