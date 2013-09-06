-module(rabbithub_subscription).

-export([start_subscriptions/0]).
-export([create/2, delete/1]).
-export([start_link/1]).
-export([register_subscription_pid/3, erase_subscription_pid/1]).

%% Internal export
-export([expire/1]).

-include("rabbithub.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% Should be exported by timer module, but isn't
system_time() ->
    {MegaSec, Sec, MicroSec} = now(),
    1000000 * (MegaSec * 1000000 + Sec) + MicroSec.

start_subscriptions() ->
    {atomic, Leases} =
        mnesia:transaction(fun () ->
                                   mnesia:foldl(fun (Lease, Acc) -> [Lease | Acc] end,
                                                [],
                                                rabbithub_lease)
                           end),
    lists:foreach(fun start/1, Leases).

create(Subscription, LeaseSeconds) ->
    RequestedExpiryTime = system_time() + LeaseSeconds * 1000000,
    Lease = #rabbithub_lease{subscription = Subscription,
                             lease_expiry_time_microsec = RequestedExpiryTime},
    {atomic, ok} = mnesia:transaction(fun () -> ok = mnesia:write(Lease) end),
    start(Lease).

delete(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(fun () -> mnesia:delete({rabbithub_lease, Subscription}) end),
    {atomic, SubPids} =
        mnesia:transaction(
          fun () ->
                  SubPids = mnesia:read({rabbithub_subscription_pid, Subscription}),
                  ok = mnesia:delete({rabbithub_subscription_pid, Subscription}),
                  SubPids
          end),
    lists:foreach(fun (#rabbithub_subscription_pid{pid = Pid,
                                                   expiry_timer = TRef}) ->
                          {ok, cancel} = timer:cancel(TRef),
                          gen_server:cast(Pid, shutdown)
                  end, SubPids),
    ok.

expire(Subscription) ->
    rabbit_log:info("RabbitHub expiring subscription~n~p~n", [Subscription]),
    delete(Subscription).

start_link(Lease =
           #rabbithub_lease{subscription =
                            #rabbithub_subscription{resource =
                                                    #resource{kind = ResourceTypeAtom}}}) ->
    case ResourceTypeAtom of
        exchange ->
            gen_server:start_link(rabbithub_pseudo_queue, [Lease], []);
        queue ->
            gen_server:start_link(rabbithub_consumer, [Lease], [])
    end.

start(Lease) ->
    case supervisor:start_child(rabbithub_subscription_sup, [Lease]) of
        {ok, _Pid} ->
            ok;
        {error, normal} ->
            %% duplicate processes return normal, so as to not provoke the error logger.
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

register_subscription_pid(Lease, Pid, ProcessModule) ->
    Result = register_subscription_pid1(Lease, Pid),
    rabbit_log:info("RabbitHub register subscription (startup); ~p~n~p~n~p~n", [Result, ProcessModule, Lease]),
    Result.

register_subscription_pid1(#rabbithub_lease{subscription = Subscription,
                                            lease_expiry_time_microsec = ExpiryTimeMicro},
                           Pid) ->
    NowMicro = system_time(),
    case NowMicro > ExpiryTimeMicro of
        true ->
            %% Expired.
            ok = delete(Subscription),
            expired;
        false ->
            %% Not *yet* expired. Always start a timer, since even if
            %% it's a duplicate we want to cancel the existing timer
            %% and create a new timer to fire at the new time.
            {ok, TRef} = timer:apply_after((ExpiryTimeMicro - NowMicro) div 1000,
                                           ?MODULE, expire, [Subscription]),
            NewPidRecord = #rabbithub_subscription_pid{subscription = Subscription,
                                                       pid = Pid,
                                                       expiry_timer = TRef},
            {atomic, Result} =
                mnesia:transaction(
                  fun () ->
                          case mnesia:read(rabbithub_subscription_pid, Subscription) of
                              [] ->
                                  ok = mnesia:write(NewPidRecord);
                              [ExistingRecord =
                                 #rabbithub_subscription_pid{pid = ExistingPid,
                                                             expiry_timer = OldTRef}] ->
                                  case is_process_alive(ExistingPid) of
                                      true ->
                                          {ok, cancel} = timer:cancel(OldTRef),
                                          R1 = ExistingRecord#rabbithub_subscription_pid{
                                                 expiry_timer = TRef},
                                          ok = mnesia:write(R1),
                                          duplicate;
                                      false ->
                                          ok = mnesia:write(NewPidRecord)
                                  end
                          end
                  end),
            Result
    end.

erase_subscription_pid(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(fun () -> mnesia:delete({rabbithub_subscription_pid, Subscription}) end),
    ok.
