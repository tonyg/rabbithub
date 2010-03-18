-module(rabbithub_subscription).

-export([start_subscriptions/0]).
-export([create/4, delete/1]).
-export([start_link/1]).
-export([attempt_refresh/2]).
-export([register_subscription_pid/3, erase_subscription_pid/1]).

%% Internal export
-export([expire/1]).

-include("rabbithub.hrl").
-include("rabbit.hrl").

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

create(Subscription, LeaseSeconds, Secret, Token) ->
    RequestedExpiryTime = system_time() + LeaseSeconds * 1000000,
    Lease = #rabbithub_lease{subscription = Subscription,
                             lease_duration_seconds = LeaseSeconds,
                             lease_expiry_time_microsec = RequestedExpiryTime,
                             secret = Secret,
                             token = Token},
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

attempt_refresh(Lease = #rabbithub_lease{
                  subscription = Subscription = #rabbithub_subscription{
                                   callback = Callback,
                                   topic = Topic},
                  token = VerifyToken,
                  lease_duration_seconds = LeaseSeconds
                 }, Pid) ->
    error_logger:info_report({attempt_refresh, Subscription}),
    case rabbithub:do_validate(Callback, Topic, LeaseSeconds,
                                   subscribe, VerifyToken) of
        ok              ->
            NowMicro = system_time(),
            NextExpiry = NowMicro + (LeaseSeconds * 1000000),
            start_timer_and_store(Lease#rabbithub_lease{
                                    lease_expiry_time_microsec = NextExpiry},
                                  NowMicro, Pid),
            ok;
        {error, _Reason} -> expire(Subscription)
    end.

expire(Subscription) ->
    error_logger:info_report({expiring, Subscription}),
    ok = delete(Subscription),
    expired.

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

start_timer_and_store(Lease = #rabbithub_lease{
                        subscription = Subscription,
                        lease_expiry_time_microsec = Expiry},
                      NowMicro, Pid) ->
    %% Always start a timer, since even if
    %% it's a duplicate we want to cancel the existing timer
    %% and create a new timer to fire at the new time.
    {ok, TRef} = timer:apply_after((Expiry - NowMicro) div 1000,
                                   ?MODULE, attempt_refresh, [Lease, Pid]),
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
    Result.

%% This is a callback for the processes started in start_link above.
register_subscription_pid(Lease, Pid, ProcessModule) ->
    Result = register_subscription_pid1(Lease, Pid),
    error_logger:info_report({startup, Result, ProcessModule, Lease}),
    Result.

register_subscription_pid1(Lease =
                           #rabbithub_lease{
                             lease_expiry_time_microsec = ExpiryTimeMicro},
                           Pid) ->
    NowMicro = system_time(),
    case NowMicro > ExpiryTimeMicro of
        true ->
            %% possibly we are recovering and a lease has expired
            attempt_refresh(Lease, Pid);
        false ->
            %% Not *yet* expired.
            start_timer_and_store(Lease, NowMicro, Pid)
    end.

erase_subscription_pid(Subscription) ->
    {atomic, ok} =
        mnesia:transaction(fun () -> mnesia:delete({rabbithub_subscription_pid, Subscription}) end),
    ok.
