-module(rabbithub_consumer).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rabbithub.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-record(state, {subscription, q_monitor_ref, consumer_tag}).

init([Lease = #rabbithub_lease{subscription = Subscription}]) ->
    process_flag(trap_exit, true),
    case rabbithub_subscription:register_subscription_pid(Lease, self(), ?MODULE) of
        ok ->
            really_init(Subscription);
        expired ->
            {stop, normal};
        duplicate ->
            {stop, normal}
    end.

really_init(Subscription = #rabbithub_subscription{resource = Resource}) ->
    case rabbit_amqqueue:lookup(Resource) of
        {ok, Q = #amqqueue{pid = QPid}} ->
            ConsumerTag = rabbit_guid:binary(rabbit_guid:gen(), "amq.http.consumer"),
            MonRef = erlang:monitor(process, QPid),
            rabbit_amqqueue:basic_consume(Q, false, self(), undefined, false,
                                          ConsumerTag, false, none, undefined),
            {ok, #state{subscription = Subscription,
                        q_monitor_ref = MonRef,
                        consumer_tag = ConsumerTag}};
        {error, not_found} ->
            ok = rabbithub:error_and_unsub(Subscription,
                                           {rabbithub_consumer, queue_not_found, Subscription}),
            {stop, not_found}
    end.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({deliver, _ConsumerTag, AckRequired,
             {_QNameResource, QPid, MsgId, Redelivered, BasicMessage}},
            State = #state{subscription = Subscription}) ->
    case rabbithub:deliver_via_post(Subscription,
                                    BasicMessage,
                                    [{"X-AMQP-Redelivered", atom_to_list(Redelivered)}]) of
        {ok, _} ->
            ok = rabbit_amqqueue:notify_sent(QPid, self()),
            case AckRequired of
                true ->
                    ok = rabbit_amqqueue:ack(QPid, [MsgId], self());
                false ->
                    ok
            end;
        {error, Reason} ->
            ok = rabbithub:error_and_unsub(Subscription,
                                           {rabbithub_consumer, http_post_failure, Reason})
    end,
    {noreply, State};
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Request, State) ->
    {stop, {unhandled_info, Request}, State}.

terminate(_Reason, _State = #state{subscription = Subscription}) ->
    error_logger:info_report({stopping_consumer, _Reason, _State}),
    ok = rabbithub_subscription:erase_subscription_pid(Subscription),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
