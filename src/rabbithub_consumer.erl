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
            %% Note that prefetch count is set to 1. This will likely have some impact
            %% on performance; however this is an internal consumer and HTTP POST
            %% operations will invariably be the rate-limiting step. Setting prefetch
            %% count to 1 allows better control over HTTP error handling.
            rabbit_amqqueue:basic_consume(Q, false, self(), undefined, false, 1,
                                          ConsumerTag, false, [], undefined),
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
        {error, Reason, Content} ->
            case is_integer(Reason) of 
                true ->
                    %% If requeue_on_http_post_error is set to false then messages associated with
                    %% failed HTTP POSTs will be dropped or published to a dead letter exchange (if
                    %% one is associated with the subscription queue in question). Note 
                    %% that this ties in with setting the prefetch count to 1 (see above), which
                    %% ensures that at most 2 messages will be rejected per error before the
                    %% subscription gets deleted and the consumer processes is terminated. This
                    %% setting is primarily intended for debugging purposes. For example, bad data
                    %% might cause the receiving web application to break. By using this setting
                    %% in conjunction with a dead letter exchange (and queue) it is possible to 
                    %% capture the offending messages, rather than have them end up back on the 
                    %% subscription queue and getting stuck in some sort of error loop.
                    case application:get_env(rabbithub, requeue_on_http_post_error) of
                       {ok, false} ->
                           ok = rabbit_amqqueue:notify_sent(QPid, self()),
                           case AckRequired of
                               true ->
                                   ok = rabbit_amqqueue:reject(QPid, false, [MsgId], self());
                               false ->
                                   ok
                           end;
                       _ ->
                           ok
                    end;
                false ->
                    ok
            end,
            ok = rabbithub:error_and_unsub(Subscription,
                                           {rabbithub_consumer, http_post_failure, Reason, Content})
    end,
    {noreply, State};
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Request, State) ->
    {stop, {unhandled_info, Request}, State}.

terminate(_Reason, _State = #state{subscription = Subscription}) ->
    rabbit_log:info("RabbitHub stopping consumer, ~p~n~p~n", [_Reason, _State]),
    ok = rabbithub_subscription:erase_subscription_pid(Subscription),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
