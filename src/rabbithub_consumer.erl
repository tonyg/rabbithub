-module(rabbithub_consumer).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rabbithub.hrl").
-include("rabbit.hrl").

-record(state, {subscription, q_monitor_ref, consumer_tag}).

init([Subscription]) ->
    error_logger:info_report({starting_consumer, Subscription}),

    process_flag(trap_exit, true),
    case rabbithub_subscription:register_subscription_pid(Subscription, self()) of
        ok ->
            really_init(Subscription);
        duplicate ->
            {stop, normal}
    end.

really_init(Subscription = #rabbithub_subscription{resource = Resource}) ->
    case rabbithub:rabbit_call(rabbit_amqqueue, lookup, [Resource]) of
        {ok, Q = #amqqueue{pid = QPid}} ->
            ConsumerTag = rabbithub:binstring_guid("amq.http.consumer"),
            MonRef = erlang:monitor(process, QPid),
            rabbithub:rabbit_call(rabbit_amqqueue, basic_consume,
                                  [Q, false, self(), self(), undefined,
                                   ConsumerTag, false, undefined]),
            {ok, #state{subscription = Subscription,
                        q_monitor_ref = MonRef,
                        consumer_tag = ConsumerTag}};
        {error, not_found} ->
            {stop, queue_not_found}
    end.

error_and_delete_self(ErrorReport, State = #state{subscription = Subscription}) ->
    error_logger:error_report(ErrorReport),
    rabbithub_subscription:delete(Subscription),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({deliver, _ConsumerTag, AckRequired,
             {#resource{name = _QNameBin}, QPid, MsgId, Redelivered,
              #basic_message{exchange_name = #resource{name = _ExchangeNameBin},
                             routing_key = RoutingKeyBin,
                             content = #content{payload_fragments_rev = PayloadRev}}}},
            State = #state{subscription = #rabbithub_subscription{topic = TopicStr,
                                                                  callback = CallbackStr}}) ->
    ExtraQuery =
        lists:flatten(io_lib:format("hub.topic=~s", [TopicStr])),
    %% FIXME: get content properties out in some clean way
    PayloadBin = list_to_binary(lists:reverse(PayloadRev)),
    case simple_httpc:req("POST",
                          CallbackStr,
                          ExtraQuery,
                          [{"Content-length", integer_to_list(size(PayloadBin))},
                           {"X-AMQP-Routing-Key", RoutingKeyBin},
                           {"X-AMQP-Redelivered", atom_to_list(Redelivered)}],
                          PayloadBin) of
        {ok, StatusCode, _StatusText, _Headers, _Body} ->
            if
                StatusCode >= 200 andalso StatusCode < 300 ->
                    ok = rabbithub:rabbit_call(rabbit_amqqueue, notify_sent, [QPid, self()]),
                    case AckRequired of
                        true ->
                            ok = rabbithub:rabbit_call(rabbit_amqqueue, ack,
                                                       [QPid, none, [MsgId], self()]);
                        false ->
                            ok
                    end,
                    {noreply, State};
                true ->
                    error_and_delete_self({rabbithub_consumer,
                                           http_post_unexpected_status,
                                           StatusCode}, State)
            end;
        {error, Reason} ->
            error_and_delete_self({rabbithub_consumer,
                                   http_post_failure,
                                   Reason}, State)
    end;
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
