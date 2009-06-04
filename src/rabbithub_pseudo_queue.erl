-module(rabbithub_pseudo_queue).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rabbithub.hrl").

-record(state, {subscription, rabbit_monitor_ref, queue_name}).

init([Subscription]) ->
    error_logger:info_report({starting_pseudo_queue, Subscription}),

    process_flag(trap_exit, true),
    case rabbithub_subscription:register_subscription_pid(Subscription, self()) of
        ok ->
            really_init(Subscription);
        duplicate ->
            {stop, normal}
    end.

really_init(Subscription = #rabbithub_subscription{resource = Resource,
                                                   topic = Topic}) ->
    QueueName = rabbithub:r(queue, rabbithub:binstring_guid("amq.http.pseudoqueue")),
    Q = rabbithub:rabbit_call(rabbit_amqqueue, pseudo_queue,
                              [QueueName, self()]),
    Q = rabbithub:rabbit_call(rabbit_amqqueue, internal_declare,
                              [Q, false]),
    case rabbithub:rabbit_call(rabbit_exchange, add_binding,
                               [Resource, QueueName, list_to_binary(Topic), []]) of
        ok ->
            RabbitPid = rabbithub:rabbit_call(erlang, whereis, [rabbit_sup]),
            MonRef = erlang:monitor(process, RabbitPid),
            {ok, #state{subscription = Subscription,
                        rabbit_monitor_ref = MonRef,
                        queue_name = QueueName}};
        {error, exchange_not_found} ->
            {stop, exchange_not_found}
    end.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({deliver, _Txn = none, BasicMessage},
            State = #state{subscription = Subscription}) ->
    case rabbithub:deliver_via_post(Subscription, BasicMessage, []) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ok = rabbithub:error_and_unsub(Subscription,
                                           {rabbithub_pseudo_queue, http_post_failure, Reason})
    end,
    {noreply, State};
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Request, State) ->
    {stop, {unhandled_info, Request}, State}.

terminate(_Reason, _State = #state{subscription = Subscription,
                                   queue_name = QueueName}) ->
    error_logger:info_report({stopping_pseudo_queue, _Reason, _State}),
    ok = rabbithub_subscription:erase_subscription_pid(Subscription),
    rabbithub:rabbit_call(rabbit_amqqueue, internal_delete, [QueueName]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
