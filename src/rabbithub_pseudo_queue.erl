-module(rabbithub_pseudo_queue).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include("rabbithub.hrl").

-record(state, {subscription, queue_name}).

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

%% Added for 3.2.1 as rabbit_amqqueue:pseudo_queue/2 does not correctly initialise
%% all fields (in particular, for 3.2.1 we need to correctly initialise "decorators").
pseudo_queue(QueueName, Pid) ->
    #amqqueue{name         = QueueName,
              durable      = false,
              auto_delete  = false,
              arguments    = [],
              pid          = Pid,
              slave_pids   = [],
              decorators   = []}.

really_init(Subscription = #rabbithub_subscription{resource = Resource,
                                                   topic = Topic}) ->
    QueueName = rabbithub:r(queue, rabbit_guid:binary(rabbit_guid:gen(), "amq.http.pseudoqueue")),
    Q = pseudo_queue(QueueName, self()),
    Q = rabbit_amqqueue:internal_declare(Q, false),
   case rabbit_binding:add(#binding{source      = Resource,
                                    destination = QueueName,
                                    key         = list_to_binary(Topic),
                                    args        = []}) of
        ok ->
            {ok, #state{subscription = Subscription, queue_name = QueueName}};
        {error, exchange_not_found} ->
            {stop, not_found}
    end.


deliver(BasicMessage, Subscription, State) ->
    case rabbithub:deliver_via_post(Subscription, BasicMessage, []) of
        {ok, _} ->
            ok;
        {error, Reason, Content} ->
            ok = rabbithub:error_and_unsub(Subscription,
                                           {rabbithub_pseudo_queue, http_post_failure, Reason, Content})
    end,
    {noreply, State}.

%% handle_call(Request, _From, State) ->
%%    {stop, {unhandled_call, Request}, State}.
%%

handle_call({deliver, _Delivery = #delivery{message = BasicMessage}, _Flow}, _From,
            State = #state{subscription = Subscription}) ->
    deliver(BasicMessage, Subscription, State).

handle_cast({deliver, _Delivery = #delivery{message = BasicMessage}, _MS, _Flow},
            State = #state{subscription = Subscription}) ->
     deliver(BasicMessage, Subscription, State);

handle_cast({deliver, _Delivery = #delivery{message = BasicMessage}, _Flow},
            State = #state{subscription = Subscription}) ->
    deliver(BasicMessage, Subscription, State);

handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Request, State) ->
    {stop, {unhandled_info, Request}, State}.

terminate(_Reason, _State = #state{subscription = Subscription,
                                   queue_name = QueueName}) ->
    rabbit_log:info("RabbitHub stopping pseudo queue, ~p~n~p~n", [_Reason, _State]),
    ok = rabbithub_subscription:erase_subscription_pid(Subscription),
%%    rabbit_amqqueue:internal_delete(QueueName, self()),
    rabbit_amqqueue:internal_delete(QueueName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
