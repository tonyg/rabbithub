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

%%     receive
%% 	{unavailable, JID, RKBin, AllResources} ->
%% 	    {atomic, NewState} =
%% 		mnesia:transaction(
%% 		  fun () ->
%% 			  NewPriorities =
%% 			      case AllResources of
%% 				  true ->
%% 				      [E || E = {_, {J, _}} <- Priorities,
%% 					    not jids_equal_upto_resource(J, JID)];
%% 				  false ->
%% 				      lists:keydelete({JID, RKBin}, 2, Priorities)
%% 			      end,
%% 			  case NewPriorities of
%% 			      [] ->
%% 				  mnesia:delete({rabbitmq_consumer_process,
%% 						 State#consumer_state.queue}),
%% 				  terminate;
%% 			      _ ->
%% 				  State#consumer_state{priorities = NewPriorities}
%% 			  end
%% 		  end),
%% 	    case NewState of
%% 		terminate ->
%% 		    ?INFO_MSG("**** terminating consumer~n~p", [State#consumer_state.queue]),
%% 		    consumer_done(State#consumer_state{priorities = []}),
%% 		    done;
%% 		_ ->
%% 		    ?MODULE:consumer_main(NewState)
%% 	    end;
%% 	{presence, JID, RKBin, Priority} ->
%% 	    NewPriorities = lists:keysort(1, keystore({JID, RKBin}, 2, Priorities,
%% 						      {-Priority, {JID, RKBin}})),
%% 	    ?MODULE:consumer_main(State#consumer_state{priorities = NewPriorities});
%% 	{'$gen_cast', {deliver, _ConsumerTag, false, {_QName, QPid, _Id, _Redelivered, Msg}}} ->
%% 	    #basic_message{exchange_name = #resource{name = XNameBin},
%% 			   routing_key = RKBin,
%% 			   content = #content{payload_fragments_rev = PayloadRev}} = Msg,
%% 	    [{_, {TopPriorityJID, _}} | _] = Priorities,
%% 	    send_message(jlib:make_jid(binary_to_list(XNameBin),
%% 				       State#consumer_state.lserver,
%% 				       binary_to_list(RKBin)),
%% 			 TopPriorityJID,
%% 			 "chat",
%% 			 binary_to_list(list_to_binary(lists:reverse(PayloadRev)))),
%% 	    rabbit_amqqueue:notify_sent(QPid, self()),
%% 	    ?MODULE:consumer_main(State);
%% 	Other ->
%% 	    ?INFO_MSG("Consumer main ~p got~n~p", [State#consumer_state.queue, Other]),
%% 	    ?MODULE:consumer_main(State)
%%     end
