-module(rabbithub_app).
-behaviour(application).

-include("rabbithub.hrl").

-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    ok = contact_rabbitmq(),
    ok = setup_schema(),
    rabbithub_sup:start_link().

stop(_State) ->
    ok.

contact_rabbitmq() ->
    RabbitNode = case application:get_env(rabbitmq_node) of
                     undefined ->
                         [_NodeName, NodeHost] = string:tokens(atom_to_list(node()), "@"),
                         A = list_to_atom("rabbit@" ++ NodeHost),
                         application:set_env(rabbithub, rabbitmq_node, A),
                         A;
                     {ok, A} ->
                         A
                 end,
    {contacting_rabbitmq, RabbitNode, pong} =
        {contacting_rabbitmq, RabbitNode, net_adm:ping(RabbitNode)},
    error_logger:info_report({contacted_rabbitmq, RabbitNode}),
    ok.

setup_schema() ->
    mnesia:stop(),
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end,
    ok = mnesia:start(),
    ok = create_table(rabbithub_lease,
                      [{attributes, record_info(fields, rabbithub_lease)},
                       {disc_copies, [node()]}]),
    ok = create_table(rabbithub_subscription_pid,
                      [{attributes, record_info(fields, rabbithub_subscription_pid)}]),
    ok = mnesia:wait_for_tables([rabbithub_lease,
                                 rabbithub_subscription_pid],
                                5000),
    ok.

create_table(Name, Params) ->
    case mnesia:create_table(Name, Params) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Name}} ->
            ok
    end.
