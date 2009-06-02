-module(rabpubsubhub_app).
-behaviour(application).

-include("rabpubsubhub.hrl").

-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    rabpubsubhub_deps:ensure(),
    ok = contact_rabbitmq(),
    ok = setup_schema(),
    rabpubsubhub_sup:start_link().

stop(_State) ->
    ok.

contact_rabbitmq() ->
    RabbitNode = case application:get_env(rabbitmq_node) of
                     undefined ->
                         [_NodeName, NodeHost] = string:tokens(atom_to_list(node()), "@"),
                         A = list_to_atom("rabbit@" ++ NodeHost),
                         application:set_env(rabpubsubhub, rabbitmq_node, A),
                         A;
                     {ok, A} ->
                         A
                 end,
    {contacting_rabbitmq, pong} = {contacting_rabbitmq, net_adm:ping(RabbitNode)},
    ok.

setup_schema() ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end,
    ok = mnesia:start(),
    ok = create_table(rabpubsubhub_subscription,
                      [{attributes, record_info(fields, rabpubsubhub_subscription)},
                       {disc_copies, [node()]}]),
    ok.

create_table(Name, Params) ->
    case mnesia:create_table(Name, Params) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Name}} ->
            ok
    end.
