-module(rabbithub).

-export([start/0, stop/0]).
-export([rabbit_node/0, rabbit_call/3, r/2, rs/1]).
-export([respond_xml/5]).

-include_lib("xmerl/include/xmerl.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
start() ->
    rabbithub_deps:ensure(),
    ensure_started(crypto),
    application:start(rabbithub).

stop() ->
    Res = application:stop(rabbithub),
    application:stop(crypto),
    Res.

rabbit_node() ->
    {ok, N} = application:get_env(rabbitmq_node),
    N.

rabbit_call(M, F, A) ->
    case rpc:call(rabbit_node(), M, F, A) of
        {badrpc, {'EXIT', Reason}} ->
            exit(Reason);
        V ->
            V
    end.

r(ResourceType, ResourceName) when is_list(ResourceName) ->
    r(ResourceType, list_to_binary(ResourceName));
r(ResourceType, ResourceName) ->
    rabbit_call(rabbit_misc, r, [<<"/">>, ResourceType, ResourceName]).

rs(Resource) ->
    rabbit_call(rabbit_misc, rs, [Resource]).

respond_xml(Req, StatusCode, Headers, StylesheetRelUrlOrNone, XmlElement) ->
    Req:respond({StatusCode,
                 [{"Content-type", "text/xml"}] ++ Headers,
                 "<?xml version=\"1.0\"?>" ++
                   stylesheet_pi(StylesheetRelUrlOrNone) ++
                   xmerl:export_simple([XmlElement],
                                       xmerl_xml,
                                       [#xmlAttribute{name=prolog, value=""}])}).

stylesheet_pi(none) ->
    [];
stylesheet_pi(RelUrl) ->
    ["<?xml-stylesheet href=\"", RelUrl, "\" type=\"text/xsl\" ?>"].
