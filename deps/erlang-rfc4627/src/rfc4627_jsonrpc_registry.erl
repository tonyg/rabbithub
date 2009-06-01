%% JSON-RPC service registry
%%---------------------------------------------------------------------------
%% @author Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% @author LShift Ltd. <query@lshift.net>
%% @copyright 2007, 2008 Tony Garnock-Jones and LShift Ltd.
%% @license
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%% @since 1.2.0

%% @private
%% @doc JSON-RPC service registry implementation.
%%
%% Started and managed by functions defined in module {@link rfc4627_jsonrpc}.

-module(rfc4627_jsonrpc_registry).
-include("rfc4627_jsonrpc.hrl").

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%% @doc gen_server behaviour callback.
init(_Args) ->
    {ok, no_jsonrpc_state}.

%% @doc gen_server behaviour callback.
terminate(_Reason, _State) ->
    %% FIXME: should we notify services here?
    ok.

%% @doc gen_server behaviour callback.
code_change(_OldVsn, State, _Extra) ->
    State.

%% @doc gen_server behaviour callback.
handle_call({lookup_service, Service}, _From, State) ->
    case get({service, Service}) of
	undefined ->
	    {reply, not_found, State};
	ServiceRec ->
	    {reply, ServiceRec, State}
    end;

handle_call({register_service, Pid, ServiceDescription}, _From, State) ->
    SD = ServiceDescription#service{handler = {pid, Pid}},
    erlang:monitor(process, Pid),
    put({service_pid, Pid}, SD#service.name),
    put({service, SD#service.name}, SD),
    {reply, ok, State}.

%% @doc gen_server behaviour callback.
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    {noreply, State}.

%% @doc gen_server behaviour callback.
handle_info({'DOWN', _MonitorRef, process, DownPid, _Reason}, State) ->
    case get({service_pid, DownPid}) of
	undefined ->
	    %% How strange.
	    {noreply, State};
	ServiceName ->
	    erase({service_pid, DownPid}),
	    erase({service, ServiceName}),
	    {noreply, State}
    end.
