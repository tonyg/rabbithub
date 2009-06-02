-module(simple_httpc).

-export([req/6]).

req(Host, Port, Method, Path, Headers, Body) ->
    case gen_tcp:connect(Host, Port, [binary,
                                      {active, false},
                                      {packet, raw}]) of
        {ok, Sock} ->
            RequestBin = list_to_binary(format_request(Method, Path, Headers, Body)),
            ok = gen_tcp:send(Sock, RequestBin),
            error_logger:info_report({request, Host, Port, RequestBin}),
            %% ok = gen_tcp:shutdown(Sock, write),
            collect_response(Sock, {httpc_response, parse, [nolimit, true]});
        {error, Reason} ->
            {error, 503, io_lib:format("~p", [Reason])}
    end.

format_request(Method, Path, Headers, Body) ->
    [Method, " ", Path, " HTTP/1.0\r\n",
     [[K, ": ", V, "\r\n"] || {K, V} <- Headers], "\r\n",
     Body].

collect_response(Sock, {Mod, Fun, ArgList}) ->
    %% error_logger:info_report({collect_response, Mod, Fun, ArgList}),
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            case Mod:Fun([Data | ArgList]) of
                {ok, {HttpVersion, StatusCode, StatusText, Headers, Body}} ->
                    case collect_body(Sock,
                                      lists:keysearch("content-length", 1,
                                                      http_response:header_list(Headers)),
                                      Body) of
                        {ok, FullBody} ->
                            gen_tcp:close(Sock),
                            format_resp(HttpVersion, StatusCode, StatusText, Headers, FullBody);
                        {error, Reason} ->
                            {relay_collect_body_error, Reason}
                    end;
                {Mod1, Fun1, ArgList1} ->
                    collect_response(Sock, {Mod1, Fun1, ArgList1})
            end;
        {error, Reason} ->
            {relay_socket_error, Reason}
    end.

collect_body(Sock, {value, {_, LengthStr}}, Body) ->
    collect_body_length(Sock, list_to_integer(LengthStr), size(Body), [Body]);
collect_body(Sock, false, Body) ->
    collect_body_eof(Sock, [Body]).

collect_body_length(_Sock, Length, SoFar, Acc)
  when Length == SoFar ->
    {ok, lists:reverse(Acc)};
collect_body_length(Sock, Length, SoFar, Acc) ->
    case gen_tcp:recv(Sock, Length - SoFar) of
        {ok, Data} ->
            collect_body_length(Sock, Length, SoFar + size(Data), [Data | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

collect_body_eof(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            collect_body_eof(Sock, [Data | Acc]);
        {error, closed} ->
            {ok, lists:reverse(Acc)};
        {error, Reason} ->
            {error, Reason}
    end.

format_resp(_HttpVersion, StatusCode, StatusText, Headers, Body) ->
    {ok, StatusCode, StatusText, http_response:header_list(Headers), list_to_binary([Body])}.
