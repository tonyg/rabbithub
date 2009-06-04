-module(simple_httpc).

-export([req/5, req/6]).

split_netloc(S) ->
    case string:tokens(S, "@") of
        [UserPass, HostPort] ->
            {Host, Port} = split_hostport(HostPort),
            case string:tokens(UserPass, ":") of
                [User, Pass] ->
                    {User, Pass, Host, Port};
                [User] ->
                    {User, "", Host, Port}
            end;
        [HostPort] ->
            {Host, Port} = split_hostport(HostPort),
            {none, none, Host, Port}
    end.

split_hostport(S) ->
    case string:tokens(S, ":") of
        [Host, Port] ->
            {Host, list_to_integer(Port)};
        _ ->
            {S, 80}
    end.

req(Method, FullUrl, ExtraQuery, Headers, Body) ->
    case catch mochiweb_util:urlsplit(FullUrl) of
        {"http", NetLoc, Path, ExistingQuery, Fragment} ->
            {User, Pass, Host, Port} = split_netloc(NetLoc),
            NewQuery = case {ExistingQuery, ExtraQuery} of
                           {_, ""} -> ExistingQuery;
                           {"", _} -> ExtraQuery;
                           _ -> ExistingQuery ++ "&" ++ ExtraQuery
                       end,
            NewPath = mochiweb_util:urlunsplit_path({Path, NewQuery, Fragment}),
            NewHeaders = case User of
                             none -> Headers;
                             _ -> [{"Authorization",
                                    "Basic " ++ base64:encode_to_string(User ++ ":" ++ Pass)}
                                   | Headers]
                         end,
            req(Host, Port, Method, NewPath, NewHeaders, Body);
        _ ->
            {error, invalid_callback_url}
    end.

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
            {error, {503, io_lib:format("~p", [Reason])}}
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
