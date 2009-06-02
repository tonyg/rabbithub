-module(rabbithub_auth).

-export([check_auth/2]).

check_auth(Req, Fun) ->
    case Req:get_header_value("authorization") of
        undefined ->
            request_auth(Req);
        "Basic " ++ AuthInfo ->
            case check_auth_info(AuthInfo) of
                {ok, Username} ->
                    Fun(Username);
                {error, _Reason} ->
                    request_auth(Req)
            end
    end.

request_auth(Req) ->
    Req:respond({401, [{"WWW-Authenticate", "Basic realm=\"rabbitmq\""}],
                 "Authentication required."}).

check_auth_info(AuthInfo) ->
    {User, Pass} = case string:tokens(base64:decode_to_string(AuthInfo), ":") of
                       [U, P] -> {U, P};
                       [U] -> {U, ""}
                   end,
    case catch rabbithub:rabbit_call(rabbit_access_control, user_pass_login,
                                     [list_to_binary(User),
                                      list_to_binary(Pass)]) of
        {'EXIT', {amqp, access_refused, _, _}} ->
            {error, access_refused};
        _ ->
            {ok, User}
    end.
