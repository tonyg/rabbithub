-module(rabbithub_auth).

-export([check_authentication/2, check_authorization/5]).

check_authentication(Req, Fun) ->
    case Req:get_header_value("authorization") of
        undefined ->
            case rabbithub:default_username() of
                undefined ->
                    request_auth(Req);
                Username ->
                    Fun(Username)
            end;
        "Basic " ++ AuthInfo ->
            case check_auth_info(AuthInfo) of
                {ok, Username} ->
                    Fun(Username);
                {error, _Reason} ->
                    forbidden(Req)
            end
    end.

check_authorization(Req, Resource, Username, PermissionsRequired, Fun) ->
    CheckResults = [catch rabbit_access_control:check_resource_access(
                            list_to_binary(Username), Resource, P)
                    || P <- PermissionsRequired],
    case lists:foldl(fun check_authorization_result/2, ok, CheckResults) of
        ok ->
            Fun();
        failed ->
            forbidden(Req)
    end.

check_authorization_result({'EXIT', _}, ok) ->
    failed;
check_authorization_result(ok, ok) ->
    ok;
check_authorization_result(_, failed) ->
    failed.

forbidden(Req) ->
    Req:respond({403, [], "Forbidden"}).

request_auth(Req) ->
    Req:respond({401, [{"WWW-Authenticate", "Basic realm=\"rabbitmq\""}],
                 "Authentication required."}).

check_auth_info(AuthInfo) ->
    {User, Pass} = case string:tokens(base64:decode_to_string(AuthInfo), ":") of
                       [U, P] -> {U, P};
                       [U] -> {U, ""}
                   end,
    case catch rabbit_access_control:user_pass_login(list_to_binary(User),
                                                     list_to_binary(Pass)) of
        {'EXIT', {amqp, access_refused, _, _}} ->
            {error, access_refused};
        _ ->
            {ok, User}
    end.
