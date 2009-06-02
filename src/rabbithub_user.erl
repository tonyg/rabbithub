-module(rabbithub_user).

-export([handle_req/1]).

handle_req(Req) ->
    rabbithub_auth:check_auth(Req, fun (Username) -> handle(Req, Username) end).

handle(Req, Username) ->
    rabbithub:respond_xml(Req, 200, [], "/t.xsl.xml",
                          {root, [{content,
                                   [{username, [Username]},
                                    {greeting, ["Hello"]}]}]}).
