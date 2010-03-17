{application, rabbithub,
 [{description, "rabbithub"},
  {vsn, "0.0.1"},
  {modules, [
    rabbithub,
    rabbithub_app,
    rabbithub_auth,
    rabbithub_consumer,
    rabbithub_pseudo_queue,
    rabbihub_subscription,
    rabbihub_subscription_sup,
    rabbithub_sup,
    rabbithub_web,
    simple_httpc
  ]},
  {registered, []},
  {mod, {rabbithub_app, []}},
  {env, [{default_username, "guest"}]},
  {applications, [kernel, stdlib, crypto]}]}.
