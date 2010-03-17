{application, rabbithub,
 [{description, "rabbithub"},
  {vsn, "0.0.1"},
  {modules, [
    rabbithub,
    rabbithub_app,
    rabbithub_sup,
    rabbithub_web,
    rabbithub_deps
  ]},
  {registered, []},
  {mod, {rabbithub_app, []}},
  {env, [{default_username, "guest"}]},
  {applications, [kernel, stdlib, crypto]}]}.
