{application, rabpubsubhub,
 [{description, "rabpubsubhub"},
  {vsn, "0.0.1"},
  {modules, [
    rabpubsubhub,
    rabpubsubhub_app,
    rabpubsubhub_sup,
    rabpubsubhub_web,
    rabpubsubhub_deps
  ]},
  {registered, []},
  {mod, {rabpubsubhub_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
