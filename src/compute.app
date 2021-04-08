{application, compute,
 [{description, " compute"},
  {vsn, "1.0.0"},
  {modules, [compute_app,
             compute_sup,
	     compute]},
  {registered, [compute]},
  {applications, [kernel, stdlib]},
  {mod, {compute_app, []}}
 ]}.
