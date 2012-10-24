-module(example).

-export([ run/0 ]).

run() ->
  erlibnotify:init("example_app"),
  N = erlibnotify:notification_new("Example notification", "Sample libnotify notification", ""),
  erlibnotify:notification_show(N),
  erlibnotify:uninit().
