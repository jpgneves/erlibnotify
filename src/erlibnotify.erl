-module(erlibnotify).

-export([ init/1,
	  uninit/0,
	  is_initted/0,
	  %% get_app_name/0,
	  %% get_server_caps/0,
	  %% get_server_info/4,
	  notification_new/3,
	  %% notification_update/4,
	  notification_show/1,
	  notification_set_timeout/2
	  %% notification_set_category/2,
	  %% notification_set_urgency/2,
	  %% notification_set_hint/3
	]).

-on_load(init_nif/0).

init_nif() ->
  ok = erlang:load_nif(code:priv_dir(?MODULE) ++ "/erlibnotify_nif", 0).

init(_AppName) ->
  exit(nif_library_not_loaded).

uninit() ->
  exit(nif_library_not_loaded).

is_initted() ->
  exit(nif_library_not_loaded).

notification_new(_Summary, _Body, _Icon) ->
  exit(nif_library_not_loaded).

notification_show(_Notification) ->
  exit(nif_library_not_loaded).

notification_set_timeout(_Notification, _Timeout) ->
  exit(nif_library_not_loaded).
