%% This file is part of erlibnotify.
%%
%% erlibnotify is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% erlibnotify is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with erlibnotify.  If not, see <http://www.gnu.org/licenses/>.

-module(erlibnotify).

-export([ init/1,
	  uninit/0,
	  is_initted/0,
	  %% get_app_name/0,
	  %% get_server_caps/0,
	  %% get_server_info/4,
	  notification_new/1,
	  notification_new/2,
	  notification_new/3,
	  notification_update/2,
	  notification_update/3,
	  notification_update/4,
	  notification_show/1,
	  notification_set_timeout/2,
	  %% notification_set_category/2,
	  notification_set_urgency/2
	  %% notification_set_hint/3
	]).

-on_load(init_nif/0).

init_nif() ->
  ok = erlang:load_nif(filename:absname(code:priv_dir(?MODULE) ++ "/erlibnotify_nif"), 0).

init(_AppName) ->
  exit(nif_library_not_loaded).

uninit() ->
  exit(nif_library_not_loaded).

is_initted() ->
  exit(nif_library_not_loaded).

notification_new(Summary) ->
  notification_new(Summary, "", "").

notification_new(Summary, Body) ->
  notification_new(Summary, Body, "").

notification_new(_Summary, _Body, _Icon) ->
  exit(nif_library_not_loaded).

notification_update(Notification, Summary) ->
  notification_update(Notification, Summary, "", "").

notification_update(Notification, Summary, Body) ->
  notification_update(Notification, Summary, Body).

notification_update(_Notification, _Summary, _Body, _Icon) ->
  exit(nif_library_not_loaded).

notification_show(_Notification) ->
  exit(nif_library_not_loaded).

notification_set_timeout(_Notification, _Timeout) ->
  exit(nif_library_not_loaded).

notification_set_urgency(_Notification, _Urgency) ->
  exit(nif_library_not_loaded).
