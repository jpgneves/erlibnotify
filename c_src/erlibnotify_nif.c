/*
    This file is part of erlibnotify.

    erlibnotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    erlibnotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with erlibnotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <libnotify/notify.h>
#include "erl_nif.h"

#define MAX_APP_NAME_LEN 256
#define MAX_SUMMARY_LEN 128
#define MAX_BODY_LEN 512
#define MAX_ICON_LEN 512

#define ATOM_OK "ok"

static ErlNifResourceType* ErlNotificationType;

typedef struct _ErlNotification {
  NotifyNotification* notification;
} ErlNotification;

ERL_NIF_TERM gboolean_to_atom(ErlNifEnv* env, gboolean bool) {
  const char* val = bool == TRUE ? "true":"false";
  return enif_make_atom(env, val);
}

static int load(ErlNifEnv * env, void** priv_data, ERL_NIF_TERM load_info){
  ErlNotificationType = enif_open_resource_type(env,
						NULL,
						"erl_notification",
						NULL,
						ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
						NULL);
  if(ErlNotificationType == NULL) {
    return -1;
  } else {
    return 0;
  }
}

static ERL_NIF_TERM init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char* app_name = (char*)calloc(MAX_APP_NAME_LEN, sizeof(char));

  ERL_NIF_TERM ret;

  if(!enif_get_string(env, argv[0], app_name, MAX_APP_NAME_LEN, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  ret = gboolean_to_atom(env, notify_init(app_name));
  free(app_name);
  return ret;
}

static ERL_NIF_TERM uninit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  notify_uninit();
  return enif_make_atom(env, ATOM_OK);
}

static ERL_NIF_TERM is_initted_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  return gboolean_to_atom(env, notify_is_initted());
}

static ERL_NIF_TERM notification_new_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char* summary = (char*)calloc(MAX_SUMMARY_LEN, sizeof(char));
  char* body = (char*)calloc(MAX_BODY_LEN, sizeof(char));
  char* icon = (char*)calloc(MAX_ICON_LEN, sizeof(char));

  NotifyNotification* notification;
  ErlNotification* e_notification = enif_alloc_resource(ErlNotificationType, sizeof(ErlNotification));

  ERL_NIF_TERM ret;

  if(!enif_get_string(env, argv[0], summary, MAX_SUMMARY_LEN, ERL_NIF_LATIN1) ||
     !enif_get_string(env, argv[1], body, MAX_BODY_LEN, ERL_NIF_LATIN1) ||
     !enif_get_string(env, argv[2], icon, MAX_ICON_LEN, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  notification = notify_notification_new(summary, body, icon);
  e_notification->notification = notification;

  ret = enif_make_resource(env, e_notification);
  enif_release_resource(e_notification);

  return ret;
}

static ERL_NIF_TERM notification_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNotification* e_notification = NULL;
  char* summary = (char*)calloc(MAX_SUMMARY_LEN, sizeof(char));
  char* body = (char*)calloc(MAX_BODY_LEN, sizeof(char));
  char* icon = (char*)calloc(MAX_ICON_LEN, sizeof(char));

  if(!enif_get_resource(env, argv[0], ErlNotificationType, (void**)&e_notification) ||
     !enif_get_string(env, argv[1], summary, MAX_SUMMARY_LEN, ERL_NIF_LATIN1) ||
     !enif_get_string(env, argv[2], body, MAX_BODY_LEN, ERL_NIF_LATIN1) ||
     !enif_get_string(env, argv[3], icon, MAX_ICON_LEN, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  return gboolean_to_atom(env, notify_notification_update(e_notification->notification,
							  summary,
							  body,
							  icon));
}

static ERL_NIF_TERM notification_show_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNotification* e_notification = NULL;
  GError* error = NULL;

  ERL_NIF_TERM ret;

  if(!enif_get_resource(env, argv[0], ErlNotificationType, (void**)&e_notification)) {
    return enif_make_badarg(env);
  }

  if(!notify_notification_show(e_notification->notification, &error)) {
    ret = enif_make_tuple2(env, enif_make_atom(env, "error"),
			   enif_make_string(env, error->message, ERL_NIF_LATIN1));
  } else {
    ret = gboolean_to_atom(env, TRUE);
  }

  return ret;
}

static ERL_NIF_TERM notification_set_timeout_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNotification* e_notification = NULL;
  int timeout;

  if(!enif_get_resource(env, argv[0], ErlNotificationType, (void**)&e_notification) ||
     !enif_get_int(env, argv[1], &timeout)) {
    return enif_make_badarg(env);
  }

  notify_notification_set_timeout(e_notification->notification, (gint)timeout);

  return enif_make_atom(env, ATOM_OK);
}

static ERL_NIF_TERM notification_set_urgency_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNotification* e_notification = NULL;
  int urgency;

  if(!enif_get_resource(env, argv[0], ErlNotificationType, (void**)&e_notification) ||
     !enif_get_int(env, argv[1], &urgency)) {
    return enif_make_badarg(env);
  }

  notify_notification_set_urgency(e_notification->notification, urgency);

  return enif_make_atom(env, ATOM_OK);
}

static ErlNifFunc nif_funcs[] = {
  {"init", 1, init_nif},
  {"uninit", 0, uninit_nif},
  {"is_initted", 0, is_initted_nif},
  {"notification_new", 3, notification_new_nif},
  {"notification_update", 4, notification_update_nif},
  {"notification_show", 1, notification_show_nif},
  {"notification_set_timeout", 2, notification_set_timeout_nif},
  {"notification_set_urgency", 2, notification_set_urgency_nif}
};

ERL_NIF_INIT(erlibnotify, nif_funcs, load, NULL, NULL, NULL);
