erlibnotify - Erlang NIF bindings to libnotify
===========

Currently notifications are implemented as mutable objects so all operations are destructive.
This will most likely not change as it would imply doing deep copies of many GLib objects manually, which would break whenever there is a change in their internal representation.

Features:

- It doesn't crash much. :)

Send bug reports, pull/feature requests, thanks or threats here: https://github.com/jpgneves