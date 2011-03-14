mod_log_rest
============

mod_log_rest is a ejabberd module aimed at logging chat messages
to a REST service. mod_log_rest was originally based on mod_log_chat
from Process One.

Compilation and installation
----------------------------

You need to have Erlang installed as well as the ejabberd-dev module
(checkout it in the same directory than mod_log_chat is).

- Run
  erl -pa ../../ejabberd-dev/trunk/ebin -make
  in the trunk directory of mod_log_chat.

- Copy generated mod_log_rest.beam file from the ebin directory to the
  directory where your ejabberd .beam files are.

- Edit the "modules" section of your ejabberd.cfg configuration file to
  suit your needs (see conf/ejabberd.conf.sample for examples).

- Be sure that the directories where you want to create your log
  files exists and are writable by you ejabberd user.

- Restart ejabberd.
