twister.el
==========

An interface from Emacs to the twister microblogging application.

The purpose of this package is to augment other twister clients. Emacs
is an editor and, even when using other clients for twister, people
may want to post their messages using their editor.

While a view of timelines and follow/unfollow logic may be supported
by this client in time, the focus is on making posting and replying as
enjoyable as possible from an Emacs perspective.

Dependencies
------------

This library uses https://github.com/skeeto/elisp-json-rpc to issue
json rpc commands to the twister daemon. Make sure it is reachable by
your emacs load-path.


Usage
-----

```el
(require 'twister)

;; Configure an exsting username
(setq twister-user "yournick")

;; Usage from code
(twister-post
  "I have installed twister.el, and it works! -- https://github.com/mrvdb/twister.el  #emacs #twister")


;; Interactive use
(twister-create-post)  ;; Creates a window in which to edit a twister post C-c C-c posts it

(twister-post-buffer)  ;; Posts the current buffer (with length check) to twister
(twister-post-region)  ;; Posts the current region (with length check) to twister
