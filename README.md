twister.el
==========

An interface to the twister microblogging application from Emacs


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
