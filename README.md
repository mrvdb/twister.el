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


;; Interactive use
(twister-create-post)  ;; Creates a window in which to edit a twister post C-c C-c posts it

(twister-post-buffer)  ;; Posts the current buffer (with length check) to twister
(twister-post-region)  ;; Posts the current region (with length check) to twister
