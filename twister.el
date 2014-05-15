;;; twister.el --- A client for the twister distribute microblogging system

;; Copyright (C) 2014 Marcel van der Boom <marcel@hsdev.com>

;;; Commentary:
;; The original idea for this client was to minimally implement:
;; - make it possible to post to twister directly from Emacs;
;; - have autocompletion for 'known users' when posting a message
;;   (this probably means defining a 'mode'

;; During the implementation of the above the following nice-to-have
;; came to mind:
;; - get a timeline in a buffer in emacs
;; - allow for replies
;; - allow for direct messages
;; - manage follow lists
;; - have avatars obviously. (including the checkmark, which is brilliant)
;;

;; Requirements
;; - json-rpc : https://github.com/mrvdb/elisp-json-rpc
;;   This repository is included as submodule

;; Installation
;; (add-to-list 'load-path "/path/to/twister.el")
;; (require 'twister)
;; (setq twister-user "yournick")
;;
;;; Credits
;;  - The elisp-json-rpc library of Christopher Wellons <wellons@nullprogrma.com>
;;    does the real work

;;; Code:
(require 'json-rpc)

;; Configuration variables
(defgroup twister nil
  "Using the twister microblogging system"
  :tag "Microblogging"
  :group 'applications
  )

(defcustom twister-user "twister_user"
  "The nickname you use on your twister instance. When posting messages, this will be the name used"
  :type 'string
  :group 'twister)

(defcustom twister-rpcuser "user"
  "The RPC username configured in the twister.conf file"
  :type 'string
  :group 'twister)

(defcustom twister-rpcpassword "pwd"
  "The RPC password for the `twister-rpcuser configured in the twister.conf file"
  :type 'string
  :group 'twitter)

(defcustom twister-host "localhost"
  "Host where the twister daemon runs"
  :type 'string
  :group 'twister)

(defcustom twister-port 28332
  "Port on which twister daemon runs and serves RPC commands"
  :type 'integer
  :group 'twister)


;; Get a connection to the twister daemon
(setf twisterd (json-rpc-connect
                twister-host twister-port
                twister-rpcuser twister-rpcpassword))

;; Preliminary convention
;; tw-* methods      -> sorta private for now, don't use directly
;; twister-* methods -> public API

(defun tw-get-last-post(user)
  "Get the last post of a user"
  (let (obj (json-new-object))
    (json-rpc twisterd "getposts" 1
              (vector (json-add-to-object obj "username" user)))))

(defun tw-get-next-k(user)
  "Get the next 'k' for a user; this is a post sequence"
  ;; Data structure visible here: http://twister.net.co/?page_id=21
  (+ 1 (plist-get
   (plist-get
    (elt (tw-get-last-post user) 0)
    :userpost) :k)))

(defun twister-post(msg)
  "Post msg to the configured twister daemon"
  (interactive)

  (json-rpc
   twisterd "newpostmsg"
   twister-user
   (tw-get-next-k twister-user) msg))


(defun twister-post-region (begin end)
  "Post the current region to twister"
  (interactive "r")

  (twister-post
   (buffer-substring-no-properties begin end)))

(defun twister-post-buffer()
  "Post the current buffer to twister"
  (interactive)

  (twister-post-region
   (point-min point-max)))


(provide 'twister)
;;; twister.el ends here
