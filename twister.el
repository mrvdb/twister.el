;;; twister.el --- A client for the twister distribute microblogging system

;; Copyright (C) 2014 Marcel van der Boom <marcel@hsdev.com>

;;; Commentary:
;; The original idea for this client was to minimally implement:
;; - make it possible to post to twister directly from Emacs;
;; - have autocompletion for 'known users' when posting a message
;;   (this probably means defining a 'mode')

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
  "The nickname you use on your twister instance. When posting
messages, this will be the name used"
  :type 'string
  :group 'twister)

(defcustom twister-rpcuser "user"
  "The RPC username configured in the twister.conf file"
  :type 'string
  :group 'twister)

(defcustom twister-rpcpassword "pwd"
  "The RPC password for the `twister-rpcuser configured in the
twister.conf file"
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

(defcustom twister-max-msgsize 140
  "Maximum size of messages to post.  Default of the server is 140.
If you want larger messages, you will need to enable automatic splitting
in the twister configuration."
  :type 'integer
  :group 'twister)

(defcustom twister-post-buffername "New Twister Message"
  "Name of the buffer in which Twister Messages can be composed."
  :type 'string
  :group 'twister)

(defvar twister-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'twister-post-buffer)
    (define-key map "\C-c\C-k" 'twister-close-post)
    map) "Keymap for `twister-post-mode'.")

(define-derived-mode twister-post-mode text-mode "twister-post"
  "Twister major mode for posting new messages."
  ;; Reason I want this:
  ;; - define autompletion on @ sign
  ;; - define specific key map for posting messages
  )

;; Preliminary convention
;; tw-* methods      -> sorta private for now, don't use directly
;; twister-* methods -> public API
(defun twister-rpc (method &rest params)
  "Wrapper for the json-rpc method for twister use.
The connection is closed afer each use.  This is not necessarily
the most effective.  METHOD is the RPC method we are calling
while PARAMS contain the rest of the parameters."

  (let* ((twisterd (json-rpc-connect
                    twister-host twister-port
                    twister-rpcuser twister-rpcpassword))

         (result (apply 'json-rpc twisterd method params)))
    (json-rpc-close twisterd)
    result))

(defun tw-get-last-post(user)
  "Get the last post of a user"
  (let (obj (json-new-object))
    (twister-rpc "getposts" 1
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

  (twister-rpc "newpostmsg"
   twister-user
   (tw-get-next-k twister-user) msg))

(defun twister-post-region (begin end)
  "Post the current region to twister"
  (interactive "r")

  (let ((selection (buffer-substring-no-properties begin end)))

    (when (or (<= (length selection) twister-max-msgsize)
              (y-or-n-p (format (concat "The message is %i characters long. "
                                        "Do you still want to post? ")
                                (length selection))))
      (message "Posting message...")
      (twister-post selection))))

(defun twister-post-buffer()
  "Post the current buffer to twister"
  (interactive)

  (twister-post-region (point-min) ( point-max)))

(defun twister-create-post ()
  (interactive)
  "Create a new buffer for writing a note"
  (with-current-buffer (get-buffer-create twister-post-buffername)

    (twister-post-mode)

    (switch-to-buffer-other-window (current-buffer))
    (fit-window-to-buffer (selected-window) 10 10)))

(defun twister-close-post ()
  "Hide and kill the posting buffer if it is the special posting buffer."
  (interactive)
  (when (get-buffer twister-post-buffername)
    (with-current-buffer twister-post-buffername
      ;; if the window is the sole window in its frame, delete-window will error
      (if (window-parent) (delete-window))
      (kill-buffer twister-post-buffername)
      )))

(provide 'twister)
;;; twister.el ends here
