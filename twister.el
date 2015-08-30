;;; twister.el --- A client for the twister distributed microblogging system
;; Copyright (C) 2014-2015 Marcel van der Boom <marcel@hsdev.com>

;; Author: Marcel van der Boom <marcel@hsdev.com>
;; Maintainer: Marcel van der Boom
;; URL: https://github.com/mrvdb/twister.el
;; Created: 14-05-2014
;; Version: 0.1
;; Keywords: microblogging, json

;;; Commentary:
;; The original idea for this client was to minimally implement:
;; ✓ make it possible to post to twister directly from Emacs;
;; ✓ have autocompletion for 'known users' when posting a message
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
;; - pcomplete: (built into emacs)

;; Installation
;; (add-to-list 'load-path "/path/to/twister.el")
;; (require 'twister)
;; (setq twister-user "yournick")
;; (twister-create-post)
;;
;;; Credits
;;  - The elisp-json-rpc library of Christopher Wellons <wellons@nullprogrma.com>
;;    does the real work
;;  - some code was mimicked from identica-mode by Gabriel Saldana

;;; Code:
(require 'json-rpc)
(require 'pcomplete)

;; Configuration variables
(defgroup twister nil
  "Using the twister microblogging system"
  :tag "Microblogging"
  :group 'applications
  )

(defgroup twister-faces nil
  "Faces for twister mode"
  :group 'twister
  :group 'faces)

(defcustom twister-user "twister_user"
  "The nickname you use on your twister instance.
When posting messages, this will be the name used"
  :type 'string
  :group 'twister)

(defcustom twister-rpcuser "user"
  "The RPC username configured in the twister.conf file."
  :type 'string
  :group 'twister)

(defcustom twister-rpcpassword "pwd"
  "The RPC password for the `twister-rpcuser.
This is configured in the twister.conf file"
  :type 'string
  :group 'twitter)

(defcustom twister-host "localhost"
  "Host where the twister daemon runs."
  :type 'string
  :group 'twister)

(defcustom twister-port 28332
  "Port on which twister daemon runs and serves RPC commands."
  :type 'integer
  :group 'twister)

(defcustom twister-max-msgsize 140
  "Maximum size of messages to post.  Default of the server is 140.
If you want larger messages, you will also need to enable
automatic splitting in the twister configuration."
  :type 'integer
  :group 'twister)

(defcustom twister-post-buffername "New Twister Message"
  "Name of the buffer in which Twister Messages can be composed."
  :type 'string
  :group 'twister)

(defcustom twister-preview-formatting t
  "Preview formatting specifiers in the post buffer.
Twister has support for *bold*, ~italic~, -strike-through- and
_underlined_ format specifiers."
  :type 'boolean
  :group 'twister)

(defcustom twister-active-addresses t
  "Make URLs and mail addresses clickable.
This uses the standard `goto-address-mode'."
  :type 'boolean
  :group 'twister)

(defface twister-hashtag
  '((default (:inherit link :underline nil)))
  "Twister mode face for hash tags"
  :group 'twister-faces)

(defface twister-bold
  '((default (:inherit bold)))
  "Twister mode face for bold text"
  :group 'twister-faces)

(defface twister-italic
  '((default (:inherit italic)))
  "Twister mode face for italic text"
  :group 'twister-faces)

(defface twister-underline
  '((default :inherit underline))
  "Twister mode face for underlined text"
  :group 'twister-faces)

(defface twister-nickname
  '((default (:inherit font-lock-type-face )))
  "Twister mode face for nicknames"
  :group 'twister-faces)

(defface twister-strikethrough
  '((default (:inherit default :strike-through t)))
  "Twister mode face for strike-through text"
  :group 'twister-faces)

;; End configuration variables

(defvar twister-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c"  'twister-post-buffer)
    (define-key map "\C-c\C-k"  'twister-close-post)
    (define-key map "\C-i"      'completion-at-point) ;; \C-i == TAB
    (define-key map "\C-c\C-s"  'twister-shortenurl-replace-at-point)
    map) "Keymap for `twister-post-mode'.")

(define-derived-mode twister-post-mode text-mode "twister-post"
  "Twister major mode for posting new messages."
  ;; Reason I want this:
  ;; ✓ define autompletion on @ sign
  ;; ✓ define specific key map for posting messages
  )

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

(defun twister-get-last-post(user)
  "Get the last post of a user"
  (let (obj (json-new-object))
    (twister-rpc "getposts" 1
              (vector (json-add-to-object obj "username" user)))))

(defun twister-get-next-k(user)
  "Get the next 'k' for a user; this is a post sequence number.
The data structure that contains it is documented at:
http://twister.net.co/?page_id=21"

  (+ 1 (plist-get
   (plist-get
    (elt (twister-get-last-post user) 0)
    :userpost) :k)))

(defun twister-post(msg)
  "Post msg to the configured twister daemon"
  (interactive)

  (twister-rpc "newpostmsg"
   twister-user
   (twister-get-next-k twister-user) msg))

(defun twister-post-region (begin end)
  "Post the current region to twister.
The BEGIN and END arguments are the usual points of the region."
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
  "Create a new buffer for writing a note."
  (interactive)
  (with-current-buffer (get-buffer-create twister-post-buffername)

    (twister-post-mode)

    (switch-to-buffer-other-window (current-buffer))
    (fit-window-to-buffer (selected-window) 10 10)))

(defun twister-close-post ()
  "Hide and kill the posting buffer if it is the special posting buffer.
This is typically used in combination with `twister-create-post'
to end the posting activity."
  (interactive)
  (when (get-buffer twister-post-buffername)
    (with-current-buffer twister-post-buffername
      ;; if the window is the sole window in its frame, delete-window will error
      (if (window-parent) (delete-window))
      (kill-buffer twister-post-buffername)
      )))

(defun twister-getfollowing (&optional user)
  "Get a vector of usernames which are followed by `twister-user'.
The USER parameter is only useful for the locally registered
users.  In most cases this will be the same as the `twister-user'
so we use that if user is not specified."
  (interactive)

  (twister-rpc "getfollowing" (if user user twister-user)))

(defun twister-completion-entries ()
  "Produce a list of entries to which completion can be matched.
For now, this is just the nicknames the user follows"

  (mapcar (lambda (x) (concat "@" x)) (twister-getfollowing)))

(defun twister-parse-completion-arguments ()
  "Look for completable items between POINT and what is before it.
This includes '@nicknames' and '#hashtags' for the moment."
  (save-excursion
    (let* ((end (point))
           (start (search-backward "@" nil t)) ;; Only search @.... stuff
           (ptt (if start start end)))
      (list (list "dummy"  ;; hmm, not liking this
                  (buffer-substring-no-properties ptt end))
            (point-min) ptt))))

(defun twister-default-completion ()
  "Default completion function for twister."
  (pcomplete-here (twister-completion-entries)))


;; Define autocompletion for the twister-post-mode
(defun pcomplete-twister-post-setup ()
  "Setup `twister-post-mode' to use pcomplete."
  (interactive)

  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'twister-parse-completion-arguments)

  (set (make-local-variable 'pcomplete-default-completion-function)
       'twister-default-completion))

(defun twister-nick-completion-at-point ()
  "Using nicknames, provide a completion table for the text around point."
  (interactive)

  (let* ((end (point))
         (start
          (save-excursion
            (+ end (skip-syntax-backward "w_."))))) ;; '@' is punctuation???

    (list start end (twister-completion-entries) :exclusive t)))

(makunbound 'twister-post-font-lock-keywords)
(defvar twister-post-font-lock-keywords
  '(("#[[:alnum:]_]+"             . 'twister-hashtag)
    ("\\(^\\| \\)\\(@[[:alnum:]_]+\\)"   . (2  'twister-nickname))
    ("\\*\\([[:alnum:]-_]+\\)\\*" . (1 'twister-bold))
    ("~\\([[:alnum:]-_]+\\)~"     . (1 'twister-italic))
    ("_\\([[:alnum:]-_]+\\)_"     . (1 'twister-underline))
    ("\\-\\([[:alnum:]-_]+\\)\\-" . (1 'twister-strikethrough)))
  "Syntax highlighting keywords for twister mode.")

(defun twister-post-mode-setup ()
  "Initialize the twister post-mode."

  ;; Set up pcomplete
  ;; This really should be left up to to user
  (pcomplete-twister-post-setup)

  ;; Add nick completion to at-point completion
  (add-hook 'completion-at-point-functions
	    'twister-nick-completion-at-point nil t)

  (set (make-local-variable 'font-lock-defaults) nil)
  (setq font-lock-defaults '(twister-post-font-lock-keywords))

  ;; Make links clickable
  (if twister-active-addresses
      (goto-address-mode))

  ;; Add counter to the modeline, so we can see what we are doing
  (setq mode-line-format
	(cons (format "%s (%%i/%s) " "Twist:" twister-max-msgsize) mode-line-format)))

(add-hook 'twister-post-mode-hook 'twister-post-mode-setup)

(defun twister-ur1ca-get (api longurl)
  "Shortens url through ur1.ca.
API is their endpoint to which LONGURL is posted for shortening."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "longurl=" (url-hexify-string longurl)))
         (buffer (url-retrieve-synchronously api)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (prog1
          (if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
              (match-string-no-properties 1)
            (error "URL shortening service failed: %s" longurl))
        (kill-buffer buffer)))))

(defun twister-shortenurl-replace-at-point ()
  "Replace the url at point with a shorter version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (twister-ur1ca-get "http://ur1.ca" (thing-at-point 'url))))
        (when url
          (save-restriction
            (narrow-to-region (first url-bounds) (rest url-bounds))
            (delete-region (point-min) (point-max))
            (insert url)))))))


(provide 'twister)
;;; twister.el ends here
