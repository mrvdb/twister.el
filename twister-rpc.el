;;; twister-rpc.el --- Implementation of the twister rpc api in emacs lisp
;; Copyright (C) 2015 Marcel van der Boom <marcel@hsdev.com>

;; Author: Marcel van der Boom <marcel@hsdev.com>
;; Maintainer: Marcel van der Boom
;; URL: https://github.com/mrvdb/twister.el
;; Created: 30-08-2015
;; Version: 0.1
;; Keywords: microblogging, json, rpc

;;; Commentary:
;; This is the pure mapping of the RPC api from twister into
;; emacs-lisp functions

;;; Code:

(require 'json-rpc)

(defgroup twister-rpc nil
  "Twister JSON RPC api"
  :group 'twister)

(defcustom twister-host "localhost"
  "Host where the twister daemon runs."
  :type 'string
  :group 'twister-rpc)

(defcustom twister-port 28332
  "Port on which twister daemon runs and serves RPC commands."
  :type 'integer
  :group 'twister-rpc)

(defcustom twister-rpcuser "user"
  "The RPC username configured in the twister.conf file."
  :type 'string
  :group 'twister-rpc)

(defcustom twister-rpcpassword "pwd"
  "The RPC password for the `twister-rpcuser.
This is configured in the twister.conf file"
  :type 'string
  :group 'twister-rpc)

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

(defun twister-getfollowing (user)
  "Get a vector of usernames which are followed by `twister-user'.
The USER parameter is only useful for the locally registered
users.  In most cases this will be the same as the `twister-user'
so we use that if user is not specified."
  (interactive)

  (twister-rpc "getfollowing" user))

(provide 'twister-rpc)
;;; twister-rpc.el ends here
