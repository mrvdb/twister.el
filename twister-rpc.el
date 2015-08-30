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

;;; Implemented methods:
;;
;; - getfollowing(user) : get vector of users that USER follows
;; - getgroupinfo(alias): get info about group specified by ALIAS
;; - getspammsg         : get the message sent for generated blocks
;; - help(method)       : retrieves help messages for METHOD
;; - listgroups         : get vector of registered group aliases
;; - listwalletusers    : get vector of registered users
;;
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

(defun twister-help(method)
  "Gets the help description for METHOD"
  (twister-rpc "help" method))

(defun twister-getfollowing (user)
  "Get a vector of usernames which are followed by `twister-user'.
The USER parameter is only useful for the locally registered
users.  In most cases this will be the same as the `twister-user'
so we use that if user is not specified."
  (twister-rpc "getfollowing" user))

(defun twister-listgroups()
  "Get a vector of the registered groups."
  (twister-rpc "listgroups"))

(defun twister-getgroupinfo(alias)
  "Get info about a groupd specified by ALIAS.

This returns a list with three labeled members:
`members'     : a vector of the members of the group
`description' : a description of the group
`alias'       : the group alias."
  (twister-rpc "getgroupinfo" alias))

(defun twister-listwalletusers()
  "Get a vector of the local users."
  (twister-rpc "listwalletusers"))

(defun twister-getspammsg()
  "Returns a vector with the message that is sent for generated
blocks."
  (twister-rpc "getspammsg"))

(provide 'twister-rpc)
;;; twister-rpc.el ends here
