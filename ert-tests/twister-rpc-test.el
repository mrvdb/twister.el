;; Unit test for twister-rpc.el

(require 'twister-rpc)
(when (require 'undercover nil t)
  (undercover "twister-rpc.el"))

;; Set up our test environment

;; TEMPORARY OVERRIDE for json-rpc-ensure to support IPv6
(defun json-rpc-ensure (connection)
  "Re-establish connection to CONNECTION if needed, returning CONNECTION."
  (let ((old-process (json-rpc-process connection)))
    (if (and old-process (process-live-p old-process))
        connection
      (let* ((buffer (generate-new-buffer " *json-rpc*"))
             (host (json-rpc-host connection))
             (process (make-network-process :name (format "json-rpc-%s" host)
                                            :buffer buffer
                                            :host host
                                            :service (json-rpc-port connection)
                                            :family nil
                                            :coding '(utf-8 . utf-8))))
        (setf (process-sentinel process)
              (lambda (proc _)
                (run-at-time 0 nil #'kill-buffer (process-buffer proc))))
        (prog1 connection
          (setf (json-rpc-process connection) process))))))

;; Specify connectivity to our test-server
;; This should go into the travis.yml file, so we can use
;; multiple test environments, for example an ipv4 one
;; because travis-ci has no IPv6 outgoing connectivity yet.

(setq twister-rpcuser "travis")
(setq twister-rpcpassword "wmI9HQGumUBmY89wBRUiuBE5")
(setq twister-host "185.92.221.186")
(setq twister-port "28332")

;; ;; Check if we have connectivity to our testmachine
;; (ert-deftest ensure-connectivity nil
;;   "Make sure we can connect to our RPC server for the tests"
;;   :tags '(:interactive)
;;   (let* ((server (json-rpc-connect
;; 		  twister-host twister-port
;; 		  twister-rpcuser twister-rpcpassword)))

;;     ;; We should have a live connection no
;;     ;; FIXME: i am not 100% sure this is a reliable test,
;;     ;; for example, when 403 is expected, this still succeeds.
;;     (should (json-rpc-live-p server))

;;     ;; Close it and git it some time
;;     (json-rpc-close server)
;;     (json-rpc-close server)
;;     (sleep-for 2)

;;     ;; Now we should not have one
;;     ;; (should-not (json-rpc-live-p server))
;;     ))

;; the above test is a precondition for all the others, I'm
;; not sure how to encode that into the tests.

(ert-deftest basic-return-types nil
  "Make sure our implemented methods return the proper types."
  :tags '(:interactive)
  (should (eql (type-of (twister-help "help")) 'string))
  (should (eql (type-of (twister-getfollowing "")) 'vector))
  (should (eql (type-of (twister-listgroups)) 'vector))
  (should (eql (type-of (twister-getspammsg)) 'vector))
  (should (eql (type-of (twister-listwalletusers)) 'vector))
  ;; getgroupinfo requires that there is a group
  )

;;; Tests I want to have:
;; - for all methods, a predictable test
;; - get k, post, get next-k,  next-K should be k+1
;; - check return types
;;
(provide 'twister-rpc-test)
;;; twister-rpc-test.el ends here
