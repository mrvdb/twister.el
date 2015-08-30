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

;; Check if we have connectivity to our testmachine
(ert-deftest ensure-connectivity nil
  "Make sure we can connect to our RPC server for the tests"
  :tags '(:interactive)
  (let* ((server (json-rpc-connect
		  twister-host twister-port
		  twister-rpcuser twister-rpcpassword)))

    ;; We should have a live connection now
    (should (json-rpc-live-p server))

    ;; Close it and git it some time
    (json-rpc-close server)
    (sleep-for 2)

    ;; Now we should not have one
    (should-not (json-rpc-live-p server))))
