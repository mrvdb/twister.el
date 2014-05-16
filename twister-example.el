;; Quick example on how to use twister.el

(add-to-list 'load-path "elisp-json-rpc")
(require 'twister)

;; Either use customize or elisp directly to customize

;; Fill in your own nick here
(setq twister-nick "yournick")

(twister-post "I have installed twister.el, and it works! -- https://github.com/mrvdb/twister.el  #emacs #twister")
