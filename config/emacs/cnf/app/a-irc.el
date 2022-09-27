;; Load function with password
(if (file-exists-p "~/sync/app_data/emacs/irc/a-irc.el")
    (load "~/sync/app_data/emacs/irc/a-irc.el")
  (warn "Could not load irc config"))

(setq erc-echo-notices-in-minibuffer-flag t)

(provide 'a-irc)