(setq gc-cons-threshold most-positive-fixnum)
;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; load own config files
(setq custom-file (concat user-emacs-directory "customs.el"))
(unless (file-exists-p custom-file)
    (make-empty-file "customs.el"))
(load-file custom-file)

(push (concat user-emacs-directory "cnf") load-path)

;; (debug-watch 'TeX-command-extra-options)
(require 'variables)
(message "Done loading: variables.el")
(require 'essentials)
(message "Done loading: essentials.el")
(require 'keybindings)
(message "Done loading: keybindings.el")
(require 'additionals)
(message "Done loading: additionals.el")
(require 'programming)
(message "Done loading: programming.el")
(require 'ui)
(message "Done loading: ui.el")
(require 'functions)
(message "Done loading: functions.el")
(require 'fixes)
(message "Done loading: fixes.el")
(require 'temp)
(message "Done loading: temp.el")

;; measure startup time and print it
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; set gc values back to smth reasonable
(setq gc-cons-threshold 104857600) ;; 100 MegaByte (LSP)
