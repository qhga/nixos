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
;; (load-file custom-file)

(push (concat user-emacs-directory "cnf") load-path)

(require 'variables)
(require 'essentials)
(require 'keybindings)

;; Launch: /bin/emacs -q --load ~/.emacs.d/minimal.el
;; PUT THE CODE YOU WANT TO TEST IN BETWEEN THOSE TWO LINES
;; --------------------------------------------------------
(push (concat user-emacs-directory "cnf/app") load-path)
(require 'a-mvtn)
(require 'a-orgmode)
;; --------------------------------------------------------

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
