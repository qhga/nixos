(require 'sql)
(defalias 'sql-get-login 'ignore)

(load-file (concat phga/sync "/app_data/emacs/sql/mysql.el"))

;; SQLUP: Upcase sql syntax
(straight-use-package 'sqlup-mode)
(push '("\\.sql\\'" . (lambda ()
                        (sql-mode)
                        (sql-highlight-mysql-keywords)
                        (sqlup-mode)
                        (sqlup-capitalize-as-you-type))) auto-mode-alist)

(defun sql-connect-preset (product name)
  (setq sql-product product)
  (sql-connect name))

(defun sql-naz ()
  (interactive)
  (sql-connect-preset 'mysql 'naz)
  (sql-mysql))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (sqlup-mode)))

;; USE LIKE THIS IN ORG-MODE
;; if naz is written without ' it is passed as a string by default
;; the underlying function does not convert to symbol
;; #+begin_src sql :dbconnection 'naz :engine mysql


;; HISTORIC

;; https://www.emacswiki.org/emacs/SqlMode
;; (defvar sql-last-prompt-pos 1
;;   "position of last prompt when added recording started")
;; (make-variable-buffer-local 'sql-last-prompt-pos)
;; (put 'sql-last-prompt-pos 'permanent-local t)

;; (defun sql-add-newline-first (output)
;;   "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
;;     This fixes up the display of queries sent to the inferior buffer
;;     programatically."
;;   (let ((begin-of-prompt
;;          (or (and comint-last-prompt-overlay
;;                   ;; sometimes this overlay is not on prompt
;;                   (save-excursion
;;                     (goto-char (overlay-start comint-last-prompt-overlay))
;;                     (looking-at-p comint-prompt-regexp)
;;                     (point)))
;;              1)))
;;     (if (> begin-of-prompt sql-last-prompt-pos)
;;         (progn
;;           (setq sql-last-prompt-pos begin-of-prompt)
;;           (concat "\n" output))
;;       output)))

;; (defun sqli-add-hooks ()
;;   "Add hooks to `sql-interactive-mode-hook'."
;;   (add-hook 'comint-preoutput-filter-functions
;;             'sql-add-newline-first))

;; (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

(provide 'l-sql)