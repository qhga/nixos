;; Stolen from f1p and modified
;; REQUIRES: y -S typescript
(require 'l-web)
(straight-use-package 'tide)
(require 'tide)

(defun fp/tide-mode-hook ()
  (setq-local flycheck-check-syntax-automatically
              '(save idle-change new-line mode-enabled))

  (defun fp/tide-jump-to-definition ()
    (interactive)
    (evil-set-jump)
    (tide-jump-to-definition))

  (company-mode 1)
  (flycheck-mode 1)
  (setq-local company-tooltip-align-annotations t))

(add-hook 'tide-mode-hook 'fp/tide-mode-hook)

(general-def
  :states 'normal
  :keymaps 'tide-mode-map
  "gd" 'fp/tide-jump-to-definition
  "gr" 'tide-references
  "SPC SPC r" 'tide-rename-symbol)

(general-def
  :states 'normal
  :keymaps 'tide-references-mode-map
  "q" 'quit-window
  "RET" 'tide-goto-line-reference
  "a" (lambda () (interactive)
        (tide-goto-line-reference)
        (pulse-momentary-highlight-one-line (point))
        (select-window (previous-window))))

(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; ----------------------------------------------------------------------
;; web-tide-mode
;; ----------------------------------------------------------------------

(evil-define-operator evil-tide-format (beg end)
  "Format text with tide. See `evil-indent' for reference."
  :move-point nil
  :type line
  ;; these two movements mimic the behaviour of `evil-indent`. not sure if they
  ;; are useful, but consistency is always nice
  (goto-char beg)
  (evil-first-non-blank)
  (tide-format-region beg end))

(define-derived-mode web-tide-mode web-mode "WebTide"
  "Web mode with tide"
  (tide-setup))

(flycheck-add-mode 'javascript-tide 'web-tide-mode)

(general-def
  :states '(normal visual)
  :keymaps 'web-tide-mode-map
  "=" 'evil-tide-format)

;; This has to be set again here despite already being configured
;; because tide requires typescript-mode, which inserts itself
;; at the top of auto-mode alist.
(add-to-list 'auto-mode-alist '("\\.\\(tsx?\\)\\|\\(jsx?\\)\\'" . web-tide-mode))

;; PRETTIER: Formats js code
;; Requires: prettier
(straight-use-package 'prettier-js)
(add-hook 'web-tide-mode-hook 'prettier-js-mode)

;; If we want to use the prettier in node modules (pkg: add-node-modules-path)
;; --no-semi, --use-tabs, --no-bracket-spacing: Not required bc default
;; (setq prettier-js-args
;;       '("--print-width=90"
;;         "--tab-width=2"
;;         "--single-quote"
;;         "--trailing-comma=es5"
;;         "--parser=babel" ;; used to be babylon (should be the standard anyway)
;;         "--jsx-single-quote"
;;         ))

(provide 'l-typescript)