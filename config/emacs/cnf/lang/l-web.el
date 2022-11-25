;; WEB
(straight-use-package 'web-mode)
(setq web-mode-engines-alist
      '(("go" . "\\.html\\'"))
      web-mode-enable-auto-quoting nil)
;; (straight-use-package 'php-mode)
;; (push '("\\.php\\'" . php-mode) auto-mode-alist)

;; EMMET
(straight-use-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

;; Keybindings
(phgas-leader
  :states 'normal
  :definer 'minor-mode
  :keymaps 'emmet-mode
  "SPC e" '((lambda() (interactive) (evil-append 1) (emmet-expand-line nil))
            :which-key "expand emmet"))

;; TAILWINDCSS
(straight-use-package 'lsp-tailwindcss)

(provide 'l-web)