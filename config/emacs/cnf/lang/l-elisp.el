(phgas-leader
  :keymaps 'emacs-lisp-mode-map
  :states 'normal
  "x" '(:ignore t :which-key "Eval")
  "x e" 'eval-last-sexp
  "x b" 'eval-buffer
  )

(phgas-leader
  :keymaps 'emacs-lisp-mode-map
  :states 'visual
  "x" '(:ignore t :which-key "Eval")
  "x e" 'eval-region
  )

(provide 'l-elisp)