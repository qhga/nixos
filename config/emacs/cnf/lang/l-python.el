;; PYTHON
;; PYLS
;; sudo pip install python-lsp-server[all]
;; (setq lsp-pylsp-plugins-flake8-enabled nil
;;       lsp-pylsp-plugins-pylint-enabled nil
;;       lsp-pylsp-plugins-pyflakes-enabled t
;;       lsp-pylsp-plugins-autopep8-enabled nil
;;       lsp-pylsp-plugins-pydocstyle-enabled nil)

;; (add-hook 'python-mode-hook 'lsp)

;; PYRIGHT
(setq python-indent-guess-indent-offset-verbose nil
      lsp-pyright-auto-import-completions t
      lsp-pyright-typechecking-mode "on"
      python-shell-completion-native-enable nil)
;; sudo npm install -g pyright
;; y -S pyright
(straight-use-package 'lsp-pyright)
(add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp)))

(provide 'l-python)