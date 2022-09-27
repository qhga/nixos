;; Prerequisites
;; mkdir /opt/isabelle-emacs && cd /opt/isabelle-emacs
;; git clone https://github.com/m-fleury/isabelle-emacs.git .
;; git checkout Isabelle2021-1-more-vscode
;; ./bin/isabelle components -I
;; ./bin/isabelle components -a
;; ./bin/isabelle build -b HOL
;; OR
;; y -S isabelle
;; Apparently, we are missing out on some features with the normal isabelle build

(straight-use-package 'session-async)
(straight-use-package '(isar-mode :host github :repo "m-fleury/isar-mode" :files ("*.el")))
(straight-use-package '(isar-goal-mode :host github :repo "m-fleury/simp-isar-mode" :files ("*.el")))
(straight-use-package '(lsp-isar :host github :repo "m-fleury/isabelle-emacs" :branch "Isabelle2021-1-more-vscode" :files ("src/Tools/emacs-lsp/lsp-isar/*.el")))

(add-hook 'isar-mode-hook #'lsp-isar-define-client-and-start)
(add-hook 'lsp-isar-init-hook 'lsp-isar-open-output-and-progress-right-spacemacs)

;; The forked isabelle-emacs did not start due to a problem with JAVA_HOME (idk why)
(setq lsp-isar-path-to-isabelle "/opt/isabelle")

;; Adding the AFP
;; Append to the file ~/.isabelle/Isabelle2021-1-vsce/etc/settings

;; AFP=/path/to/AFP/thys
;; Then change in the emacs/spacemacs configuration

;; (setq lsp-isabelle-options (list "-d" "\$AFP"))
;; This is also the place to include further paths. Remember, however, that including more paths, makes Isabelle slower to start.
(provide 'l-isabelle)