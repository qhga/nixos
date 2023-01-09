;; Downloads cargo, clippy, rust-docs, rust-std, rustc, rustfmt
;; rustup default stable
;; Install missing dependencies
;; rustup component add rust-analyzer rust-analysis rust-src
;; rust-analyzer replaced rls
(straight-use-package 'rustic)
;; (straight-use-package 'rust-mode)
(setq lsp-rust-server 'rust-analyzer
      rustic-lsp-server 'rust-analyzer)

(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook (lambda () (setq-local projectile-project-test-cmd "cargo test -- --show-output")))

(general-def
  :states 'normal
  :keymaps 'rust-mode-map
  "=" 'rustic-format-buffer)

(provide 'l-rust)