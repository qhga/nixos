;; Downloads cargo, clippy, rust-docs, rust-std, rustc, rustfmt
;; rustup default stable
;; Install missing dependencies
;; rustup component add rls rust-analysis rust-src
(straight-use-package 'rustic)
;; (straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'lsp)

(general-def
  :states 'normal
  :keymaps 'rust-mode-map
  "=" 'rust-format-buffer)

(provide 'l-rust)