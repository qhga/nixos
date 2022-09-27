;; curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; p -S rustup
;; rustup update
;; rustup component add rls rust-analysis rust-src
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'lsp)

(general-def
  :states 'normal
  :keymaps 'rust-mode-map
  "=" 'rust-format-buffer)

(provide 'l-rust)