;; Requires wgrep package (is loaded as dependency through straight)
(straight-use-package 'rg)

;; Initial State
(evil-set-initial-state 'rg-mode 'normal)

;; Keybindings
(general-def
  :states 'normal
  :keymaps 'rg-mode-map
  "i" 'wgrep-change-to-wgrep-mode
  "a" 'wgrep-change-to-wgrep-mode
  "c" 'wgrep-change-to-wgrep-mode

  ")" 'rg-next-file
  "(" 'rg-prev-file
  "k" 'compilation-previous-error
  "j" 'compilation-next-error
  "M-p" 'previous-error-no-select
  "M-n" 'next-error-no-select
  "e" 'evil-forward-word-end
  "E" 'evil-forward-WORD-end
  "w" 'evil-forward-word-begin
  "W" 'evil-forward-WORD-begin
  "b" 'evil-backward-word-begin
  "B" 'evil-backward-WORD-begin

  "l" 'evil-forward-char
  "h" 'evil-backward-char
  "g g" 'evil-goto-first-line
  "G" 'evil-goto-line

  "SPC SPC ." 'wgrep-save-all-buffers
  "C-c C-c" (lambda () (interactive) (wgrep-save-all-buffers) (quit-window))
  "q" 'quit-window
  )

(general-def
  :states '(motion visual)
  :keymaps 'rg-mode-map
  "e" 'evil-forward-word-end
  "l" 'evil-forward-char
  "w" 'evil-forward-word-begin
  "g g" 'evil-goto-first-line
  )

(general-def
  :states '(normal insert visual emacs)
  :keymaps 'wgrep-mode-map
  "C-c C-c" 'wgrep-finish-edit
  "C-c C-k" 'wgrep-abort-changes
  )

(provide 'a-rg)