;; ORG-MODE: FIX so the correct org package is loaded (not the one shipped with emacs)
;; Has to be as early in the config as possible, so no other package can load the
;; incorrect org version beforehand
(straight-use-package 'org)

;; ASYNC: Asynchronous functionality
(straight-use-package 'async)
(require 'async)

;; GENERAL: Keybindings
(straight-use-package 'general)
(require 'general)

(general-create-definer phgas-leader
  :prefix "<SPC>"
  :non-normal-prefix "C-SPC")

;; EVIL:
;; depends on: goto-chg, undo-tree
(straight-use-package 'evil)
(straight-use-package 'undo-tree)
;; variables
(setq evil-want-C-u-scroll t
      evil-want-integration t
      evil-want-keybinding nil
      evil-undo-system 'undo-tree)
(require 'evil)
;; start mode
(evil-mode t)
(global-undo-tree-mode t)

;; https://github.com/emacs-evil/evil/issues/1288
;; Credit goes to: https://github.com/nnicandro
;; Fix for the broken org-src-tab-acts-natively functionality
;; Tab in fact does nothing in src blocks if evil is enabled
(defun evil-org-insert-state-in-edit-buffer (fun &rest args)
  "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
  (let ((evil-default-state 'insert)
        ;; Force insert state
        evil-emacs-state-modes
        evil-normal-state-modes
        evil-motion-state-modes
        evil-visual-state-modes
        evil-operator-state-modes
        evil-replace-state-modes)
    (apply fun args)
    (evil-refresh-cursor)))

(advice-add 'org-babel-do-key-sequence-in-edit-buffer
            :around #'evil-org-insert-state-in-edit-buffer)

;; EVIL-NERD-COMMENTER: Easy un/comment
(straight-use-package 'evil-nerd-commenter)

;; EVIL-SURROUND:
(straight-use-package 'evil-surround)
;; start mode
(global-evil-surround-mode t)

;; EVIL-COLLECTION: improved evil support for multiple packages
(straight-use-package 'evil-collection)
;; variables
(setq evil-collection-setup-minibuffer t
      evil-collection-mode-list
      '(ibuffer help calc nov man calendar ivy minibuffer dired debug
        doc-view arc-mode magit vterm))
;; start mode
(evil-collection-init)

;; IVY: better popup menu
(straight-use-package 'ivy)
;; variables
(setq ivy-use-virtual-buffers t
      ivy-re-builders-alist '((t . ivy--regex-ignore-order))
      enable-recursive-minibuffers t
      ivy-wrap t
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create
      ivy-count-format "%d/%d ")
;; remove caret (has to be counsel not ivy #thanksfornothinginternet)
(with-eval-after-load 'counsel (setq ivy-initial-inputs-alist nil))
;; start mode
(ivy-mode t)

;; Keybindings
(phgas-leader
  :states 'normal
  :keymaps 'ivy-minibuffer-map
  "SPC f" 'ivy-immediate-done)

;; IVY-PRESCIENT: better suggestions for ivy
(straight-use-package 'ivy-prescient)
;; start mode
(ivy-prescient-mode)

;; SWIPER: / search
(straight-use-package 'swiper)

;; COUNSEL: several improved functions like find-file, etc.
(straight-use-package 'counsel)
(setq counsel-grep-base-command "rg -i -M 120 --hidden --no-heading --line-number --color never %s"
      counsel-rg-base-command "rg -i -M 120 --hidden --no-heading --line-number --color never %s")

;; WHICH-KEY: hints in the mini bar
(straight-use-package 'which-key)
(which-key-setup-minibuffer)
;; start mode
(which-key-mode)

;; ACE-WINDOW: jump between windows faster
(straight-use-package 'ace-window)
(setq aw-scope 'frame
      aw-keys '(?h ?t ?n ?s ?u ?e ?o))

;; ELECTRIC-PAIR: auto-balance brackets
(electric-pair-mode t)
;; electric-indent-mode is a bit buggy in src blocks...
;; Unfortunately it adds a bug, where the cursor jumps to the start of the src
;; block after inserting a nested block of any sort {}, [], () and pressing RET
;; This is triggered by: org-return -> newline-and-indent
;; Adding an advice did not work, since the point must not only be recovered
;; but also increased by the level of indention... Also, it seemed like another
;; function is responsible for the reset since I added the advie to
;; org-babel-do-key-sequence-in-edit-buffer (Probably newline-and-indent)

;; hooks
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
;; MESSAGE BUFFER:
(evil-initial-state 'message-mode 'normal)

(provide 'essentials)
