;; General settings
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2
              tab-width 2
              comint-terminfo-terminal "dumb" ;; compilation output escape sequences
              compilation-read-command t
              compilation-scroll-output t
              compilation-window-height 10)


(add-hook 'prog-mode-hook (lambda () (require 'whitespace)
                            (setq show-trailing-whitespace t)))
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; RAINBOW-DELIMITERS: colorful parens
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; COMPANY: autocompletion
(straight-use-package 'company)
(setq company-idle-delay 0.0
      company-tooltip-limit 5
      company-minimum-prefix-length 3
      company-echo-delay 0
      company-auto-complete nil)
(add-hook 'prog-mode-hook 'company-mode)

;; Keybindings
(defun phga/keymap-company ()
  (interactive)
  (general-def
	  :keymaps 'company-active-map
	  "<tab>" 'company-complete-selection
    "<return>" nil
    "RET" nil))

(general-def
  :states '(normal insert)
  :keymaps 'company-mode-map
  "C-<return>" 'company-search-candidates)

(add-hook 'company-mode-hook 'phga/keymap-company)

;; COMPANY-PRESCIENT: better autocomplete suggestions based on past selections
(straight-use-package 'company-prescient)
;; hooks
(add-hook 'company-mode-hook 'company-prescient-mode)

;; YASNIPPET: snippets
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(require 'yasnippet)
(yas-reload-all)
;; hooks
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
;; (add-hook 'yas-minor-mode-hook 'yas-reload-all)

;; PROJECTILE
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(counsel-projectile-mode)
(straight-use-package 'imenu-anywhere)

(phgas-leader
  :states '(normal visual)
  :keymaps 'override
  "p" '(:ignore t :which-key "Projectile")
  "p f" 'projectile-find-file
  "p d" 'projectile-switch-project
  "p i" 'projectile-install-project
  "p c" 'projectile-compile-project
  "p R" 'projectile-run-project
  "p t" 'projectile-test-project
  "p r" 'counsel-projectile-rg
  )

(defvar phga/post-compilation-auto-quit-enable t
  "Set this to nil to deactivate `phga/post-compilation-auto-quit'.")

(defun phga/post-compilation-auto-quit (buf str)
  "Close the compilation window and kill buffer BUF if no error or warning was yielded in STR."
  (when (and phga/post-compilation-auto-quit-enable
             (null (string-match "\\(.*exited abnormally.*\\|.*[Ww]arning.*\\)" str)))
    ;;no errors, make the compilation window go away in a few seconds
    (run-at-time "3 sec" nil 'quit-window t (get-buffer-window buf))
    (message "No Compilation Errors!")))

(add-to-list 'compilation-finish-functions
             'phga/post-compilation-auto-quit)

;; FLYCHECK: on the fly syntax checking
(straight-use-package 'flycheck)
(setq flycheck-highlighting-mode 'symbols) ;; full symbols not cols
;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
(add-hook 'prog-mode-hook #'flycheck-mode)

;; Keybindings
(general-def :states '(normal visual) :keymaps 'prog-mode-map
  "g F" 'dired-at-point
  "g ! n" 'flycheck-next-error
  "g ! p" 'flycheck-previous-error
  "g ! l" 'flycheck-list-errors
  )

;; LSP: language server in emacs
;; In case shit breaks -> goto github page - search for stable lable - reset to commit xxxxxx
(straight-use-package 'lsp-mode)
(setq lsp-eldoc-render-all nil
      read-process-output-max 1048576
      lsp-idle-delay 0.500
      lsp-prefer-capf t
      lsp-enable-indentation nil
      lsp-headerline-breadcrumb-enable-diagnostics nil)
;; The lsp-prefer-capf variable did not work anymore and probably another company backend
;; was the source of the weird flycheck/make errors which arised with any lsp-backend for c++
;; This fixes the issue by changing the company backend to the one recommended by the lsp team
(add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))

(straight-use-package 'lsp-ui)
(with-eval-after-load 'lsp (require 'lsp-ui-flycheck))
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-ui-doc-enable nil
      lsp-enable-symbol-highlighting t
      lsp-lens-enable nil ;; ref count
      lsp-ui-doc-include-signature t
      lsp-ui-peek-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-sideline-enable nil)

;; DAP: Debug Adapter Protocol is a wire protocol for communication
;;      between client and Debug Server
(straight-use-package 'dap-mode)
(dap-auto-configure-mode)

;; DIRENV: works by invoking direnv to obtain the environment for the current file, then
;; updating the emacs variables process-environment and exec-path
(straight-use-package 'direnv)
(advice-add 'lsp :before #'direnv-update-environment)

;; OTHER SMALL LANGUAGES: with nearly no setup
;; JS
;; (straight-use-package 'js2-mode)
;; (push '("\\.\\(js\\)\\'" . js2-mode) auto-mode-alist)
;; ;; sudo npm i -g javascript-typescript-langserver
;; (add-hook 'js2-mode-hook #'lsp-deferred)

;; STRUCTURE LANGUAGES: mostly highlighting and indent
;; CADDY
(straight-use-package 'caddyfile-mode)
(push '("\\(Caddyfile\\|caddy.conf\\)\\'" . caddyfile-mode) auto-mode-alist)

;; JSON
(straight-use-package 'json-mode)
(push '("\\.\\(json\\|imp\\)\\'" . json-mode) auto-mode-alist)
(add-hook 'json-mode-hook
          (lambda () (json-pretty-print-buffer)))

;; CSV
(straight-use-package 'csv-mode)
(push '("\\.\\(csv\\|xls\\)\\'" . csv-mode) auto-mode-alist)

;; YAML
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq evil-shift-width yaml-indent-offset)))

;; DOCKER-COMPOSE
(straight-use-package 'docker-compose-mode)
(push '("docker-compose.yml\\'" . docker-compose-mode) auto-mode-alist)
(straight-use-package 'dockerfile-mode)
(push '("Dockerfile\\'" . dockerfile-mode) auto-mode-alist)

;; NGINX
(straight-use-package 'nginx-mode)

;; RAINBOW
(straight-use-package 'rainbow-mode)

;; EDITOR-CONFIG
(straight-use-package 'editorconfig)
(editorconfig-mode t)

;; RESTCLIENT
(straight-use-package 'restclient)
(general-def
  :states 'normal
  :keymaps 'restclient-mode-map
  "TAB" 'restclient-toggle-body-visibility
  "C-c C-c" 'restclient-http-send-current-stay-in-window)
(push '("\\.http\\'" . restclient-mode) auto-mode-alist)

;; PYTHON-INTERACTIVE-SHELL
(setq python-shell-completion-native-disabled-interpreters
      '("pypy" "ipython" "python")) ;; added python bc of warning (readline support)

;; STYLUS MODE
(straight-use-package 'stylus-mode)
(straight-use-package 'sws-mode)

(straight-use-package 'haskell-mode)

;; Additional languages with more config
(push (concat user-emacs-directory "cnf/lang") load-path)

;; load languages if required
(push '("\\.nix\\'" . (lambda () (require 'l-nix) (nix-mode))) auto-mode-alist)
(push '("\\.rs\\'" . (lambda () (require 'l-rust) (rust-mode))) auto-mode-alist)
(push '("\\.thy\\'" . (lambda () (require 'l-isabelle) (isar-mode))) auto-mode-alist)
(push '("\\.go\\'" . (lambda () (require 'l-go) (go-mode))) auto-mode-alist)
(push '("\\.\\([ch]pp\\|cc\\)\\'" . (lambda () (require 'l-cc) (c++-mode))) auto-mode-alist)
(push '("\\.[ch]\\'" . (lambda () (require 'l-cc) (c-mode))) auto-mode-alist)
(push '("\\.tex\\'" . (lambda () (require 'l-tex) (LaTeX-mode))) auto-mode-alist)
(push '("\\.el\\'" . (lambda () (require 'l-elisp) (emacs-lisp-mode))) auto-mode-alist)
(push '("\\.py\\'" . (lambda () (require 'l-python) (python-mode))) auto-mode-alist)
(push '("\\.java\\'" . (lambda () (require 'l-java) (java-mode))) auto-mode-alist)
(push '("\\.\\(tsx?\\|jsx?\\)\\'" . (lambda () (require 'l-typescript) (web-tide-mode))) auto-mode-alist)
(push '("\\.\\(xml\\|html\\|php\\|css\\)\\'" . (lambda () (require 'l-web) (web-mode))) auto-mode-alist)

;; SQL
(require 'l-sql)

;; Keybindings
(phgas-leader
  :states 'normal
  :keymaps '(go-mode-map c++-mode-map c-mode-map java-mode-map rust-mode-map)
  "SPC g d" 'lsp-find-definition
  "SPC g D" 'lsp-find-declaration
  "SPC g t" 'lsp-goto-type-definition
  "SPC r" 'lsp-rename
  "SPC d" 'lsp-ui-doc-glance
  "SPC i" 'imenu
  "SPC p i" 'ivy-imenu-anywhere
  )

(phgas-leader
  :states 'normal
  :keymaps '(js2-mode-map python-mode-map)
  "SPC g d" 'lsp-find-definition
  "SPC g D" 'lsp-find-declaration
  "SPC g t" 'lsp-goto-type-definition
  "SPC r" 'lsp-rename
  "SPC d" 'lsp-ui-doc-glance
  "SPC i" 'imenu
  "SPC p i" 'ivy-imenu-anywhere
  )

(phgas-leader
  :states 'normal
  :keymaps 'lsp-mode-map
  "SPC l i" 'lsp-organize-imports
  "SPC l c" 'lsp-describe-session
  "SPC l r" 'lsp-restart-workspace)

;; IDENTATION & other stuff
(general-def
  :states '(normal)
  :keymaps '(prog-mode-map)
  "=" 'phga/format-buffer
  "<C-tab>" (lambda() (interactive) (evil-append 1) (company-complete)))

(general-def
  :states '(insert)
  :keymaps '(prog-mode-map)
  "<C-tab>" (lambda() (interactive) (evil-append 1) (company-complete)))

(general-def
  :states '(visual)
  :keymaps '(prog-mode-map org-mode-map)
  "=" 'evil-indent
  "+" 'align-regexp)

;; nXML mode
(general-def
  :states '(normal)
  :keymaps '(nxml-mode-map)
  "=" 'phga/format-xml-buffer)

;; JSON mode
(general-def
  :states '(normal)
  :keymaps '(json-mode-map)
  "=" 'phga/format-json-buffer)

;; HISTORIC
;; (phgas-leader
;;   :states '(normal visual)
;;   :keymaps 'matlab-mode-map
;;   "SPC r" 'matlab-shell-run-region-or-line
;;   "SPC c" (lambda () (interactive) (matlab-shell-run-region (point-min) (point-max)))
;;   )

(provide 'programming)
