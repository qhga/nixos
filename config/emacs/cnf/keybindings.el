(defvar phga/sync "~/sync")
(defvar phga/lumi "~/lumi")
(if (string= "hisoka" (system-name))
    (defvar phga/git "/fast/git")
(defvar phga/git "~/git"))
(defvar phga/pw "~/.password-store")
(defvar phga/work (concat phga/sync "/work"))
(defvar phga/emacsconf "~/.emacs.d/cnf")
(defvar phga/dotfiles "~/.dotfiles")
(defvar phga/fresh "~/fresh")
(defvar phga/semesterfiles (concat phga/sync "/docs/study/MASTER/s3"))
(defvar phga/orgfiles (concat phga/sync "/org"))
(defvar phga/books "~/shares/nxt/no_sync/books")

;; Ged rid of default bindings I don't enjoy :<
(general-def :states '(normal insert emacs visual) :keymaps 'override
  ;; This motherf***** + another ESC used to close my windows. NOO MOOORE!
  "M-ESC" (lambda () (interactive) (message "You pressed it again Mr. Fatfinger...")))


(general-def
  :states 'normal
  :keymaps 'epa-key-list-mode-map
  "m" 'epa-mark-key
  "u" 'epa-unmark-key)

(phgas-leader
  :states 'motion
  :keymaps '(compilation-mode-map)
  "a" 'ace-window
  "k k" 'kill-buffer-and-window)

;; EMACS-WIDE BINDINGS
(phgas-leader
  :states '(normal visual emacs)
  :keymaps 'override

  "SPC" '(:ignore t :which-key "MinorMode Binds")
  "SPC p" 'fill-paragraph
  "SPC SPC" 'counsel-M-x

  "a" 'ace-window
  "d" (lambda () (interactive) (message "Nah we ain't using this anymore (;"))
  "f" 'counsel-find-file
  "F" 'phga/find-file-sudo
  "g" 'magit-status
  "h" 'evil-avy-goto-line
  "i" 'counsel-imenu
  "y" 'counsel-yank-pop

  "8" 'insert-char

  "/" 'swiper
  ";" 'evilnc-comment-or-uncomment-lines
  "." 'save-buffer
  ")" 'evil-forward-section-end
  "(" 'evil-backward-section-begin


  ;; window stuff
  "w" '(:ignore t :which-key "Window")
  "w w" 'other-window
  "w v" 'split-window-right
  "w h" 'split-window-below
  "w f" 'toggle-maximize-buffer
  "w L" 'windsize-right
  "w H" 'windsize-left
  "w J" 'windsize-down
  "w K" 'windsize-up

  ;; switch stuff
  "s" '(:ignore t :which-key "Switch")
  "s b" 'switch-to-buffer
  "s n" 'switch-to-next-buffer
  "s p" 'switch-to-prev-buffer
  "s s" 'counsel-switch-buffer
  "s a" 'ace-swap-window

  ;; kill stuff
  "k" '(:ignore t :which-key "Kill")
  "k b" 'kill-buffer
  "k w" 'delete-window
  "k W" 'kill-buffer-and-window
  "k o b" 'kill-some-buffers
  "k o w" 'delete-other-windows
  "k q q" 'kill-emacs
  "k k" 'kill-current-buffer

  ;; buffer stuff
  "b" '(:ignore t :which-key "Buffer (revert)")
  "b r" 'revert-buffer
  "b a r" 'auto-revert-mode

  ;; edit stuff
  "e" '(:ignore t :which-key "Edit")
  "e c" '((lambda() (interactive) (dired phga/emacsconf)) :which-key "edit emacs config")
  "e b" '((lambda() (interactive) (dired phga/books)) :which-key "edit books")
  "e d" '((lambda() (interactive) (dired phga/dotfiles)) :which-key "edit dotfiles")
  "e e" '((lambda() (interactive) (dired phga/sync)) :which-key "edit sync files")
  "e l" '((lambda() (interactive) (dired phga/lumi)) :which-key "edit lumi files")
  "e s" '((lambda() (interactive) (dired phga/semesterfiles)) :which-key "edit semester")
  "e f" '((lambda() (interactive) (dired phga/fresh)) :which-key "edit fresh files")
  "e o" '((lambda() (interactive) (dired phga/orgfiles)) :which-key "edit org files")
  "e g" '((lambda() (interactive) (dired phga/git)) :which-key "edit git files")
  "e w" '((lambda() (interactive) (dired phga/work)) :which-key "edit work files")
  "e p" '((lambda() (interactive) (dired phga/pw)) :which-key "edit passwords")

  "e m" '(gnus :which-key "edit mail")

  ;; remote tramp
  "e r" '(:ignore t :which-key "Edit Remote Files")
  "e r 1" '(phga/connect-to-nsv1 :which-key "edit files on nsv1")
  "e r h" '(phga/connect-to-helium :which-key "edit files on helium")
  "e r g" '(phga/connect-to-glou :which-key "edit files on glou")
  "e r p" '(phga/connect-to-pihole :which-key "edit files on pihole")
  "e r n" '(phga/connect-to-naz :which-key "edit files on naz")

  ;; files stuff
  "l" '(:ignore t :which-key "Load")
  "l c" '((lambda() (interactive) (load-file (concat user-emacs-directory "init.elc")))
          :which-key "load compiled config")

  "q" '(:ignore t :which-key "Quality :)")
  "q t" 'phga/cycle-themes
  "q =" 'phga/text-scale-adjust
  "q c" 'count-lines-words-region

  "t" '(:ignore t :which-key "Tools")
  "t a" 'org-agenda
  "t d" 'dictcc
  "t c" 'calc
  "t m" 'man
  "t k" 'calendar
  "t b" 'ibuffer
  "t r" '((lambda () (interactive) (counsel-rg nil default-directory))
          :which-key "Ripgrep in current folder")
  "t R" '((lambda (regex) (interactive "MFile Search: ")
            (shell-command (format "find * -type f -name '%s'" regex)))
          :which-key "Search for files in current folder")
  "t t" '(phga/run-terminal-here :which-key "Alacritty in current dir")
  "t s" 'phga/new-shell
  "t v" 'phga/new-vterm
  "t u" 'vundo
  "t c" 'org-capture
  "t i" '(:ignore t :which-key "Insert")
  "t i t" 'phga/insert-current-timestamp

  "m" '(:ignore t :which-key "Modes")
  "m s" 'flyspell-mode

  "r" '(:ignore t :which-key "Ripgrep")
  "r r" (lambda (regex) (interactive "MRegex: ") (rg regex "everything" "."))

  "?" '(:ignore t :which-key "Describe")
  "? v" 'describe-variable
  "? f" 'describe-function
  "? c" 'describe-char
  "? m" 'describe-mode
  "? k" 'describe-key
  "? F" 'describe-face
  )

(provide 'keybindings)
