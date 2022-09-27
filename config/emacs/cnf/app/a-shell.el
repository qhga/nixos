;; VARIABLES
(setq shell-command-switch "-c")
(setenv "EMACS_SHELL" "shell")

(defun phga/get-face-color (face attr)
  (when (facep face)
    (let ((color (face-attribute face attr)))
      (if (string-match-p "#[0-9a-fA-F]\\{6\\}" color)
          color
        (let ((rgb-val (color-name-to-rgb color)))
          (when rgb-val
            (upcase (apply 'color-rgb-to-hex
                           (append rgb-val '(2))))))))))

;; Vectors do not evaluate things on assignment
;; That hack took me a while... (Still not an elisp expert)
(require 'term) ;; required for term-color to be present
(add-hook 'shell-mode-hook
          (lambda ()
            (ansi-color-map-update 'ansi-color-names-vector
                       `[,(or (phga/get-face-color 'term-color-black :foreground) "black")
                         ,(or (phga/get-face-color 'term-color-red :foreground) "red")
                         ,(or (phga/get-face-color 'term-color-green :foreground) "green")
                         ,(or (phga/get-face-color 'term-color-yellow :foreground) "yellow")
                         ,(or (phga/get-face-color 'term-color-blue :foreground) "blue")
                         ,(or (phga/get-face-color 'term-color-magenta :foreground) "magenta")
                         ,(or (phga/get-face-color 'term-color-cyan :foreground) "cyan")
                         ,(or (phga/get-face-color 'term-color-white :foreground) "white")])))


;; Influenced by https://stackoverflow.com/a/16779511
(defun phga/new-shell ()
  (interactive)
  "Creates a new shell in the current window and renames in with a random name"
  (let* ((curr-window (get-buffer-window (current-buffer)))
         (adjectives '("last" "worthless" "labored" "sordid" "puzzled"
                       "tremendous" "clammy" "present" "outrageous" "raspy"
                       "nimble" "rampant" "sour" "angry" "colorful" "early"
                       "alert" "quixotic" "husky" "shrill" "disturbed" "damaged"
                       "breakable" "painful" "gainful" "delicate" "unruly"
                       "huge" "spooky" "incompetent" "dependent" "long"
                       "skillful" "troubled" "longing" "second-hand" "dazzling"
                       "wary" "resonant" "guarded" "abiding" "crowded" "eatable"
                       "grandiose" "alike" "unsuitable" "shut" "ambitious"))
         (shell-name (generate-new-buffer-name
                      (concat "*" (nth (random (length adjectives))
                                       adjectives) "-shell*"))))
    (generate-new-buffer shell-name)
    (set-window-dedicated-p curr-window nil)
    (set-window-buffer curr-window shell-name)
    (shell shell-name)))


;; NEW BASH COMPLETION
;; Make sure to add:
;; export HISTCONTROL=ignoreboth
;; to the .bashrc that is sourced by shell-mode
;; TODO: both basically do the same but native-complete does not show aliases
(straight-use-package 'native-complete)
(straight-use-package 'bash-completion)
(with-eval-after-load 'shell
  (native-complete-setup-bash)
  (bash-completion-setup))

;; SHX: Advanced shell
(straight-use-package 'shx)
(setq shx-max-output 1024
      shx-img-height 250
      shx-show-hints nil)
(shx-global-mode 1)

(defun shx-cmd-unkeep (_arg)
  "(SAFE) Remove a kept command from `shx-kept-commands'."
  (let ((unkeep (completing-read
                 "What to unkeep: "
                 (mapcar (lambda (el)
                           (car el)) shx-kept-commands))))
    (setq shx-kept-commands
          (delq (assoc unkeep shx-kept-commands) shx-kept-commands))
    (shx-insert "Removed " unkeep "from shx-kept-commands\n")))

(provide 'a-shell)