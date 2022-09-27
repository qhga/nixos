(defun phga/aetts--get-color (face attr)
  (when (facep face)
    (let ((color (face-attribute face attr)))
      (if (string-match-p "#[0-9a-fA-F]\\{6\\}" color)
          color
        (let ((rgb-val (color-name-to-rgb color)))
          (when rgb-val
            (upcase (apply 'color-rgb-to-hex
                           (append rgb-val '(2))))))))))

(defun phga/bright-color (col)
  (format "#%X"
          (min 16777215
               (* 1.0005 (string-to-number (substring col 1 nil) 16)))))

;; (phga/bright-color "#f36c60")
;; MAYB: use ansi-color-map
;; TODO: check if term colors exist, if not use those below and set them!
(defun phga/apply-emacs-theme-to-system ()
  "Creates a .Xresources file with the colors of the currently active dark color theme
and adjusts colors in configs for: Rofi, Dunst, Qutebrowser"
  (interactive)
  ;; Hacky-Smacky...
  (defface term-color-black nil "")
  (defface term-color-red nil "")
  (defface term-color-green nil "")
  (defface term-color-yellow nil "")
  (defface term-color-blue nil "")
  (defface term-color-magenta nil "")
  (defface term-color-cyan nil "")
  (defface term-color-white nil "")
  ;; Get colors from theme specification
  (let* ((xres-file (expand-file-name "~/.dotfiles/xserver/.Xresources"))
         (rofi-file (expand-file-name "~/.dotfiles/rofi/flat.rasi"))
         (qute-file (expand-file-name "~/.dotfiles/qutebrowser/config/config.py"))
         (dunst-file (expand-file-name "~/.dotfiles/dunst/dunstrc"))
         (background (phga/aetts--get-color 'default :background))
         (foreground (phga/aetts--get-color 'default :foreground))
         (err (phga/aetts--get-color 'error :foreground))
         (war (phga/aetts--get-color 'warning :foreground))
         (suc (phga/aetts--get-color 'success :foreground))
         ;; (info (phga/aetts--get-color 'compilation-info :foreground))
         (black (phga/aetts--get-color 'term-color-black :foreground))
         (red (phga/aetts--get-color 'term-color-red :foreground))
         (green (phga/aetts--get-color 'term-color-green :foreground))
         (yellow (phga/aetts--get-color 'term-color-yellow :foreground))
         (blue (phga/aetts--get-color 'term-color-blue :foreground))
         (magenta (phga/aetts--get-color 'term-color-magenta :foreground))
         (cyan (phga/aetts--get-color 'term-color-cyan :foreground))
         (white (phga/aetts--get-color 'term-color-white :foreground))
         (bg-alt (format "#%X"
                         (min 16777215 (+ 1052688 (string-to-number
                                                   (substring background 1 nil) 16)))))
         (bg-border (format "#%X"
                            (min 16777215 (+ 2236962 (string-to-number
                                                      (substring background 1 nil) 16)))))
         (header "! Term settings (st)
*.font:       -UNKN-Ttyp0-normal-normal-normal-*-17-*-*-*-m-*-iso10646-1
*.termname:   st-256color
*.borderpx:   8
! Colors"))

    ;; Write new colors to .Xresources file
    (when (file-exists-p xres-file)
      (rename-file xres-file (concat xres-file ".bak") t))
    (write-region (format "%s
! Normal colors
*.black:      %s
*.red:        %s
*.green:      %s
*.yellow:     %s
*.blue:       %s
*.magenta:    %s
*.cyan:       %s
*.white:      %s
! Bright colors
*.b_black:    %s
*.b_red:      %s
*.b_green:    %s
*.b_yellow:   %s
*.b_blue:     %s
*.b_magenta:  %s
*.b_cyan:     %s
*.b_white:    %s
! Special colors
*.error:      %s
*.warning:    %s
*.success:    %s
*.info:       %s
*.background: %s
*.bg_alt:     %s
*.foreground: %s"
                          header
                          black red green yellow blue magenta cyan foreground
                          bg-alt red green yellow blue magenta cyan foreground
                          ;; (phga/bright-color black) (phga/bright-color red)
                          ;; (phga/bright-color green) (phga/bright-color yellow)
                          ;; (phga/bright-color blue) (phga/bright-color magenta)
                          ;; (phga/bright-color cyan) (phga/bright-color foreground)
                          err war suc yellow background bg-border foreground)
                  nil xres-file)
    (shell-command (concat "xrdb " xres-file))
    ;; Rofi
    (shell-command (format "sed -i 's/c-accent: .*/c-accent: %s;/' %s" cyan rofi-file))
    (shell-command (format "sed -i 's/c-border: .*/c-border: %s;/' %s" bg-border rofi-file))
    (shell-command (format "sed -i 's/c-bg: .*/c-bg: %s;/' %s" bg-alt rofi-file))
    (shell-command (format "sed -i 's/c-fg: .*/c-fg: %s;/' %s" foreground rofi-file))
    ;; Qutebrowser
    (shell-command (format "sed -i 's/^bgc = .*/bgc = \"%s\"/' %s" background qute-file))
    (shell-command (format "sed -i 's/^bgc_alt = .*/bgc_alt = \"%s\"/' %s" bg-alt qute-file))
    (shell-command (format "sed -i 's/^fgc = .*/fgc = \"%s\"/' %s" foreground qute-file))
    (shell-command (format "sed -i 's/^errorc = .*/errorc = \"%s\"/' %s" err qute-file))
    (shell-command (format "sed -i 's/^warningc = .*/warningc = \"%s\"/' %s" war qute-file))
    (shell-command (format "sed -i 's/^selectedc = .*/selectedc = \"%s\"/' %s" cyan qute-file))
    (shell-command (format "sed -i 's/^statusc = .*/statusc = \"%s\"/' %s" green qute-file))
    (shell-command (format "sed -i 's/^tabsindicatorc = .*/tabsindicatorc = \"%s\"/' %s" white qute-file))
    (shell-command (format "sed -i 's/^indicatorc = .*/indicatorc = \"%s\"/' %s" white qute-file))
    ;; Dunst
    (shell-command (format "sed -i 's/background = \".*/background = \"%s\"/' %s" bg-alt dunst-file))
    (shell-command (format "sed -i 's/foreground = \".*\"$/foreground = \"%s\"/' %s" foreground dunst-file))
    (shell-command (format "sed -i 's/foreground = \".*\" #crit$/foreground = \"%s\" #crit/' %s" err dunst-file))
    (call-process-shell-command (concat "pkill dunst && (dunst &) && "
                                        "$HOME/.dotfiles/lemonbar/start.sh &&"
                                        "notify-send -u normal 'Theme changed (:'"))))


(defun phga/pass-gen ()
  "Generate a password with length 20, if cursor is on a `n' don't use special chars"
  (interactive)
  ;; ?LETTER is used to compare char with smth else
  (let* ((cset (if (and (char-after) (= (char-after) ?n))
                   (progn (delete-char 1)
                          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                 "abcdefghijklmnopqrstuvwxyzäöüABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ0123456789€ß!@#$%^&*()[]{}/|;.><:; -_"))
         (n 20)
         (len (length cset)))
    (dotimes (_ n)
      (insert (elt cset (random len))))
    ))

(defun phga/pass-find-pw ()
  "Leave all buffers containing PW open, close rest"
  (interactive)
  (let ((pw (read-string "Search: ")))
    (dolist (pwfile (directory-files-recursively "~/.password-store" ".*\\.gpg"))
      (find-file pwfile)
      (unless (search-forward pw nil t)
        (kill-this-buffer)))))

(defun phga/jump-to-closing-paren()
  "Jump to the next closing paren."
  (interactive)
  (progn
    (backward-up-list)
    (evil-jump-item)))

(defun phga/run-terminal-here ()
  (interactive "@")
  (shell-command (concat "TERM_FROM_EMACS=true st -e bash -c 'cd " default-directory "; bash' > /dev/null 2>&1 & disown ") nil nil))

;;; https://gist.github.com/mads-hartmann/3402786
(defun toggle-maximize-buffer ()
  "Maxmize current buffer, when called again, restore previous buffer setup"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun count-lines-words-region (start end)
  "Print number of lines words and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d words, %d characters"
 	         (count-lines start end)
           (count-words-region start end)
           (- end start)))

;; Workaround for new projectile versions so that libs that rely on (project-root) still work
;; https://github.com/hlissner/doom-emacs/issues/3269 Emacs < 27
;; (defun project-root (project)
;;   (car (project-roots project)))
(defun phga/format-buffer()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun phga/format-xml-buffer ()
  (interactive)
  ;; (save-excursion) does not work here, bc REPLACE
  ;; sets point to 0 and buffer content is removed
  ;; Save current point in file
  (let ((p (point)))
    (shell-command-on-region (point-min) (point-max) "xmllint --format -"
                             (current-buffer) t)
    ;; Go back to previous point
    (goto-char p)))

(defun phga/format-json-buffer ()
  (interactive)
  (let ((p (point)))
    (shell-command-on-region (point-min) (point-max)
                             "python -m json.tool --no-ensure-ascii --indent 2 -"
                             (current-buffer) t)
    ;; Go back to previous point
    (goto-char p)))

(defun phga/find-file-sudo ()
  (interactive)
  (let ((file (expand-file-name (buffer-file-name))))
    (if (string-match "^/sudo:" file)
        (user-error "File already opened with sudo")
      (find-file (concat "/sudo::" file)))))

;; https://emacs.stackexchange.com/a/13096
(defun phga/dir-locals-reapply ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun phga/dir-locals-reapply-all ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (phga/dir-locals-reapply))))))

(defun phga/load-all-configs ()
  (interactive)
  (mapc (lambda (cf)
          (message (concat "Processing: " cf))
          (load cf)) (directory-files-recursively (concat user-emacs-directory "cnf/") ".*\\.el")))

(defun phga/insert-current-timestamp ()
  (interactive)
  (insert (format-time-string " %Y-%m-%dT%H:%M" (current-time))))

(defun phga/sanitize-files-in-folder(folder)
  (interactive)
  (message "Implement this")
  ;; %s/ /_/g
  ;; %s/-/_/g
  ;; %s/__+/_/g
  ;; %s/\([A-Z]+\)/\L\1/g -> ToLower
  )

(provide 'functions)