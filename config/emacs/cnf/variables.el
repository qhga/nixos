(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(setq make-backup-files nil
      create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq auto-revert-interval 1            ; Refresh buffers fast
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      initial-major-mode 'org-mode
      recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore)        ; Quiet

(setq mouse-yank-at-point t)

(setq mode-require-final-newline nil)
;; Should help with reeeeally long lines e.g. json
;; there is also (setq-default bidi-display-reordering nil)
;; which is apparently not recommended by dev Eli because
;; it was never intended to be nil (could still improve things tho)
(setq-default bidi-paragraph-direction 'left-to-right)
;; gnupg
(require 'epa-file)
(push '("\\.gpg\\'" . epa-file-enable) auto-mode-alist)
;; only use the encrypted version of .authinfo
(setq auth-sources '("~/.authinfo.gpg"))
;; (setq-default epa-file-encrypt-to "philipg@posteo.de")


;; save unsaved buffers before killing frame
(add-hook 'delete-frame-functions 'save-some-buffers)
(add-hook 'kill-emacs-hook 'save-some-buffers)

;; Width used to draw the column indicator + used in fill-paragraph
(setq-default fill-column 90)

(provide 'variables)
