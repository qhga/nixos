(straight-use-package '(mvtn :host github :repo "f1rstperson/mvtn.el"))

(with-eval-after-load "mvtn"
  (setq mvtn-note-directories
        '((:dir "~/sync/mvtn/private" :name "prv" :structure
           ((:dir "flt" :datetree t) ;; Floating
            ;; BUG: Cannot be excluded by the mvtn-excluded-directories
            ;; (:dir "log" :datetree t)
            (:dir "sci" :datetree t) ;; Scientific
            (:dir "stc" :datetree nil))) ;; Static
          (:dir "~/sync/mvtn/study" :name "stu" :structure
           ((:dir "thi" :datetree t)))
          (:dir "~/sync/mvtn/work" :name "wrk" :structure
           ((:dir "bs1" :datetree t)
            (:dir "lw" :datetree t)
            (:dir "glou" :datetree t))))
        ;; mvtn-note-directory "~/tmp/mvtn-test"
        ;; mvtn-note-directory "~/.dotfiles/emacs/straight/repos/mvtn.el/test/test-notes"
        mvtn-default-file-extension "org"
        mvtn-search-function 'mvtn-search-full-text-rg
        mvtn-list-files-function 'mvtn-list-files-function-find
        ;; mvtn-list-files-function 'mvtn-list-files-function-native
        mvtn-excluded-directories '(".git" ".svn" "ltximg" "ORGPICS" "auto")
        mvtn-cv-enable t
        mvtn-journal-new-daily-title "Log for %Y-%m-%d"
        mvtn-journal-dir "prv/log")

  ;; (mvtn-journal-autojournal-set-feature 'git-commit t)
  ;; (mvtn-journal-autojournal-set-feature 'note-changed t)

  (setq-default olivetti-body-width 102)

  (add-hook 'mvtn-minor-mode-hook 'olivetti-mode)
  (require 'mvtn-link-buttons))

;; Workaround because RET cannot be overwritten with general-def (Button push)
(with-eval-after-load 'mvtn-tag-addons
  (defun mvtn--tag-file-list-action (button)
    (mvtn-tag-file-list-open-keep-focus)))

(phgas-leader
  :states '(normal visual emacs)
  :keymaps 'override
  "n d" 'mvtn-jump-current-year-directory
  "n n" 'mvtn-open-note
  "n N" 'mvtn-new-note
  "n r" 'mvtn-rename-current-file
  "n /" 'mvtn-search-full-text
  "n l" 'mvtn-insert-link
  "n o" 'mvtn-follow-link-at-point
  "n b" 'mvtn-search-backlinks
  "n t" 'mvtn-tag-file-list
  )

(general-def
  :states 'normal
  :keymaps 'mvtn-tag-file-list-mode-map
  "g r" 'revert-buffer
  "q" 'quit-window
  )


(provide 'a-mvtn)