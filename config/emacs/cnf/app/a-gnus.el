;; ----------------------------------------------------------------------
;; accounts
;; ----------------------------------------------------------------------
(require 'gnus-group)

(if (file-exists-p "~/sync/app_data/emacs/mail/gnus-accounts.el")
    (load "~/sync/app_data/emacs/mail/gnus-accounts.el")
  (warn (concat "No gnus-accounts file found in ~/sync/app_data/emacs/mail/gnus-accounts.el. "
                "Accounts will not be available")))

;; ----------------------------------------------------------------------
;; general behaviour
;; ----------------------------------------------------------------------

(setq
 ;; I don't like my $HOME getting messed up more than it needs to be.
 gnus-directory "~/.emacs.d/News"
 gnus-startup-file "~/.emacs.d/newsrc"
 ;; The gcc header can be used to put e-mail into other folders upon being
 ;; sent. Obviously I don't want e-mails in "Sent" to be unread by default.
 gnus-gcc-mark-as-read t
 ;; I'm not 100% sure what this does. But apparently, it enables gnus to
 ;; fetch articles in advance some of the time.
 gnus-asynchronous t)

;; Don't save newsrc; I don't care about compatibility with other news readers
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)

;; Encryption
(setq mml-secure-smime-sign-with-sender t)

;; ----------------------------------------------------------------------
;; caching
;; ----------------------------------------------------------------------

;; Gnus has two different ways of doing caching. The "regular" cache, enabled by
;; `gnus-use-cache' is mostly intended for manual use. So if you want to
;; explicitly save an article to disk, you would use that. This cache stores
;; things based on marks. What marks enter the cache is determined by
;; `gnus-cache-enter-articles' and what exits the cache by
;; `gnus-cache-remove-articles'. So, by default you can use
;; `gnus-summary-tick-article-forward' to manually cache something.
(setq gnus-use-cache t)

;; The gnus "agent" is the other, more modern and automated way of doing
;; caching. It is supposed to run mostly invisibly without user interaction.
(setq gnus-agent t)

;; To actually make it work as expected and enter all articles you fetch
;; automatically to it's cache, you use this incantation:
(add-hook 'gnus-select-article-hook 'gnus-agent-fetch-selected-article)


;; ----------------------------------------------------------------------
;; archiving
;; ----------------------------------------------------------------------

;; Gnus archives messages in several ways: First, it automatically archives
;; every e-mail you sent into `gnus-message-archive-group'. Second, it will
;; archive everything marked as "expired" to `nnmail-expiry-target' if it is
;; older than `nnmail-expiry-wait' days. In order to have every mail you read
;; marked as "expired" by default, I set (auto-expire . t) for the relevant
;; accounts in `gnus-parameters'.

;; I use nnmaildir as a backend for archiving. This is because I keep that
;; maildir on my nextcloud instance as a "backup" solution. In order to not get
;; sync-conflicts between computers though, it helps to have every mail as a
;; separate file. If you are concerned about inode usage (which you probably
;; aren't, check with \"df -h\"), you may want to the default nnfolder
;; instead. Additionally, you will need to create the folder to the maildir
;; yourself and add an entry like this to your `gnus-secondary-select-methods':
;;
;; (nnmaildir "archive"
;;            (directory "~/docs/Mail/archive")
;;            (get-new-mail nil))

;; (setq gnus-message-archive-group (format-time-string "nnmaildir+archive:unsorted.sent.%Y")

;;       ;; wait for N days before expiring articles
;;       nnmail-expiry-wait         365

;;       ;; This is just a (imo) sensible default. I change this variable on a
;;       ;; per-account basis using `gnus-parameters'.
;;       nnmail-expiry-target (lambda (groupname) (concat "nnmaildir+archive:unsorted.expired."
;;                                                        (fp/expiry-mail-date))))

;; (defun fp/expiry-mail-date ()
;;   "If run in buffer containing valid email-headers, this will
;; return the year in the date header of that mail as a string. If
;; that fails, it will return the current year. Useful to use for a
;; `nnmail-expiry-target'"
;;   (condition-case nil
;;       (save-excursion
;;         (goto-char (point-min))
;;         (format-time-string "%Y" (mail-header-parse-date
;;                                   (mail-header 'date (mail-header-extract)))))
;;     (error (format-time-string "%Y" (current-time)))))


;; ----------------------------------------------------------------------
;; group buffer
;; ----------------------------------------------------------------------

;; To categorize groups, we can use "topics". All topic actions in the group
;; buffer start with the prefix "T". Personally, I use topics create a
;; traditional e-mail client interface where I have the folders of my different
;; e-mail accounts grouped together by account. The alignment of these groups
;; gets saved to your newsrc.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; I like to sort by rank first, alphabet second. You can hit C-c C-s to apply
;; this sorting.
(setq gnus-group-sort-function
      '(gnus-group-sort-by-unread
        gnus-group-sort-by-alphabet
        gnus-group-sort-by-rank))

;; Visuals
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; ----------------------------------------------------------------------
;; summary buffer
;; ----------------------------------------------------------------------

;; --- formatting of entries ---
(setq gnus-summary-line-format
      (concat
       "%R%U%z"            ; cached, read, score
       "%-16,16"           ; next column
       "&user-date;"       ; date in the format of `gnus-user-date-format-alist'
       "  "                ; next column
       "%4L:%-30,30f"      ; "length in lines":"'from' or 'to' header"
       "  "                ; next column
       "%B"                ; tree style threads. see `gnus-sum-thread-tree-*'
       "%S"                ; subject
       "\n"                ; end
       ))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (t . "%Y-%m-%d %H:%M")))

;; 32 the char for a space
(setq gnus-replied-mark 32
      gnus-forwarded-mark 32)

(setq gnus-sum-thread-tree-false-root "─┬> "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─> "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "└─> "
      gnus-sum-thread-tree-vertical "│")

;; --- starting position ---
(setq gnus-summary-goto-unread nil)
(add-hook 'gnus-summary-prepared-hook (lambda () (end-of-buffer) (previous-line)))

;; --- other ---
(setq gnus-auto-select-first nil)
(setq gnus-summary-mode-line-format "%p [current: %A, unread: %Z]")
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-sort-by-date)

;; ----------------------------------------------------------------------
;; message buffer
;; ----------------------------------------------------------------------

;; --- use html mail only if necessary
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; Disables display of images by default.  Call `gnus-article-show-images' when
;; needed
(setq gnus-inhibit-images t)

;; ----------------------------------------------------------------------
;; dired interaction
;; ----------------------------------------------------------------------

;; This mostly just allows you to attach files from dired to a message by
(turn-on-gnus-dired-mode)

;; ----------------------------------------------------------------------
;; sending mail
;; ----------------------------------------------------------------------

;; This determines how gnus formats the original message with
;; `gnus-summary-reply-with-original' and friends.
(setq message-cite-style
      '((message-cite-function 'message-cite-original)
        (message-citation-line-function 'message-insert-formatted-citation-line)
        (message-cite-reply-position 'above)
        (message-yank-prefix "> ")
        (message-yank-cited-prefix ">")
        (message-yank-empty-prefix ">")
        (message-citation-line-format "On %D %R %p, %N wrote:")))

;; The simplest (imo) way to do message templating is to just compose something
;; in message mode, save that as a file and then as a bookmark. So i just invent
;; the ".message" extension to open up in message mode for this purpose.
(add-to-list 'auto-mode-alist (cons "\\.message\\'" 'message-mode))

;; ----------------------------------------------------------------------
;; viewing mail
;; ----------------------------------------------------------------------

;; Colors in rendered html look ugly and can be distracting; So i turn them
;; off. If you want to see the content as it was intended, use
;; `gnus-article-browse-html-article' to open it in your default browser.
(setq shr-use-colors nil)

;; Enter mode in Normal-State
(evil-set-initial-state 'gnus-group-mode 'normal)
(evil-set-initial-state 'gnus-summary-mode 'normal)
(evil-set-initial-state 'gnus-article-mode 'normal)
(evil-set-initial-state 'gnus-server-mode 'normal)
;; Keybindings
(general-def
  :states 'normal
  :keymaps 'gnus-group-mode-map
  "q" (lambda () (interactive) (gnus-save-newsrc-file) (quit-window))

  "o"   'gnus-group-select-group
  "RET" (lambda () (interactive) (gnus-group-select-group 100))

  "x" 'gnus-group-kill-group
  "p" 'gnus-group-yank-group

  "m" 'gnus-group-mark-group
  "u" 'gnus-group-unmark-group
  "U" 'gnus-group-unmark-all-groups
  "M" 'gnus-group-mark-buffer

  "s" 'gnus-group-apropos
  "S" 'gnus-group-description-apropos

  "r" 'gnus-group-get-new-news
  "R" (lambda () (interactive) (gnus-group-get-new-news '(4)))

  "^" 'gnus-group-enter-server-mode

  ;; "/" 'gnus-group-make-nnir-group
  "/" 'gnus-group-read-ephemeral-search-group

  ;; unbind
  "<tab>" (lambda () (interactive) (message "HELO"))
  ;; "<backtab>" 'gnus-topic-unindent
  )

(general-def
  :states 'normal
  :keymaps 'gnus-summary-mode-map
  "q" 'gnus-summary-exit
  "Q" 'gnus-summary-exit-no-update

  "RET" 'gnus-summary-scroll-up

  "r" 'gnus-summary-wide-reply
  "R" 'gnus-summary-wide-reply-with-original
  "f" 'gnus-summary-mail-forward
  "c" 'gnus-summary-mail-other-window

  "m" 'gnus-summary-mark-as-processable
  "u" 'gnus-summary-unmark-as-processable
  "U" 'gnus-summary-unmark-all-processable
  "x" 'gnus-summary-universal-argument

  "o"  'gnus-summary-put-mark-as-read-next
  "O"  'gnus-summary-put-mark-as-unread-next
  "e"  'gnus-summary-put-mark-as-expirable
  "D" 'gnus-summary-delete-article

  "gr" (lambda () (interactive) (gnus-summary-rescan-group 100))

  "i" 'gnus-summary-isearch-article
  "/" 'evil-search-forward
  "n" 'evil-search-next
  "si" 'gnus-summary-increase-score
  "sl" 'gnus-summary-lower-score
  )

(general-def
  :states 'normal
  :keymaps 'gnus-server-mode-map
  "q"   'gnus-server-exit
  "RET" 'gnus-server-read-server

  ;; online status
  "C" 'gnus-server-close-server
  "D" 'gnus-server-deny-server
  "L" 'gnus-server-offline-server
  "O" 'gnus-server-open-server

  "r" 'gnus-server-regenerate-server
  "x" 'gnus-server-kill-server
  "s" 'gnus-server-scan-server
  "p" 'gnus-server-yank-server

  "a a" 'gnus-agent-add-server
  "a r" 'gnus-agent-remove-server
  )

(general-def
  :states 'normal
  :keymaps 'gnus-article-mode-map
  "q" 'gnus-summary-expand-window)

(phgas-leader
  :states '(normal emacs)
  :keymaps 'message-mode-map
  "SPC e" 'mml-secure-message-encrypt-pgp
  "SPC a" 'mml-attach-file
  "SPC p" 'fill-paragraph
  )

(provide 'a-gnus)