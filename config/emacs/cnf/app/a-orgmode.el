
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: org-mode is required in essentials.el so we use the git instead of the shipped version ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ORG-DRILL
(straight-use-package 'org-drill)

;; HTMLIZE: html export for orgmode
(straight-use-package 'htmlize)

;; There was a long discussion on reddit that this package slows
;; down the redisplay optimizations of emacs
;; https://old.reddit.com/r/emacs/comments/p9yjgq/turn_off_fontlock_under_cursor/ha6zor6/
;; ORG-APPEAR: shows emphasis symbols when required
(straight-use-package 'org-appear)
(setq org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t
      org-appear-autokeywords t
      org-appear-inside-latex t)
(add-hook 'org-mode-hook 'org-appear-mode)

;; ORG-DOWNLOAD: insert screenshots on the fly
(straight-use-package 'org-download)
(require 'org-download)

;; ORG-TREE-SLIDE: org presentations
(straight-use-package 'org-tree-slide)

(org-tree-slide-simple-profile)
(setq org-tree-slide-slide-in-effect nil)
(add-hook 'org-tree-slide-mode-hook (lambda() (evil-insert-state) (evil-normal-state)))

;; Keybindings
(general-define-key
 :states '(normal emacs)
 :keymaps 'org-tree-slide-mode-map
 "<prior>" 'org-tree-slide-move-previous-tree
 "<next>" 'org-tree-slide-move-next-tree
 "<up>" 'org-tree-slide-move-previous-tree
 "<down>" 'org-tree-slide-move-next-tree)

(phgas-leader
  :states 'normal
  :keymaps 'org-mode-map
  "SPC m" '(:ignore t :which-key "Mode (Treeslide)")
  "SPC m p" 'org-tree-slide-mode)

;; ORG-PRESENT
;; (straight-use-package 'org-present)

;; (defun phga/org-present-start()
;;   (interactive)
;;   (org-present-big)
;;   (org-display-inline-images)
;;   (org-present-read-write))

;; (defun phga/org-present-stop()
;;   (interactive)
;;   )

;; (setq phga/org-present-is-active nil)

;; (defun phga/org-present-toggle()
;;   (interactive)
;;   (if phga/org-present-is-active
;;       (progn (org-present-quit)
;;              (setq phga/org-present-is-active nil))
;;     (progn (org-present)
;;            (setq phga/org-present-is-active t))))


;; (add-hook 'org-present-mode-hook 'phga/org-present-start)
;; (add-hook 'org-present-mode-hook 'phga/org-present-start)

;; (general-define-key
;;  :states '(normal emacs)
;;  :keymaps 'org-present-mode-keymap
;;  "<prior>" 'org-present-prev
;;  "<next>" 'org-present-next)

;; (phgas-leader
;;   :states 'normal
;;   :keymaps 'org-mode-map
;;   "SPC m" '(:ignore t :which-key "Mode (Present)")
;;   "SPC m p" 'phga/org-present-toggle)

;; OB-GO: Org babel support for go
(straight-use-package 'ob-go)
(with-eval-after-load 'ob-go (require 'l-go)) ;; enable highlighting, etc

;; ORG-REF: org citing
;; (straight-use-package 'org-ref)

;; EVIL-ORG: more org bindings
(straight-use-package 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(with-eval-after-load 'org-mode
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

;; (add-hook 'org-mode-hook (lambda () (interactive) (message "OPENED A NEW ORG FILE")))

;; OX-KOMA-LETTER: org export koma latex
;; (straight-use-package 'org)
;; (add-to-list 'load-path "/home/phga/.dotfiles/emacs/straight/repos/org/contrib/lisp/")
(add-to-list 'load-path "/home/phga/.dotfiles/config/emacs/local-pkgs")
(with-eval-after-load 'ox (require 'ox-koma-letter))

;; ORG-SRC: Load language specific configs when fontifying src blocks for languages which
;; configs are dynamically loaded and not present at the time of the fontification process
(defun phga/advice-load-config-for-org-src-block-language (lang start end)
  "Look in `load-path' for a file that provides the feature l-LANG.
LANG is the language used by `org-src-font-lock-fontify-block'.
START and END are not used by this advice"
  (ignore-errors (require (intern (concat "l-" lang)))))

(advice-add 'org-src-font-lock-fontify-block
            :before #'phga/advice-load-config-for-org-src-block-language)

(defvar phga/org-export-directory)
(defun phga/advice-create-and-set-org-export-directory (f extension
                                                          &optional subtreep pub-dir)
  "Create and set `phga/org-export-directory' which is used as PUB-DIR in F.
F is `org-export-output-file-name' SUBTREEP and EXTENSION are not
modified here."
  (let ((pub-dir (or pub-dir (and (boundp 'phga/org-export-directory)
                                  phga/org-export-directory))))
    (unless (file-directory-p pub-dir)
      (make-directory phga/org-export-directory))
    (apply f extension subtreep pub-dir '()))) ;; '() is required by apply

(advice-add 'org-export-output-file-name :around #'phga/advice-create-and-set-org-export-directory)

(setq phga/org-export-directory "./auto")

(evil-set-initial-state 'org-agenda-mode 'normal)
(add-to-list 'org-export-backends 'md)
(setq org-agenda-tags-column org-tags-column
      org-babel-min-lines-for-block-output 50
      org-confirm-babel-evaluate nil
      ;; Since ORG-Mode 9.1.9 functions inside of (file "FILENAME") are no longer
      ;; evaluated if the template list is prefixed with '
      ;; Solution: use ` to prefix list and ,(concat) to indicate req. evaluation
      org-directory "~/sync/mvtn/private/stc"
      org-capture-templates
      `(("i" "Idea"
         entry (file+headline ,(concat org-directory "/20220202-000000 prv.org") "Ideas")
         "* IDEA %?\n %T\n")
        ("n" "Note"
         entry (file+headline ,(concat org-directory "/20220202-000000 prv.org") "Notes")
         "* NOTE %?\n %T\n")
        ("t" "Todo"
         entry (file+headline ,(concat org-directory "/20220202-000000 prv.org") "Todos")
         "* TODO %?\n %T\n")
        ("r" "Read later"
         entry (file+headline ,(concat org-directory "/20220202-000000 prv.org") "Read later")
         "* %?\n %T\n"))

      org-default-notes-file (concat org-directory "/20220203-000000 refile.org")
      org-duration-format 'h:mm

      ;; Latex
      org-latex-listings 'minted
      ;; org-latex-listings nil
      org-latex-packages-alist '(("outputdir=auto" "minted"))
      org-latex-inputenc-alist '(("utf8" . "utf8x"))
      org-latex-prefer-user-labels t
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")

      ;; Export Config
      org-export-preserve-breaks nil
      org-export-with-email t
      org-export-with-sub-superscripts t
      org-export-in-background nil
      org-html-htmlize-output-type 'inline-css

      ;; REQUIRES: y -S plantuml graphviz
      org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar")

      ;; Visuals
      org-pretty-entities t
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-src-window-setup 'current-window
      org-use-sub-superscripts '{}
      org-ellipsis " ↷"
      org-cycle-separator-lines 0
      org-hide-emphasis-markers t

      org-startup-folded nil
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-image-actual-width nil

      org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil))

      ;; Tags
      org-tags-column 60
      org-fast-tag-selection-include-todo t
      org-fast-tag-selection-single-key 'expert

      ;; Todos
      org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (type "NEXT(n)")
        (type "MEET(m)")
        (type "IDEA(i)"))
      ;; '((type "TODO")
      ;;   ;; (type "DONE")
      ;;   (type "NEXT")
      ;;   (type "IDEA")
      ;;   (type "MEET")
      ;; (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
      ;; (sequence "IDEA(i)" "|" "TODO(t)")
      ;; )
      org-todo-keyword-faces
      '(("TODO" :inherit org-todo :weight bold)
        ("DONE" :inherit org-done :weight bold)
        ("NEXT" :weight bold)
        ("MEET" :weight bold)
        ("IDEA" :foreground "#cea7f0" :weight bold))
      org-use-fast-todo-selection t
      org-fontify-todo-headline nil
      org-fontify-done-headline nil)

;; leads to unwanted buffer visits on first clock-in
;; Probably because it tries to scan for clocks in all agenda files
;; org-agenda-files (list org-directory)

;; org-latex-listings 'listings

(font-lock-add-keywords
 'org-mode
 '(("^ *\\(-\\) " ; change '-' into a unicode bullet
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\(\\+\\) " ; change '+' into a unicode bullet
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; Get rid of the ,* in code blocks (visually)
(font-lock-add-keywords
 'org-mode
 '(("^ *\\(,\\*\\).*" ; change ',*' into *
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "*"))))))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; Scale of the latex fragments
(plist-put org-format-latex-options :scale 1.3)

;; Run/highlight code using babel in org-mode
(setq org-babel-load-languages
      '((emacs-lisp . t)
        (lisp . t)
        (python . t)
        (js . t)
        (java . t)
        (C . t)
        (sql . t)
        (calc . t)
        (perl . t)
        (shell . t)
        (octave . t)
        (plantuml . t)
        (matlab . t)
        (go . t))
      org-babel-octave-shell-command "octave -q")

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; export html with styles
(defvar org-style-css "~/sync/mvtn/setup/org.css")

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle org-style-css path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(setq org-html-htmlize-font-prefix "org-") ; default: "org-"

;; ELECTRIC PAIRS: Special pairs for org buffers
;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
(defvar org-electric-pairs '((?\' . ?\')) "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;; ORG-CLOCK
(setq org-clock-history-length 23
      org-clock-in-resume t
      org-drawers '("PROPERTIES" "LOGBOOK")
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist 'history
      org-clock-persist-query-resume nil
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t)

(org-clock-persistence-insinuate)

;; ORG-AGENDA
(defvar org-agenda-window-setup)
(setq org-agenda-window-setup 'current-window)

;; ORG-DOWNLOAD
(setq org-download-screenshot-file "~/sync/screenshots/tmp/orgcapture.png"
      org-download-screenshot-method "maim -k -s %s"
      org-download-image-attr-list '("#+ATTR_ORG: :width 600")
      org-download-annotate-function (lambda(link) ""))
(setq-default org-download-image-dir "./ORGPICS")

;; KEYBINDINGS
(general-def
  :states 'normal
  :keymaps 'org-mode-map
  ;; "TAB" 'org-cycle
  "=" 'phga/format-buffer
  )

(phgas-leader
  :states 'normal
  :definer 'minor-mode
  :keymaps 'org-src-mode
  "'" 'org-edit-src-exit
  )

(phgas-leader
  :states '(normal visual)
  :keymaps 'org-mode-map
  "'" 'org-edit-src-code
  "SPC -" 'org-insert-heading
  "SPC p" 'fill-paragraph
  "SPC i" '(:ignore t :which-key "Insert")
  "SPC i s" 'org-download-screenshot
  "SPC i t" 'org-todo
  "SPC i l" 'org-insert-link
  "SPC s l" 'org-store-link
  "SPC t" '(:ignore t :which-key "Toggle")
  "SPC t i" 'org-toggle-inline-images
  "SPC t l" 'org-latex-preview
  "SPC c" '(:ignore t :which-key "C Funktions (clock, execute)")
  "SPC c i" 'org-clock-in
  "SPC c o" 'org-clock-out
  "SPC c l" 'org-clock-in-last
  "SPC c c" 'org-babel-execute-src-block
  "SPC r" '(:ignore t :which-key "Remove")
  "SPC r r" 'org-babel-remove-result
  "SPC r a" (lambda () (interactive) (org-babel-remove-result-one-or-many t))
  "SPC o" 'org-open-at-point
  "SPC e" 'org-export-dispatch
  )

(provide 'a-orgmode)
