;; SAVEPLACE-PDF-VIEW: Restore the last place in a pdf before it was closed
(straight-use-package 'saveplace-pdf-view)
(save-place-mode 1)

;; (pdf-tools-install) wenn er die libpoppler.so ned findet > list
;; packages > mit i markieren > x neue version installieren und
;; pdf-tools-install Oder wenn die libpoppler.so neuer ist als die
;; verlangte (weil arch = superfast) dann simlink einfach die neuere
;; auf alte version /usr/lib
;; PDF-TOOLS: better pdf viewer
(straight-use-package 'pdf-tools)
(pdf-loader-install)
;; variables
(setq-default pdf-view-display-size 'fit-width)
(setq pdf-annot-activate-created-annotations t
      pdf-view-midnight-colors '("#efefef" . "#050505"))
;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode)

;; #############################################################################
;; PDF-TOOLS: Helper Functions (Evil Collection)
;; #############################################################################

(declare-function pdf-view-last-page "pdf-view")
(declare-function pdf-view-first-page "pdf-view")
(declare-function pdf-view-goto-page "pdf-view")
(declare-function pdf-view-previous-line-or-previous-page "pdf-view")
(declare-function pdf-view-next-line-or-next-page "pdf-view")

;; TODO: The following 2 functions are workarounds for
;; 'pdf-view-next-line-or-next-page' and
;; 'pdf-view-previous-line-or-previous-page' not playing well with
;; EVIL. The root cause should be found and fixed instead.
;; See https://github.com/emacs-evil/evil-collection/pull/137 for
;; details.
(defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
  "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
  (interactive "P")
  (if count
      (dotimes (_ count nil)
	      (pdf-view-next-line-or-next-page 1))
    (pdf-view-next-line-or-next-page 1)))

(defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
  "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
  (interactive "P")
  (if count
      (dotimes (_ count nil)
	      (pdf-view-previous-line-or-previous-page 1))
    (pdf-view-previous-line-or-previous-page 1)))

(defun evil-collection-pdf-view-goto-page (&optional page)
  "`evil' wrapper around `pdf-view-last-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-last-page)
    (image-eob)))

(defun evil-collection-pdf-view-goto-first-page (&optional page)
  "`evil' wrapper around `pdf-view-first-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-first-page)
    (image-bob)))

;; PDF-TOOLS: Rotate current page
;; https://emacs.stackexchange.com/questions/24738/how-do-i-rotate-pages-in-pdf-tools/24766#24766
(defun pdf-view--rotate (&optional counterclockwise-p page-p)
  "Rotate PDF 90 degrees.  Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
  ;; error out when pdftk is not installed
  (if (null (executable-find "pdftk"))
      (error "Rotation requires pdftk")
    ;; only rotate in pdf-view-mode
    (when (eq major-mode 'pdf-view-mode)
      (let* ((rotate (if counterclockwise-p "left" "right"))
             (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
             (page   (pdf-view-current-page))
             (pages  (cond ((not page-p)                        ; whole doc?
                            (format "1-end%s" rotate))
                           ((= page 1)                          ; first page?
                            (format "%d%s %d-end"
                                    page rotate (1+ page)))
                           ((= page (pdf-info-number-of-pages)) ; last page?
                            (format "1-%d %d%s"
                                    (1- page) page rotate))
                           (t                                   ; interior page?
                            (format "1-%d %d%s %d-end"
                                    (1- page) page rotate (1+ page))))))
        ;; empty string if it worked
        (if (string= "" (shell-command-to-string
                         (format (concat "pdftk %s cat %s "
                                         "output %s.NEW "
                                         "&& mv %s.NEW %s")
                                 file pages file file file)))
            (pdf-view-revert-buffer nil t)
          (error "Rotation error!"))))))

(defun pdf-view-rotate-clockwise (&optional arg)
  "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
  (interactive "P")
  (pdf-view--rotate nil (not arg)))

(defun pdf-view-rotate-counterclockwise (&optional arg)
  "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
  (interactive "P")
  (pdf-view--rotate :counterclockwise (not arg)))


;; #############################################################################
;; PDF-TOOLS: Keybindings
;; #############################################################################

(evil-set-initial-state 'pdf-view-mode 'normal)
(general-def
  :states '(normal visual emacs)
  :keymaps 'pdf-view-mode-map
  ;; --- Motion
  "j" 'evil-collection-pdf-view-next-line-or-next-page
  "k" 'evil-collection-pdf-view-previous-line-or-previous-page
  "J" 'pdf-view-next-page
  "K" 'pdf-view-previous-page
  "l" 'image-forward-hscroll
  "h" 'image-backward-hscroll
  "gg" 'evil-collection-pdf-view-goto-first-page
  "G" 'evil-collection-pdf-view-goto-page
  ;; --- Scaling
  "=" 'pdf-view-enlarge
  "-" 'pdf-view-shrink
  "0" 'pdf-view-scale-reset
  "H" 'pdf-view-fit-height-to-window ; evil-image has "H"
  "P" 'pdf-view-fit-page-to-window
  "W" 'pdf-view-fit-width-to-window ; evil-image has "W"
  "R" 'pdf-view-rotate-clockwise
  ;; --- Annotation
  "<C-down-mouse-1>" 'pdf-view-mouse-extend-region
  "<down-mouse-1>" 'pdf-view-mouse-set-region
  "a d" 'pdf-annot-delete
  "a a" 'pdf-annot-add-highlight-markup-annotation
  "a l" 'pdf-annot-list-annotations
  "a m" 'pdf-annot-add-markup-annotation
  "a o" 'pdf-annot-add-strikeout-markup-annotation
  "a s" 'pdf-annot-add-squiggly-markup-annotation
  "a t" 'pdf-annot-add-text-annotation
  "a u" 'pdf-annot-add-underline-markup-annotation
  ;; --- Misc
  "o" 'pdf-outline
  "y" 'pdf-view-kill-ring-save
  "gr" 'revert-buffer
  "/" 'pdf-occur
  "x" 'pdf-view-auto-slice-minor-mode
  "m" 'pdf-view-midnight-minor-mode
  "L" 'org-store-link)

(general-def
  :states 'normal
  :keymaps 'pdf-outline-buffer-mode-map
  "RET" 'pdf-outline-follow-link
  "q" 'quit-window)

(general-def
  :states 'normal
  :keymaps 'pdf-occur-buffer-mode-map
  "RET" 'pdf-occur-view-occurrence
  "q" 'tablist-quit)

(provide 'a-pdf)