(straight-use-package 'auctex)
;; variables
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t
      TeX-PDF-mode t
      TeX-DVI-via-PDFTeX t
      LaTeX-item-indent 0
      ispell-dictionary "german")

(defun phga/revert-latex-document-buffer (pdf)
  (let* ((parts (split-string pdf "/"))
         (fname (car (last parts 1)))
         (pl (nbutlast parts 1))
         (dir (string-join pl "/"))
         (file (concat dir "/auto/" fname)))
    (message (concat "Reverting file: " file))
    (TeX-revert-document-buffer file)))

(add-hook 'TeX-after-compilation-finished-functions #'phga/revert-latex-document-buffer)


;; Sane defaults to keep the working dir as clean as possible
;; Also add a .dir-locals.el file with the following content:
;; ((nil . ((TeX-master . "/home/phga/sync/docs/study/BA/proposal/proposal.tex"))))
;; The line below does only work with the actual values not stringp
;; (push '(TeX-master . "/mein/toller/string") safe-local-variable-values)

;; (setq-default TeX-command-extra-options "-output-directory=auto -shell-escape")
;; setq-default does for whatever reason only set outpud-dir but not shell-escape
;; FKING TeX-style-hooks kept overwriting the vars set in LaTeX-mode-hook
;; After a bit of digging around, I found this hook which sounded right...
(add-hook 'TeX-auto-apply-hook #'phga/set-fking-tex-options)
(add-hook 'LaTeX-mode-hook #'phga/set-fking-tex-options)

(defun phga/set-fking-tex-options ()
  (setq-local TeX-command-extra-options
        "-output-directory=auto -shell-escape -synctex=1"))

(add-hook 'LaTeX-mode-hook (lambda ()
                             ;; Remove Biber entry from list
                             (assq-delete-all (car (assoc "Biber" TeX-command-list)) TeX-command-list)
                             (assq-delete-all (car (assoc "Glossaries" TeX-command-list)) TeX-command-list)
                             ;; Add new Biber entry to list
                             (add-to-list 'TeX-command-list
                                          '("Biber" "biber --output-directory auto --input-directory auto %s"
                                            TeX-run-Biber nil t
                                            :help "Run Biber")
                                          t)
                             (add-to-list 'TeX-command-list
                                          '("Glossaries" "makeglossaries -d auto %s"
                                            TeX-run-command nil
                                            (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode)
                                            :help "Run makeglossaries to create glossary file")
                                          t)
                             ))

;; hooks
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; keybindings
(phgas-leader
  :states 'normal
  :keymaps 'LaTeX-mode-map
  "SPC c" (lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file))
  "SPC b" (lambda () (interactive) (save-buffer) (TeX-command "Biber" 'TeX-master-file))
  "SPC p" 'fill-paragraph
  "=" 'align-current
  )

(provide 'l-tex)
