;; For LSP install:
;; REQUIRES: go get golang.org/x/tools/gopls@latest
;; GO
(straight-use-package 'go-mode)
(setenv "GOPATH" "/home/phga/git/code/go")
(add-hook 'go-mode-hook #'lsp-deferred)

;; Bindings
(general-def
  :states 'normal
  :keymaps 'go-mode-map
  "=" 'gofmt)

(phgas-leader
  :states 'normal
  :keymaps 'go-mode-map
  "SPC c" '((lambda() (interactive) (phga/compile-go t)) :which-key "build & run")
  "SPC C" '((lambda() (interactive) (phga/compile-go nil)) :which-key "build"))

(defun phga/compile-go(build-and-run)
  "Either build and run or just build the go code"
  (interactive)
  (save-buffer)
  (shell-command "goimports -w *.go")
  (shell-command "go fmt")
  (revert-buffer :noconfirm t)
  (let* ((exe-path (concat default-directory
                           (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory default-directory)))))
         (compile-cmd "echo Building... && go build -v && echo Testing... && go test -v"))
    (when build-and-run
        (progn
          (setq exe exe-path)
          (setq killthis (file-name-nondirectory
                          (directory-file-name
                           (file-name-directory default-directory))))
          (setq run-after-compile t)))
    (compile compile-cmd)))

(defun phga/post-compilation (buf str)
  (when (null (string-match ".*exited abnormally.*" str))
    ;;no errors, make the compilation window go away in a few seconds
    (run-at-time
     "1 sec" nil 'kill-buffer
     (get-buffer-create "*compilation*"))
    (if (boundp 'run-after-compile)
        (progn (message "No Compilation Errors, running executable")
               (shell-command (concat "pkill " killthis " &> /dev/null") )
               ;; start-process-shell-command und call-... haben nicht funktioniert ):
               (start-process "NERD" "*ASYNC*" exe))
      (progn (message "No Compilation Errors!")))))

(add-to-list 'compilation-finish-functions
             'phga/post-compilation)

(provide 'l-go)
