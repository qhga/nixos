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

(provide 'l-go)
