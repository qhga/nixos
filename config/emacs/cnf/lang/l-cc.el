(straight-use-package 'ccls)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
;;hooks

;; HIER IST IRGEND EIN FEHLER
;; (defun phga/run()
;;   (interactive)
;;   (unless (boundp 'phga/compile-command) (phga/set-compile-command))
;;   (save-buffer 'no-confirm)
;;   (setq file
;;         (file-name-sans-extension
;;          (buffer-file-name (current-buffer))))
;;   (compile phga/compile-command))

;; (defun phga/set-compile-command()
;;   (interactive)
;;   (set (make-local-variable 'phga/compile-command)
;;        (let ((file (file-name-nondirectory buffer-file-name)))
;;          (cond ((derived-mode-p 'c++-mode) ;; CPP Mode
;;                 (format "%s -o %s.o %s %s"
;;                         (or (getenv "CC") "clang++")
;;                         (file-name-sans-extension file)
;;                         (or (getenv "CFLAGS") "-g -Wall -Wextra -std=c++17")
;;                         file))
;;                ((derived-mode-p 'c-mode) ;; C Mode
;;                 (format "%s -o %s.o %s %s"
;;                         (or (getenv "CC") "clang")
;;                         (file-name-sans-extension file)
;;                         (or (getenv "CFLAGS") "-g -Wall -std=c11")
;;                         file))
;;                ((file-exists-p "Makefile") "make clean && make")))))
;; )

(provide 'l-cc)
