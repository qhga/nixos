;; JAVA
(straight-use-package 'lsp-java)
(add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook (lambda ()
                            (setq-local c-default-style "java")
                            (c-set-offset 'arglist-intro '+)
                            (c-set-offset 'arglist-cont-nonempty '+)
                            (c-set-offset 'arglist-close '0)
                            (c-set-offset 'case-label '+)))


(require 'dap-java)

(provide 'l-java)