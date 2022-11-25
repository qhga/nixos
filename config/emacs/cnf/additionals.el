;; GET MORE APPLICATIONS (GNUS,...) TODO
(push (concat user-emacs-directory "cnf/app") load-path)
;; DIRED: file browser
(require 'a-dired)
;; TRAMP
(require 'a-tramp)
;; GNUS -> Thunderbird
;; (require 'a-gnus)
;; SHELL
(require 'a-shell)
;; VTERM
(require 'a-vterm)
;; PDF-TOOLS
(require 'a-pdf)
;; RIPGREP: rg layer to edit occurences in place
(require 'a-rg)
;; MVTN: Notetaking
(require 'a-mvtn)
;; ERC: IRC Client
;; (require 'a-irc)
;; EIN: Emacs Ipython Notebooks
(require 'a-ein)
;; ORG-MODE
;; hooks
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(with-eval-after-load 'org (require 'a-orgmode))
;; PEEP-DIRED: preview files in dired
(straight-use-package 'peep-dired)
;; variables
(setq peep-dired-cleanup-on-disable t
      peep-dired-enable-on-directories t)
;; hooks
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; MAGIT: nice git ui
(straight-use-package 'magit)

;; DICTCC: query dict.cc without leaving emacs
(straight-use-package 'dictcc)

;; IBUFFER: UI to clean up buffers
;; variables
(setq ibuffer-saved-filter-groups (quote (("phga"
                                           ("Dired" (mode . dired-mode))
                                           ("ORG" (mode . org-mode))
                                           ("LaTeX" (or (file-extension . "bib")
                                                        (file-extension . "tex")))
                                           ("Programming" (or (mode . c++-mode)
                                                              (mode . c-mode)
                                                              (mode . python-mode)
                                                              (mode . web-mode)
                                                              (mode . java-mode)
                                                              (mode . go-mode)
                                                              (mode . js2-mode)
                                                              (mode . emacs-lisp-mode)
                                                              (mode . sh-mode)))))))

;; hooks
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "phga")))

;; GIT-GUTTER: Indicators for git status inside the fringe
(straight-use-package 'git-gutter)
(straight-use-package 'git-gutter-fringe)
(require 'git-gutter-fringe)
;; 0 means only update on save, otherwise set to 0.02
;; otherwise cursor stutters (e.g. at 2 or 8)
(setq git-gutter:update-interval 0
      git-gutter:handled-backends '(git hg))

;; Indicator style
(fringe-helper-define 'git-gutter-fr:added 'center
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX..")

(fringe-helper-define 'git-gutter-fr:modified 'center
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX..")

(fringe-helper-define 'git-gutter-fr:deleted 'center
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX.."
  "..XXX..")

(add-hook 'prog-mode-hook #'git-gutter-mode)

(provide 'additionals)
