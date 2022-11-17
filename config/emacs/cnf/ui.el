;; GENERAL UI
(setq x-stretch-cursor t
      show-paren-delay 0
      scroll-conservatively 5
      scroll-margin 0
      mouse-wheel-scroll-amount '(2 ((shift) . 2)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 2 ;; keyboard scroll two lines at a time
      frame-title-format "Allmighty Editor w/o Kitchen Sink")

(blink-cursor-mode t)
(show-paren-mode t)

(global-linum-mode -1)
(global-hl-line-mode t)
(column-number-mode t)

(tooltip-mode 0)

(set-language-environment "UTF-8")

;; LOLOLOL I needed to set weight/style to BLACK to get it to normal...
;; when it was set to normal it was bold (only in emacs)
;; FONT
;; 0123456789 /-><&^)!?@#${} Hello what up XxOo
;; (set-frame-font "Ttyp0:size=17" t t)
;; (set-fontset-font t 'unicode "Symbols Nerd Font-14:style=Regular" nil)
;; (set-face-attribute 'default nil :font "Iosevka 14")
;; (add-to-list 'default-frame-alist '(font . "Iosevka 14"))
;; (set-fontset-font "fontset-default"
;;                   'unicode-bmp
;;                   (font-spec :family "Ttyp0" :size 17))
;; (set-face-attribute 'fixed-pitch nil :family "Ttyp0:size=17")
;; (add-to-list 'default-frame-alist '(font . "Ttyp0:size=17"))
(defvar phga/font "Jetbrains Mono")
(defvar phga/font-size "19")
(defvar phga/font-family (concat phga/font ":size=" phga/font-size))
(set-frame-font phga/font-family t t)
(set-fontset-font "fontset-default"
                  'unicode-bmp
                  (font-spec :family phga/font :size (string-to-number phga/font-size)))
(set-face-attribute 'fixed-pitch nil :family phga/font-family)
(add-to-list 'default-frame-alist `(font . ,phga/font-family))

;; TEXT-SCALE: Scale text globally
(defvar phga/text-scale-amount 1)
(defvar phga/text-scale--current-size (string-to-number phga/font-size))
(defun phga/text-scale-adjust ()
  "Adjust the size of the frame-font by `phga/text-scale-amount'."
  (interactive)
  (phga/text-scale--adjust))

(defun phga/text-scale--scale (inc)
  "Scale the frame-font by `phga/text-scale-amount'.
If INC is not nil increase the font size else decrease it"
  (if inc (setq phga/text-scale--current-size
                (+ phga/text-scale--current-size phga/text-scale-amount))
    (setq phga/text-scale--current-size
          (- phga/text-scale--current-size phga/text-scale-amount)))
  (set-frame-font (concat phga/font ":size="
                          (number-to-string phga/text-scale--current-size)) t t))

(defun phga/text-scale--adjust ()
  "Actual function which will call itself with the temporary keymap to scale the text."
  (let ((ev last-command-event)
	      (echo-keystrokes nil))
    (pcase (event-basic-type ev)
      ((or ?= ?k) (phga/text-scale--scale t))
      ((or ?- ?j) (phga/text-scale--scale nil))
      ((or ?0) (progn (set-frame-font phga/font-family t t)
                      (setq phga/text-scale--current-size
                            (string-to-number phga/font-size))
                      (error "Reset to normal text scale"))))
    (message "Use j/=, k/-, 0 for further adjustment")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (key '(?- ?j ?k ?= ?0))
         (define-key map (vector (list key))
           (lambda () (interactive) (phga/text-scale--adjust))))
       map))))

;; LIGATURES
(straight-use-package 'ligature)
;; Enable the "www" ligature in every possible major mode
;; (ligature-set-ligatures 't '("www"))
;; Enable all Cascadia Code ligatures in programming modes
(ligature-set-ligatures
 'prog-mode
 '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
   "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
   ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
   "\\\\" "://"))

(global-ligature-mode t)
;; THEMES
;; SANITYINC
;; (straight-use-package 'color-theme-sanityinc-tomorrow)
;; ;; required on current theme setup to highlight marked folders
;; (set-face-attribute 'dired-marked nil :foreground "#b294bb")
;; (setq phga/dark-theme 'sanityinc-tomorrow-night)
;; (setq phga/light-theme 'sanityinc-tomorrow-day)

;; MODUS
;; (straight-use-package 'modus-themes)
;; (setq phga/light-theme 'modus-operandi)
;; (setq phga/dark-theme 'modus-vivendi)

;; (setq modus-themes-org-blocks 'gray-background
;;       modus-themes-headings ; this is an alist: read the manual or its doc string
;;       '((1 . (overline background))
;;         (2 . (rainbow overline))
;;         (t . (semibold))))


;; TAO
;; (straight-use-package 'tao-theme)
;; (setq phga/dark-theme 'tao-yin)
;; (setq phga/light-theme 'tao-yang)
;; (setq tao-theme-use-sepia nil
;;       tao-theme-scale-fn 'tao-theme-golden-scale
;;       tao-theme-use-height nil)

;; APROPOSPRIATE
;; (straight-use-package 'apropospriate-theme)
;; (setq phga/dark-theme 'apropospriate-dark)
;; (setq phga/light-theme 'apropospriate-light)

;; ZENBURN
;; (straight-use-package 'zenburn-theme)
;; (setq phga/dark-theme 'zenburn)

;; DRACULA
;; (straight-use-package 'dracula-theme)
;; (setq phga/dark-theme 'dracula)

;; MATERIAL
;; (straight-use-package 'material-theme)
;; (setq phga/dark-theme 'material)

;; IMMATERIAL
;; (straight-use-package 'immaterial-theme)
;; (setq phga/dark-theme 'immaterial-dark)

;; SUBATOMIC
;; (straight-use-package 'subatomic-theme)
;; (setq phga/dark-theme 'subatomic)

;; SHANTY
;; (add-to-list 'custom-theme-load-path
;;              "/home/phga/.dotfiles/emacs/straight/repos/shanty-themes/")
;; (straight-use-package '(shanty-themes :host github :repo "qhga/shanty-themes"))
(straight-use-package 'shanty-themes)
(setq phga/dark-theme 'shanty-themes-dark)
(setq phga/light-theme 'shanty-themes-light)


;; FIXES
;; Shell mode line highlighted
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)
(custom-set-faces '(comint-highlight-prompt ((t nil))))


(setq phga/current-theme phga/dark-theme)
(load-theme phga/current-theme t)

;; Some themes do not load correctly in frames
(add-hook 'after-make-frame-functions
          (defun phga/frame-theme-fix (f)
            (select-frame f)
            (load-theme phga/current-theme t)))

(defun phga/cycle-themes ()
  (interactive)
  "Cycle through my preferred dark and light theme"
  (if (eq phga/current-theme phga/dark-theme)
      (progn
        (disable-theme phga/dark-theme)
        (load-theme phga/light-theme t)
        (message "Loaded light theme")
        (setq phga/current-theme phga/light-theme))
    (progn
      (disable-theme phga/light-theme)
      (load-theme phga/dark-theme t)
      (message "Loaded dark theme")
      (setq phga/current-theme phga/dark-theme))))

;; MODE-LINE
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line
                         '(""
                           ;; (:eval (propertize "%* %b"))
                           (:eval (propertize "%* %b" 'face 'mode-line-buffer-id))
                           evil-mode-line-tag
                           (:eval (propertize "%m"))))
                        ;; (:eval (propertize "%m" 'face 'font-lock-comment-face))))
                        ;; right
                        (format-mode-line mode-line-position)))))

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; OLIVETTI: Distraction free writing in some buffers
(straight-use-package 'olivetti)

(provide 'ui)
