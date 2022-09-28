;; TRAMP: remote file editing
(setq tramp-default-method "ssh")
(require 'tramp)
;; variables
(push '(nil "\\`root\\'" "/ssh:%h:") tramp-default-proxies-alist)
(push '((regexp-quote (system-name)) nil nil) tramp-default-proxies-alist)

(defun phga/connect-to-pihole()
  "Connect to our server"
  (interactive)
  (dired "/sudo:pi:/"))

(defun phga/connect-to-nsv1()
  "Connect to our server"
  (interactive)
  (dired "/sudo:nsv1:/"))

(defun phga/connect-to-helium()
  "Connect to our server"
  (interactive)
  (dired "/sudo:helium:/"))

(defun phga/connect-to-naz()
  "Connect to our server"
  (interactive)
  (dired "/sudo:naz:/"))

(defun phga/connect-to-glou()
  "Connect to our server"
  (interactive)
  (dired "/sudo:glou:/"))

(provide 'a-tramp)