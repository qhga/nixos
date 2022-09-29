(straight-use-package 'vterm)
(defun phga/new-vterm ()
  (interactive)
  "Creates a new vterm in the current window and renames in with a random name"
  (let* ((curr-window (get-buffer-window (current-buffer)))
         (adjectives '("last" "worthless" "labored" "sordid" "puzzled"
                       "tremendous" "clammy" "present" "outrageous" "raspy"
                       "nimble" "rampant" "sour" "angry" "colorful" "early"
                       "alert" "quixotic" "husky" "shrill" "disturbed" "damaged"
                       "breakable" "painful" "gainful" "delicate" "unruly"
                       "huge" "spooky" "incompetent" "dependent" "long"
                       "skillful" "troubled" "longing" "second-hand" "dazzling"
                       "wary" "resonant" "guarded" "abiding" "crowded" "eatable"
                       "grandiose" "alike" "unsuitable" "shut" "ambitious"))
         (vterm-name (generate-new-buffer-name
                      (concat "*" (nth (random (length adjectives))
                                       adjectives) "-vterm*"))))
    (vterm vterm-name)))

;; Otherwise vterm instantly exits
(add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
(add-to-list 'vterm-tramp-shells '("sudo" "/bin/bash"))

(provide 'a-vterm)