;; Eww
;;

;; https://emacs.stackexchange.com/questions/561/how-can-i-toggle-displaying-images-in-eww-without-a-page-refresh
(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

;; minimal rendering by default
(setq-default shr-inhibit-images t)   ; toggle with `I`

;; ;; TODO Before proceed to build toggle fonts, is there already a native toggle in EWW?
;; (setq-default shr-use-fonts nil)      ; toggle with `F`
;; (define-key eww-mode-map (kbd "F") #'my/eww-toggle-images)
;; (define-key eww-link-keymap (kbd "F") #'my/eww-toggle-images)
