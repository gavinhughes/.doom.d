;;; my/functions.el -*- lexical-binding: t; -*-

;; (defun gh/toggle-theme ()
;;
;; TODO Add vars for light and dark
;;      Toggle between them
;;
;;   (interactive)
;;   (if (eq (car custom-enabled-themes) 'tsdh-light)
;;       (disable-theme 'tsdh-light)
;;     (load-theme 'tsdh-light)))


(defun ps-print-buffer-with-confirmation ()
  "Paginate and print buffer contents as image."

  (interactive
   (unless (y-or-n-p "Send current buffer to default printer? ")
     (error "Canceled")))
  (ps-print-buffer))

;; You could also try setting when the system inverts colors. This would get you started
;; but needs to be changed for invert, not mode:
;; https://www.reddit.com/r/emacs/comments/hejsqm/is_there_a_way_to_detect_lightdark_mode_on_mac/
(defun toggle-theme ()
  "Toggle between light and dark themes"

  (interactive)
  (setq dark-theme  'doom-one
        light-theme 'doom-one-light)
  (if (eq (car custom-enabled-themes) dark-theme)
      (load-theme light-theme)
    (load-theme dark-theme)))
