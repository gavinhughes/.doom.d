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


(defun gh/ps-print-buffer-with-confirmation ()
  "Paginate and print buffer contents as image."

  (interactive
   (unless (y-or-n-p "Send current buffer to default printer? ")
     (error "Canceled")))
  (ps-print-buffer))
