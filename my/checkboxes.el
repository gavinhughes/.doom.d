;;; checkbox.el --- Functions for dealing with org checkboxes.

;;; Initial code extracted from: https://emacs.stackexchange.com/a/5617/18503
;;;
;;; == Dev Plan ==
;;; - Auto-select list at point.
;;; - If region is already a list, do not prepend "- ".
;;;
;;; Code:

(defun org-convert-lines-to-checklist (beg end)
  "Convert all plain lines in region to a plain list with
checkboxes."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (dotimes (_ (- (line-number-at-pos end) (line-number-at-pos beg)))
      (insert "- [ ] ")
      (indent-according-to-mode)
      (forward-line 1))))

