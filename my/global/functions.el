;; my/functions.el -*- lexical-binding: t; -*-

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


(defun gh/relative-name-nondirectory (path level)
  "Return the filename of the directory LEVEL above the end of path.
   Zero indexed. (First above is level 0.)"

  (if (= level 0)
      (file-name-nondirectory (directory-file-name (file-name-directory path)))
      (gh/relative-name-nondirectory (directory-file-name (file-name-directory path)) (- level 1))))



(defun gh/insert-time ()
  (interactive)
  (insert (format-time-string "%I:%M" (current-time))))



(defun gh/get-safari-front-name ()
  (do-applescript (string-to-multibyte "
tell application \"Safari\"
  name of document of front window
end tell
")))

(defun gh/get-safari-front-url ()
  (do-applescript (string-to-multibyte "
tell application \"Safari\"
  URL of document of front window
end tell
")))

(defun gh/yank-safari-front-url ()
  "Yank Safari's front window URL."
  (interactive)
  (kill-new (gh/get-safari-front-url)))

(defun gh/org-insert-safari-front-link ()
  "Insert a link to the page in Safari's front window."
  (interactive)
  (insert (org-make-link-string (gh/get-safari-front-url)
                                (gh/get-safari-front-name))))



(defun gh/org-roam-dailies-goto-next-note-or-tomorrow ()
  "Find next daily-note, or tomorrow it at newest note."

  (interactive "p")
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (if (eq position (- (length dailies) 1))
      (org-roam-dailies-goto-tomorrow (length dailies))))


(defun gh/agenda-file-filter (file-names)
  "Filter out unwanted files from org agenda."
  (seq-filter (lambda(x) (and
      (not (string-match "/logseq/"(file-name-directory x)))
      (not (string-match "/bak/"(file-name-directory x)))
      (not (string-match "archive"(file-name-directory x))) ; Excludes archive.org
    )) file-names))


(defun gh/set-org-agenda-files ()
  "Custom setq for org-agenda-files"
     ;; Use this to include all org files:
     ;; (setq
     ;;   org-agenda-files (sort (gh/agenda-file-filter (directory-files-recursively org-roam-directory "\\.org$")) #'string>)))
     ;;
     ;; Use this to include all org files:
     (setq org-agenda-files "")
     (setq org-agenda-files (append
                       (sort (gh/agenda-file-filter (directory-files-recursively (concat org-roam-directory "/pages") "\\.org$")) #'string>)
                       (sort (gh/agenda-file-filter (directory-files-recursively (concat org-roam-directory "/daily") "\\.org$")) #'string>)
                       )))


(defun gh/org-agenda-files-changed-p ()
  (not (equal org-agenda-files (gh/set-org-agenda-files))))


(defun gh/open-or-pop-to-agenda ()
  "Open Agenda if created, otherwise create and open it."

  (interactive)
  (if (gh/org-agenda-files-changed-p)
      (org-agenda)
      (if (gnus-buffer-exists-p "*Org Agenda*")
         (pop-to-buffer-same-window "*Org Agenda*")
         (org-agenda))))



;; WIP
;; (defun gh/org-roam-dailies-goto-next-note-or-tomorrow ()
;;   "Find next daily-note, or tomorrow it at newest note."

;;   (interactive "p")
;; (condition-case nil
;;     (delete-file filename)
;;   ((debug error) nil))
