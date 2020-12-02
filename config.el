;;; config.el -*- lexical-binding: t; -*-

(setq
  user-full-name "Gavin Hughes"
  user-mail-address "gavhug@gmail.com"
  doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
  doom-big-font (font-spec :family "DejaVu Sans Mono" :size 30) ;; For presentations/streaming
  doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 16)
  doom-serif-font (font-spec :family "DejaVu Serif")

  ;; Org mode
  org-directory "~/Dropbox/OrgNotes/"
  org-ellipsis " ▼ "

  undo-limit 80000000                         ; Raise undo-limit to 80Mb
  evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
  auto-save-default t                         ; Nobody likes to loose work, I certainly don't
  inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
  truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
  global-visual-line-mode t                   ; Visual line navigation everywhere.
  ispell-program-name "hunspell"

  ;; https://github.com/jschaf/esup/issues/54
  ;; work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  esup-depth 0
  )

(after! org
  (+company/toggle-auto-completion)
  (disable-flyspell)
  (load! "my/checkboxes")
  (setq
  org-hide-emphasis-markers t
  display-line-numbers nil
  org-todo-keywords '(
  (sequence
   "TODO(t)"
   "WIP(w)"
   "|"
   "DONE(d!)"
   "CANCELLED(c@/!)")
  (sequence
   "[ ](T)"
   "|"
   "[X](D)")))
  )
;; Setting this anywhere in config.el cause org-todo-keywords to
;; be set to nil.
;; (setq initial-major-mode 'org-mode)             ; Start scratch buffer in org mode.


;;   (map! :map org-mode "M-S-s-<return>" #'org-insert-subheading-line-below)

; Don't spread text across the entire screen.
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  ;; visual-fill-column-width ;; Using fill-column width
  visual-fill-column-center-text t
  )

(after! eww
  (load! "my/eww")
  (map! :map eww-mode-map "I" #'my/eww-toggle-images)
  )


;;;;; All this effort to get org-insert-subheading-line-below working.
;; (map! :map org-mode
;;        "M-S-s-return" "C-S-return escape 2*d p A tab") ;; org-insert-subheading-line-below
;;
;; Or as a function
;; (defun org-insert-subheading-line-below ()
;;   (interactive)
;;   (+org/insert-item-above)
;;   (evil-force-normal-state)
;;   (evil-org-delete)
;;   (evil-paste-after)
;;   (evil-org-append-line)
;;   (org-cycle)
;;   (evil-org-append-line)
;; )

;; Maximize frame at startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(map!
 :n "s-\\" 'avy-goto-char
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line
 "M-s-]" 'next-buffer
 "M-s-[" 'previous-buffer
 "s-]"   'evil-window-right
 "s-["   'evil-window-left
 "C-c c" 'clone-indirect-buffer
 ;; org-insert-subheading
 "M-S-s-<return>" "M-s-<return>"
 )

;; These are currently only for Haskell. Scope them
;; to Haskell?
(map!
 ;; How to get the blank space after `->`?
 :i "s-;" "-> "
 )

;; (defun org-insert-subheading-above ()
;;   (interactive)
;; )


(defun disable-flyspell () (flyspell-mode -1))


;; ;; (use-package org-download
;; ;;   :after org
;; ;;   :bind
;; ;;   (:map org-mode-map
;; ;;     (("s-Y" . org-download-screenshot)
;; ;;      ("s-y" . org-download-yank))))
;; ;; (setq org-download-screenshot-method "convert clipboard: %s")
;; ;; (setq org-download-method '+org/org-download-method)
