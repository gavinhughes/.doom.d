;;; config.el -*- lexical-binding: t; -*-

(setq
  user-full-name "Gavin Hughes"
  user-mail-address "gavhug@gmail.com"
  doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
  doom-big-font (font-spec :family "DejaVu Sans Mono" :size 30) ;; For presentations/streaming
  doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 16)
  doom-serif-font (font-spec :family "DejaVu Serif")
  doom-scratch-initial-major-mode 'org-mode

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

(load! "my/functions")

(setq org-roam-directory "~/Dropbox/OrgNotes/Roam")
;; (require 'company-org-roam)
    ;; (use-package company-org-roam
    ;;   :when (featurep! :completion company)
    ;;   :after org-roam
    ;;   :config
    ;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! org-roam
        :config
        (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;; Not working yet
;; https://www.philnewton.net/blog/how-i-get-work-done-with-emacs/#fn.1
;; https://www.philnewton.net/blog/org-agenda-monthly-goals/
;; https://emacs.stackexchange.com/questions/19091/how-to-set-org-agenda-prefix-format-before-org-agenda-starts
;; (with-eval-after-load 'org-agenda
;;   (add-to-list
;;         'org-agenda-prefix-format
;;                 '((agenda . " %i %-20:c%?-12t%-6e% s")
;;                 (todo   . " %i %-20:c %-6e")
;;                 (tags   . " %i %-20:c")
;;                 (search . " %i %-20:c"))))

(setq
  org-directory "~/Dropbox/OrgNotes/"
  org-ellipsis " ▼ "
  org-blank-before-new-entry '((heading . nil)
                               (plain-list-item . nil))
  ;; https://stackoverflow.com/a/41969519/173162
  org-agenda-files (directory-files-recursively "~/Dropbox/OrgNotes/" "\\.org$")
  org-agenda-custom-commands
      '(("d" "Today's Tasks"
         ((agenda "" ((org-agenda-span 1)
        	      (org-agenda-overriding-header "Today's Tasks"))))))
  org-todo-keywords
      '((sequence
        "TODO(t)"
        "WIP(w)"
        "|"
        "DONE(d!)"
        "CANCELLED(c!)")
        (sequence
        "[ ](T)"
        "|"
        "[X](D)"))
  org-todo-keyword-faces
      '(
        ("TODO" :foreground "red" :weight bold)
        ("WIP" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAIT" :foreground "orange" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold))
)
(after! org
  (disable-flyspell)
  (vi-tilde-fringe-mode -1)
  (load! "my/checkboxes")
  (setq
        company-idle-delay nil ;; no autocompletion
        org-hide-emphasis-markers t)
  ;; These don't have the desired effect. They still do org-metaright,
  ;; and demote/promote, just without including the subheadings.
  ;; (define-key org-mode-map (kbd "S-M-<left>") nil)
  ;; (define-key org-mode-map (kbd "S-M-<right>") nil)

  ;; Org mode mappings
  (map!
   ;; These mappings are removed in favor of S-H and S-L.
   ;; The original mappings available for use elsewhere.
   ;; "S-<return>" (cmd! (org-insert-heading) (evil-append 1))
   ;; "M-S-<return>" (cmd! (org-insert-subheading) (evil-append 1))
   )
)

(map! :map org-mode-map
   "M-<left>" 'backward-word
   "M-<right>" 'forward-word
   ;; "S-<return>" (cmd! (org-insert-heading) (evil-append 1))
   ;; "M-S-<return>" (cmd! (org-insert-subheading) (evil-append 1))
   ;; "M-S-<return>" (cmd! (org-insert-subheading) (evil-append 1))
   ;; :n "S-<return>" 'org-insert-heading
   ;; ;; :i "M-S-<return>" 'org-insert-subheading
   ;; :i "S-<return>" 'org-insert-heading
      )
;; Start new buffers in org-mode.
;;   Still get buffers in Fundamental mode using s-n.
(map! :leader "b N" (cmd! (switch-to-buffer "*new*") (org-mode)))

;;   (map! :map org-mode "M-S-s-<return>" #'org-insert-subheading-line-below)

; Don't spread text across the entire screen.
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  visual-fill-column-center-text t
  )

(after! inferior-ess-mode
  map! :n "M-j" "<- "
  )

(after! eww
  (load! "my/eww")
  ;; This has global effect.  How to limit to just eww mode?
  ;; (visual-fill-column-mode t)
  (map! :map eww-mode-map
        "I" #'my/eww-toggle-images
        "M-<return>" 'my-eww-open-in-new-window
        "M-s-[" 'eww-back-url
        "M-s-]" 'eww-forward-url)
        ;; "<s-mouse-1>" 'my-eww-open-in-new-window
  )

;; Get this working to have windows in new frame
(defun my-eww-open-in-new-window ()
  (interactive)
  (eww-open-in-new-buffer)
  ;; (tear-off-window nil)
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
 "M-s-]"          'next-buffer
 "M-s-["          'previous-buffer
 "s-}"          'evil-window-next
 "s-{"          'evil-window-prev
 "C-c c"        'clone-indirect-buffer
 "s-\\"         'avy-goto-char
 "C-S-M-<return>" 'toggle-frame-fullscreen
 ;; This doesn't work in emacs-mac bc all frames are maximized.
 ;; "C-M-<return>" 'toggle-frame-maximized
 "s-k"          'kill-current-buffer

 :leader "j d" 'dired-jump
 :leader "v f" 'visual-fill-column-mode  ;; toggles
 :leader "<"  '+ivy/switch-workspace-buffer
 :leader ","  'ivy-switch-buffer
 ;; ;; Previously, but stopped wowrking...

 ;; ;; org-insert-subheading
 ;; ;;   Why not reassign the second?
 ;; ;; "M-S-s-<return>" "M-s-<return>"

 ;; ;; ROAM
 :leader "n SPC" 'org-roam-dailies-find-today

 )

(map!
 ;; :n "b" 'avy-goto-char
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line

 ;; Move cursor without going to normal mode.
 ;; Remaps goto-line
 :i "s-l" "<escape>la"
 :i "s-h" "<escape>ha"
 :i "M-s-<return>" "<escape>ja"

 ;; These are for Haskell, Elixir. Scope them?
 ;; How to get the blank space after `->`?
 :i "M--" "–" ;; m-dash
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 :i "M-s-." (cmd! (insert "|> "))

 ;; Fast workspace jumps
 :i "s-1" '+workspace/switch-to-0
 :i "s-2" '+workspace/switch-to-1
 )

;; Ledger mode
;;
(add-hook 'ledger-mode-hook (lambda ()
        (setq ledger-reports
                '(("bal" "%(binary) -f %(ledger-file) bal")
                ("reg" "%(binary) -f %(ledger-file) reg")
                ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                ("account" "%(binary) -f %(ledger-file) reg %(account)")))))

(defun disable-flyspell () (flyspell-mode -1))


;; ;; (use-package org-download
;; ;;   :after org
;; ;;   :bind
;; ;;   (:map org-mode-map
;; ;;     (("s-Y" . org-download-screenshot)
;; ;;      ("s-y" . org-download-yank))))
;; ;; (setq org-download-screenshot-method "convert clipboard: %s")
;; ;; (setq org-download-method '+org/org-download-method)
