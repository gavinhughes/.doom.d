;;; config.el -*- lexical-binding: t; -*-

(setq
  shell-file-name "/opt/homebrew/bin/fish"

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
  display-line-numbers-type 'visual

  ;; https://github.com/jschaf/esup/issues/54
  ;; work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  esup-depth 0
  )

;; Maximize frame at startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load! "my/functions")

(after! org-roam
        :config
        (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
(setq org-roam-directory "~/Dropbox/OrgNotes/Roam")

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
  '(
    (sequence
     "[ ](j)"
     "|"
     "[X](J)"
     )
    (sequence
     "TODO(k)"
     "|"
     "DONE(K!)"
     "CANCELLED(L!)"
    )
    (sequence
     "WIP(i!)"
     "PENDING(p!)"
     "|"
     )
    )
  org-todo-keyword-faces
      '(
        ("TODO" :foreground "dim grey" :weight bold)
        ("WIP" :foreground "dim grey" :weight bold)
        ("DONE" :foreground "grey25" :weight bold)
        ("PENDING" :foreground "dim grey" :weight bold)
        ("[ ]" :foreground "dim grey")
        ("[X]" :foreground "grey25")
        ("CANCELLED" :foreground "grey25" :weight bold))
)
(after! org
  ;;  (spell-fu-mode-disable)
  (vi-tilde-fringe-mode -1)
  (load! "my/checkboxes")
  (setq
        company-idle-delay nil ;; no autocompletion
        org-hide-emphasis-markers t
        spell-fu-mode nil)
)

(map! :map org-mode-map
   "M-<left>" 'backward-word
   "M-<right>" 'forward-word
   "M-s-<return>" 'org-insert-subheading
   "M-S-s-<return>" (cmd! (+org/insert-item-above 1) (org-cycle))
   "s-j" 'org-todo
)

;; Start new buffers in org-mode.
;;   Still get buffers in Fundamental mode using s-n.
(map! :leader "b N" (cmd! (switch-to-buffer "*new*") (org-mode)))

; Don't spread text across the entire screen.
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  visual-fill-column-center-text t
  visual-fill-column-width 100
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

(map!
 "M-s-]"          'next-buffer
 "M-s-["          'previous-buffer
 "s-}"            'evil-window-next
 "s-{"            'evil-window-prev
 "C-c c"          'clone-indirect-buffer
 "s-\\"           'avy-goto-char
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

 ;; Movements
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line

 ;; Move without going to normal mode.
 :i "s-l" "<escape>la"  ;; Remaps goto-line
 :i "s-h" "<escape>ha"

 ;; Character access
 :i "M--" "–" ;; m-dash. Consistent with Mac.

 ;; These are for Haskell, Elixir. Scope them?
 ;; How to get the blank space after `->`?
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

;; Pasting images in org
;; https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))
