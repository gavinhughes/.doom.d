;;; config.el -*- lexical-binding: t; -*-

;; Set manually because not captured from shell.
;; exec-path-from-shell doesn't work.
;; Where does exec-path get value from if not shell $PATH??
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")

(setq
  shell-file-name "/opt/homebrew/bin/fish"
  mac-right-command-modifier 'hyper  ;; We have hyper!
  trash-directory "~/.Trash"

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

  ;; Finder "put back" is not supported. If desired, instructions are here
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  delete-by-moving-to-trash t


  ;; https://github.com/jschaf/esup/issues/54
  ;; work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  esup-depth 0
  )

; Don't spread text across the entire screen.
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  visual-fill-column-center-text t
  visual-fill-column-width 100
)

;; Maximize frame at startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load! "my/functions")

(map!
 ;; Unavailable chords already captured by OS:
 ;; C-M-<return>    Magnet maximize window
 ;; C-M-<space>     Things quick capture


 :ni "s-<return>" (cmd! (message "Use <H-return>"))

 "M-s-SPC" 'org-capture

 ;; Movements
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line

 ;; Tiny        navigation in insert mode.
 :i "M-l" "<escape>la"
 :i "M-h" "<escape>ha"
 :i "M-j" "<escape>ja"
 :i "M-k" "<escape>ka"

 ;; Character access
 :i "M--" "–" ;; m-dash. Consistent with Mac.

 ;; Fast workspace jumps
 :i "s-1" '+workspace/switch-to-0
 :i "s-2" '+workspace/switch-to-1
 :i "s-3" '+workspace/switch-to-2
 :i "s-4" '+workspace/switch-to-3
 ;; Default new buffers to org-mode
 "s-n"          (cmd! (switch-to-buffer "*new*") (org-mode))

 ;; Remapped since it conflicts with org-mode insert-item-below muscle memory
 "H-<return>"     '+default/newline-below

 "M-s-]"          'next-buffer
 "M-s-["          'previous-buffer
 "s-'"            'evil-window-next
 "s-\""           'evil-window-prev
 "C-c c"          'clone-indirect-buffer
 "s-\\"           'avy-goto-char
 "C-S-M-<return>" 'toggle-frame-fullscreen
 ;; Doesn't work in emacs-mac. All frames are maximized.
 ;; "C-M-<return>" 'toggle-frame-maximized

 "s-k"           'kill-current-buffer
 "M-<left>"      'backward-word
 "M-<right>"     'forward-word

 :leader "b N"   (cmd! (switch-to-buffer "*new*") (org-mode))
 :leader "b n"   'rename-buffer
 :leader "j d"   'dired-jump
 :leader "v f"   'visual-fill-column-mode  ;; toggle
 :leader "<"     '+ivy/switch-workspace-buffer
 :leader ","     'ivy-switch-buffer
 :leader "SPC"   'org-roam-find-file
 :leader "n SPC" 'org-roam-dailies-find-today

  ;; Undefine. Either not in use or reassigned
  :leader "X"    (cmd! (message "Undefined"))
)

(map! :map (elixir-mode-map haskell-mode-map)
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 :i "M-s-." (cmd! (insert "|> "))
)

(after! org-roam
        :config
        (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
(setq org-roam-directory "~/Dropbox/OrgNotes/Roam"
      +org-roam-open-buffer-on-find-file nil
)


;; ORG-MODE

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

  ;; TAGS
  org-use-tag-inheritance nil
  org-tag-alist '((:startgrouptag)
                  ("Interaction")
                  (:grouptags)
                  ("ia")
                  ("{ia#.+}")
                  (:endgrouptag))
)

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))


  ;;  (spell-fu-mode-disable)
  (vi-tilde-fringe-mode -1)
  (load! "my/checkboxes")

  (setq
        company-idle-delay nil ;; no autocompletion
        org-hide-emphasis-markers t
        spell-fu-mode nil
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
           "WIP(i)"
           "PENDING(p!)"
           "PAUSED(u!)"
           "|"
           )
          )
        org-todo-keyword-faces
        '(
                ("TODO" :foreground "dim grey" :weight bold)
                ("WIP" :foreground "dim grey" :weight bold)
                ("DONE" :foreground "grey25" :weight bold)
                ("PENDING" :foreground "dim grey" :weight bold)
                ("PAUSED" :foreground "dim grey" :weight bold)
                ("[ ]" :foreground "dim grey")
                ("[X]" :foreground "grey25")
                ("CANCELLED" :foreground "grey25" :weight bold))
)

(map! :map org-mode-map
  :ni "s-<return>"         (cmd! (+org/insert-item-below 1))
  :ni "M-S-s-<return>"     (cmd! (+org/insert-item-above 1))
  :ni "M-s-<return>"       (cmd! (org-insert-subheading 1) (evil-insert 1))

  "s-j"                'org-todo
  "H-n"                'org-next-visible-heading
  "H-p"                'org-previous-visible-heading
)


;; ESS
(after! inferior-ess-mode
  map! :n "M-j" "<- "
  )

;; EWW
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


;; Ledger mode
(add-hook 'ledger-mode-hook (lambda ()
        (setq ledger-reports
                '(("bal" "%(binary) -f %(ledger-file) bal")
                ("reg" "%(binary) -f %(ledger-file) reg")
                ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                ("account" "%(binary) -f %(ledger-file) reg %(account)")))))

