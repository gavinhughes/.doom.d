;;; config.el -*- lexical-binding: t; -*-

;; When you're changing this, work on a branch.

;; Load MacOS path
;; (exec-path-from-shell-initialize)
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")

;; Does a built-in function already exist?
(defun load-directory! (dir)
  (let ((loadf (lambda (f)
    (load! (concat dir "/" f)))))
    (mapc loadf (directory-files dir nil "\\.el$"))
    )
  )

(load! "my/global/functions.el")
;; (load-directory! "my/global")

;; Use word wrap in all buffers that minor mode message-mode.
;; https://blog.jethro.dev/posts/migrating_to_doom_emacs/
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(setq
  shell-file-name "/opt/homebrew/bin/fish"
  mac-right-command-modifier 'hyper
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

;; Might be better to set this .projectile.
;; Not working.  Why?
;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
(add-to-list 'projectile-globally-ignored-file-suffixes ".org_archive")

;; If set to ‘nil’, the major mode is taken from the previously current buffer.
(setq-default major-mode 'org-mode)

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

(map!
 ;; Chords owned by MacOS:
 ;;   C-M-<return>    Magnet maximize window
 ;;   C-M-<space>     Things quick capture

 ;; :ni "s-<return>" (cmd! (message "Use <H-return>"))

 "M-s-SPC" 'org-capture

 ;; Movement
 :n "j"                  'evil-next-visual-line
 :n "k"                  'evil-previous-visual-line
    "M-<left>"           'backward-word
    "M-<right>"          'forward-word
    "H-<return>"         '+default/newline-below
    "s-\\"               'avy-goto-char
 :i "s-l" "<escape>la" ;; Step over single chars without leaving insert mode
 :i "s-h" "<escape>ha"
 :i "s-S" "<escape>0i"

 ;; Special characters
 :i "M--" "–" ;; m-dash. Consistent with Mac.

 ;; Buffers
 "s-n"            (cmd! (evil-buffer-new 1 nil))
 "s-k"            'kill-current-buffer
 "M-s-k"          'kill-buffer-and-window
 "s-,"            'ivy-switch-buffer
 "M-s-]"          'next-buffer
 "M-s-["          'previous-buffer
 "C-c c"          'clone-indirect-buffer
 "s-p"            'ps-print-buffer-with-confirmation

 ;; Windows
 "s-'"            'evil-window-next
 "s-\""           'evil-window-prev
 "C-S-M-<return>" 'toggle-frame-fullscreen
 ;; Doesn't work in emacs-mac. All frames are maximized.
 ;; "C-M-<return>" 'toggle-frame-maximized


;; Workspaces.
;; :n assignment is in the package. Add :i.
:ni "s-1"        '+workspace/switch-to-0
:ni "s-2"        '+workspace/switch-to-1
:ni "s-3"        '+workspace/switch-to-2
:ni "s-4"        '+workspace/switch-to-3

 ;; Other
 :ni "s-O"       'evil-open-above
     "s-<up>"    '+evil/insert-newline-above
     "s-<down>"  '+evil/insert-newline-below
:niv "C-u"       'universal-argument
     "H-t"       'toggle-theme

;; Make `$` behave same as in :normal.
 :v  "$"         (cmd! (evil-end-of-line) (evil-backward-char) (evil-forward-char))

 ;; Add :n to override assignment in +workspaces
 :ng "s-9"        (cmd! (find-file "~/Desktop/queue.log"))
     "H-c"        (cmd! (find-file "/Users/gavinhughes/.doom.d/config.el"))
     "<f7>"        'org-tags-view
     "<f9>"        'org-agenda-list

 ;; Undefine unused or reassigned chords
 :n "O"          'undefined ;; evil-open-above
 :ni "C-d"       'undefined ;; evil-scroll-down

 ;; Leaders – place last, otherwise errors.
 :leader "f m"   'doom/move-this-file
 :leader "b n"   'rename-buffer
 :leader "j d"   'dired-jump
 :leader "v f"   'visual-fill-column-mode  ;; toggle
 :leader "<"     '+ivy/switch-workspace-buffer
 :leader ","     'ivy-switch-buffer
 :leader "SPC"   'org-roam-find-file
 :leader "M-SPC" 'org-roam-find-file
 :leader "n SPC" 'org-roam-dailies-find-today
 :leader "o o"   'reveal-in-osx-finder

 ;; Undefine unused or reassigned chords
 :leader "X"     'undefined
 :leader "b N"   'undefined
 :leader "u"     'undefined ;; Universal argument
)

(map! :map haskell-mode-map
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 )

(map! :map elixir-mode-map
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 :i "M-s-." (cmd! (insert "|> "))
 )

;; Error on markdown-insert-list-item
;; (map! :map markdown-mode-map
;;  :ni   "s-<return>" (cmd! (evil-open-below 1) (markdown-insert-list-item))
;;       )

(after! org-roam
        :config
        (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(setq org-roam-directory "~/Dropbox/OrgNotes/Roam"
      +org-roam-open-buffer-on-find-file nil
      org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    ;; values at column 12
                                    :head "#+title:    ${title}\n#+startup:  overview\ntags:       "
                                    :unnarrowed t))
)


;; ORG-MODE

(setq
  org-directory "~/Dropbox/OrgNotes/"
  org-ellipsis " ▼ "
  org-cycle-separator-lines 3
  org-special-ctrl-k t
  ;; Not working 7/13/21
  org-ctrl-k-protect-subtree t
  org-blank-before-new-entry '((heading . nil)
                               (plain-list-item . nil))
  ;; https://stackoverflow.com/a/41969519/173162
  org-agenda-files (directory-files-recursively "~/Dropbox/OrgNotes/" "\\.org$")
  org-agenda-window-setup 'current-window
  org-agenda-show-future-repeats nil
  org-agenda-skip-deadline-if-done t
  org-agenda-skip-scheduled-if-done t
  org-agenda-skip-timestamp-if-done t
  org-agenda-start-on-weekday 0
  org-agenda-custom-commands
      '(("d" "Today's Tasks"
         ((agenda "" ((org-agenda-span 1)
        	      (org-agenda-overriding-header "Today's Tasks"))))))

  ;; TAGS
  org-use-tag-inheritance nil
  org-agenda-use-tag-inheritance nil
  org-tag-alist '((:startgrouptag)
                  ("Interaction")
                  (:grouptags)
                  ("ia")
                  ("{ia#.+}")
                  (:endgrouptag))

  ;; Waiting on a better solution:
  ;; https://www.reddit.com/r/DoomEmacs/comments/ocv27u/how_to_change_latex_preview_color_on_per_theme/
  org-format-latex-options '(:foreground "MediumPurple" :background default :scale 1.0
                             :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
                             :matchers ("$" "$$" "\\(" "\\["))


  )

(after! org
  ;; (load-directory! "my/org-mode")
  ;;  (spell-fu-mode-disable)
  (vi-tilde-fringe-mode -1)

  (setq
    org-download-method 'directory
    org-download-image-dir "images"
    org-download-heading-lvl nil
    org-download-timestamp "%Y%m%d-%H%M%S_"
    org-image-actual-width 300
        company-idle-delay nil ;; no autocompletion
        org-hide-emphasis-markers t
        spell-fu-mode nil

        org-todo-keywords
        '(
          (sequence
           "[ ](T)"
           "|"
           "[X](t)"
           )
          (sequence
           "TODO(K)"
           "|"
           "DONE(k)"
           "CANCELLED(l)"
           )
          (sequence
           "WIP(i)"
           "PENDING(p)"
           "PAUSED(u)"
           "|"
           )
          )
        org-priority-faces '((?A . (:foreground "dim grey"))
                           (?B . (:foreground "dim grey"))
                           (?C . (:foreground "dim grey")))
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
  )

;; (defun toggle-theme ()
;;   (setq (doom-theme))
;;   (if (= doom-theme 'doom-one-light)
;;       (setq (doom-theme 'doom-one))
;;     (setq (doom-there)))
;;   )

(map! :map org-mode-map
  :ni "s-<return>"         (cmd! (+org/insert-item-below 1))
  :ni "M-S-s-<return>"     (cmd! (+org/insert-item-above 1))
  :ni "M-s-<return>"       (cmd! (org-insert-subheading 1) (evil-insert 1))
  ;; Insert a heading while currently working a bullet list
  :nie "C-M-s-<return>"     (cmd! (org-previous-visible-heading 1) (+org/insert-item-below 1))

  "s-r"                'org-roam
  "s-i"                'org-roam-insert
  "s-I"                'org-roam-insert-immediate
  :niv "s-j"                'org-todo
  "H-n"                'org-next-visible-heading
  "H-p"                'org-previous-visible-heading
  "H-r"                '+org/refile-to-current-file
  "H-R"                '+org/refile-to-file
  "H-a"                'org-archive-subtree
  "s-."                'org-shiftright
  "s->"                'org-shiftleft
  "H-l"                "C-u C-u C-c C-x C-l" ;; Preview all latex
  "H-L"                "C-u C-c C-x C-l" ;; Un-preview all latex
  "s-M"                'org-refile

  ;; Choose one or the other
  "C-c l a y"          #'zz/org-download-paste-clipboard
  "C-M-y"              #'zz/org-download-paste-clipboard

)


;; ESS
(map! :map inferior-ess-mode
  :n "M-j" "<- "
  )

;; EWW
(after! eww
  ;; (load-directory! "my/eww-mode")
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
