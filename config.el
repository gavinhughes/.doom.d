;;; config.el -*- lexical-binding: t; -*-

;; Start the Emacs server from this instance so that all emacsclient calls are routed here.
;; (if (server-running-p ()) nil (server-start))

;; To reduce the risk of loading outdated byte code files, set load-prefer-newer
;; and enable auto-compile-on-load-mode as early as possible.
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq load-prefer-newer t)

(load! "my/global/functions.el")

(setq
  shell-file-name "/opt/homebrew/bin/fish"
  mac-right-command-modifier 'hyper
  trash-directory "~/.Trash"
  confirm-kill-emacs nil ; Disable exit confirmation

  user-full-name "Gavin Hughes"
  user-mail-address "gavhug@gmail.com"

  doom-scratch-initial-major-mode 'org-mode
  undo-limit 80000000                         ; Raise undo-limit to 80Mb
  evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
  auto-save-default t
  inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
  truncate-string-ellipsis "…"                ; Use unicode ellipsis
  global-visual-line-mode t                   ; Visual line navigation everywhere.

  display-line-numbers-type 'visual

  ;; Finder "put back" is not supported. If desired, instructions are here
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  delete-by-moving-to-trash t

  ;; https://github.com/jschaf/esup/issues/54
  ;; work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  esup-depth 0
  )

;; Use evil in minibuffer to support 'gh/consult-ripgrep-tag
;; to delete backwards and add \#tag.
;; TODO Why do we need evil for this? Any other value for having evil in minibuffer?
(setq evil-want-minibuffer t)

;; Don't use locate, only consult-locate.
(setq consult-locate-args "mdfind -name") ;; Instead of `locate`
;; Would be better to have the below, but need to figure out how to do it.
;; (setq consult-locate-args (concat "mdfind " args " | grep -v -e /bak/ -e archive"))

;; Spelling
(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--sug-mode=ultra"))

;; suggested by ai
;; Initially, toggle spell-checking on
;; (add-hook 'text-mode-hook #'flyspell-mode)
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Commented on [2023-04-30 Sun]
;; (remove-hook 'text-mode-hook #'spell-fu-mode) ; Focus on writing, not spelling.
(setq
   ispell-personal-dictionary "~/.doom.d/aspell.en.pws"
   ;; ispell-program-name "hunspell"
   )

;; [2023-01-17 Tue] I think I fixed this by adding "*archive" to to gitignore global and using ripgrep for searching.
;; (setq projectile-indexing-method 'native)
;; (add-to-list 'projectile-globally-ignored-file-suffixes ".org_archive")
;; (add-to-list 'projectile-globally-ignored-directories "*bak")
;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile

;; ‘nil’ sets major mode to equal previous current buffer.
(setq-default major-mode 'org-mode)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximize frame at startup

(defvar dark-theme  'gav-one)
(defvar light-theme 'gav-one-light)
(defun toggle-theme ()
  "Toggle between my light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) dark-theme)
      (load-theme light-theme)
    (load-theme dark-theme)))

;; Make text easier to read by increasing space between lines.
(add-hook 'org-mode-hook (lambda () (setq line-spacing 10)))

;; Use word wrap in all buffers that minor mode message-mode.
;; https://blog.jethro.dev/posts/migrating_to_doom_emacs/
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(setq mixed-pitch-variable-pitch-cursor nil)

  ;; Don't spread text across the entire screen.
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  visual-fill-column-center-text t
  visual-fill-column-width 100
)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(setq
  doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
  doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 18)
  doom-serif-font (font-spec :family "DejaVu Serif")
)

(setq
    org-priority-faces '((?A . (:foreground "dim grey"))
                        (?B . (:foreground "dim grey"))
                        (?C . (:foreground "dim grey")))
    org-todo-keyword-faces
    '(
            ("DOING" :foreground "grey40" :weight bold :family "DejaVu Sans Mono")
            ("ASSIGNED" :foreground "grey40" :weight bold :family "DejaVu Sans Mono")
            ("TODO" :foreground "#98be65" :weight bold :family "DejaVu Sans Mono")
            ("WIP" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
            ("DONE" :foreground "grey25" :weight bold :family "DejaVu Sans Mono")
            ("PENDING" :foreground "DarkGreen" :weight bold :family "DejaVu Sans Mono")
            ("PAUSED" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
            ("[ ]" :foreground "dim grey")
            ("[X]" :foreground "grey25")
            ("CANCELLED" :foreground "grey25" :weight bold :family "DejaVu Sans Mono"))
   )

(custom-theme-set-faces
  'user
  ;; Use only two alternating colors for heading.
  '(org-level-1 ((t (:foreground "systemTealColor" :height 1.15))))
  '(org-level-2 ((t (:weight bold :foreground "systemBrownColor"))))
  '(org-level-3 ((t (:foreground "systemTealColor"))))
  '(org-level-4 ((t (:foreground "systemBrownColor"))))
  '(org-level-5 ((t (:foreground "systemTealColor"))))
  '(org-level-6 ((t (:foreground "systemBrownColor"))))

  ;; Remove bold from links.
  ;; "pink1" is here in search of a solution that would undefine the color on a link
  ;; and inherit.
  '(link ((t (:weight normal :underline "grey37" :foreground "pink1"))))
)

; Note that filenames containing `archive` anywhere in the path are excluded.
(setq consult-ripgrep-args "rg -g !\*bak -g !\*archive\* --sortr path --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --multiline --type org .")

(defun gh/consult-ripgrep-org-roam-tag ()
  "Keybinding quick access to consult-ripgrep in the org-roam directory.
Double press search edits to search with escaped tag: \\#tag"
  (interactive)
  (if (minibufferp)
      (progn
        (backward-delete-char 1)
        (insert "\\#"))
    (consult-ripgrep org-roam-directory)))

(defun gh/clone-indirect-buffer-vertically ()
  (interactive)
  (clone-indirect-buffer nil 1)
  (+evil/window-move-right))

(defun gh/org-open-journal ()
  (interactive)
  (org-roam-dailies-goto-today)
  (evil-goto-first-line)
  (search-forward "* #journal")
  (org-tree-to-indirect-buffer)
  (evil-goto-line)
  )

(defun gh/org-roam-toggle-exclude ()
  "Toggle excluding the current node from the Roam db."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (if (gh/get-org-property-at-point "ROAM_EXCLUDE")
        (progn
            (org-roam-property-remove "ROAM_EXCLUDE" "t")
            ;; TODO Sync db?
            (message "Node included"))
      (progn
        (org-roam-property-add "ROAM_EXCLUDE" "t")
        ;; TODO Sync db?
        (message "Node exluded")))))

(defun gh/get-org-property-at-point (property)
  (let ((properties (org-entry-properties nil property)))
    (message (cdr (assoc property properties)))))

(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert roam node without opening it."

  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args))
  (evil-insert-state)
  )

(setq org-agenda-custom-commands
      '(("h" . "Pending + Name tag searches") ; describe prefix "h"
        ("hk" search "#pending")))

;; (setq
    ;; org-agenda-window-setup 'reorganize-frame
    ;; ;; (search category-keep)
    ;; org-agenda-show-future-repeats 'next ;; Shows only the first future repeat.
    ;; org-agenda-skip-deadline-if-done t
    ;; org-agenda-skip-scheduled-if-done t
    ;; org-agenda-skip-timestamp-if-done t
    ;; org-agenda-start-on-weekday 0
    ;; org-agenda-custom-commands
    ;;     '(("d" "Today's Tasks"
    ;;         ((agenda "" ((org-agenda-span 1)
    ;;                 (org-agenda-overriding-header "Today's Tasks")))))
    ;; org-agenda-custom-commands))

(defun gh/ledger-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun dlukes/ediff-doom-config (file)
  "ediff the current config with the examples in doom-emacs-dir

There are multiple config files, so FILE specifies which one to
diff.
"
  (interactive
    (list (read-file-name "Config file to diff: " doom-private-dir)))
  (let* ((stem (file-name-base file))
          (customized-file (format "%s.el" stem))
          (template-file-regex (format "^%s.example.el$" stem)))
    (ediff-files
      (concat doom-private-dir customized-file)
      (car (directory-files-recursively
             doom-emacs-dir
             template-file-regex
             nil
             (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))

(map!
     "s-x"       'undefined ; execute-extended-command
     "C-x b"     'undefined ; switch-to-buffer
 :n  "O"         'undefined ; evil-open-above
 :ni "C-d"       'undefined ; evil-scroll-down
     "M-d"       'undefined ; kill-word
     "M-z"       'undefined ; zap-to-char. Using SPC d f /char/
     "s-:"       'undefined ; iSpell
     "s-e"       'undefined ; isearch-yank-kill
     "C-SPC"     'undefined ; set-mark-command
    "C-M-<return>" 'undefined ; org insert subheading. Used by magnet

     ; Using undo-fu package mapped for Mac consistency to to s-z and S-s-z.
     "C-/"       'undefined ; undo-fu-only-undo. When would I need this?
     "C-?"       'undefined ; undo-fu-only-redo. When would I need this?
                            ; This is assigned thru Maestro as "Open Recent"
     "s--"       'undefined ; doom/decrease-font-size (use C-- instead.
                            ; text-scale-increase)
     "s-="       'undefined ; doom/increase-font-size (use C-= instead)
     "C-x <right>" 'undefined ; next-buffer
     "C-x <left>" 'undefined ; previous-buffer

 :leader ":"     'undefined ; M-x
 :leader "."     'undefined ; counsel-find-file. SPC ff
 :leader "f D"   'undefined ; doom/delete-this-file. SPC f d
 :leader "X"     'undefined
 :leader "b N"   'undefined
 :leader "b r"   'undefined ; revert-buffer. Risky. Access by M-x only.
 :leader "u"     'undefined ; Universal argument
 )

(map!
  "H-s"         (cmd! (find-file (expand-file-name "stack.org" org-roam-directory)))
  "H-c"         (cmd! (find-file "/Users/gavinhughes/.doom.d/config.org"))
  "H-\\"        'toggle-theme
  "H-y"         'doom/delete-this-file
  "H-j"         'gh/org-open-journal

   ;; Movement
   :n "$"                  'end-of-visual-line
   :n "j"                  'evil-next-visual-line
   :n "k"                  'evil-previous-visual-line
      "M-<left>"           'backward-word
      "M-<right>"          'forward-word
      "M-s-l"              'avy-goto-char
      "M-s-;"              'avy-goto-char-2
   :i "s-l" "<escape>la" ;; Step forward over single chars without leaving insert mode
   :i "s-h" "<escape>ha" ;; Step back over...
   :i "s-S" "<escape>0i" ;; Jump to col 0. Useful in org mode

   :i "M-o"  'evil-execute-in-normal-state

  :ni "M-."                'better-jumper-jump-forward
  :ni "M-,"                'better-jumper-jump-backward

  ;; Text
      "M-c"                'capitalize-region ; Overwrites capitalize-word (use ~)

  ;; Special characters
  :i "M--" "–" ;; m-dash. Consistent with Mac.

  ;; Buffers
  "s-n"            '+default/new-buffer
  "M-s-k"          'kill-current-buffer
  "M-s-K"          'kill-buffer-and-window
  "s-,"            '+vertico/switch-workspace-buffer
  "M-s-,"          'consult-buffer
  "M-s-]"          'next-buffer
  "M-s-["          'previous-buffer
  "s-p"            'ps-print-buffer-with-confirmation
  "s-;"            'org-roam-node-find
  "M-s-s"          (cmd! (save-buffer) (kill-current-buffer))

  ;; Search
  "s-\\"            'consult-ripgrep
  "M-s-\\"          'gh/consult-ripgrep-org-roam-tag

  ;; Windows
  "s-'"            'evil-window-next
  "s-\""           'evil-window-prev
  "C-S-M-<return>" 'toggle-frame-fullscreen
  ;; Doesn't work in emacs-mac. All frames are maximized.
  ;; "C-M-<return>" 'toggle-frame-maximized

  ;; Workspaces.
  ;; :n assignment is in the package. Add :i.
  ;; [2022-05-22 Sun] Just pulled out the :ni to make this work in the agenda. Any issues??
  "s-1"        '+workspace/switch-to-0
  "s-2"        '+workspace/switch-to-1
  "s-3"        '+workspace/switch-to-2
  "s-4"        '+workspace/switch-to-3

  ;; Org-ai
  "C-c a a"    'gh/orgai-file-append

  ;; Other
  :ni "s-O"       'evil-open-above
      "s-<up>"    '+evil/insert-newline-above
      "s-<down>"  '+evil/insert-newline-below
      ;; Took this out because conflict with minibuffer immediate done on selection.
      ;; "M-<return>"  '+default/newline-below
  ;; was "H-<return>" with the following note:
   ;; todo. write about the reason for this. Has to do with org mode conflict 's-<return>'
  ;; :ni "s-<return>" (cmd! (message "Use <H-return>"))
  :niv "C-u"       'universal-argument

  ;; Make `$` behave same as in :normal.
  :v  "$"         (cmd! (evil-end-of-line) (evil-backward-char) (evil-forward-char))

  ;; Leaders – place last, otherwise errors.
  :leader "f m"   'doom/move-this-file
  :leader "b n"   'rename-buffer
  :leader "b c"   'gh/clone-indirect-buffer-vertically
  :leader "j d"   'dired-jump
  :leader "j j"   (cmd! (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/OrgNotes/Roam/Journal.org"))
  :leader "f d"   'doom/delete-this-file
  :leader "q f"   'delete-frame

  ;; Git
  :leader "g f d"   'magit-diff-buffer-file

  ;; `m` Mac OS
  :leader "m m d"   '+macos/open-in-default-program
  :leader "m m o"   'reveal-in-osx-finder

  ;; `t` Toggle
  :leader "t v"   'visual-fill-column-mode
)

(map! :map minibuffer-local-map
  "s-<return>" "C-; !"
  )

(map! :map org-mode-map
  :ni "C-<return>"  (cmd! (evil-org-org-insert-heading-respect-content-below))
  :ni "s-<return>"         (cmd! (+org/insert-item-below 1))
  :ni "S-s-<return>"     (cmd! (+org/insert-item-above 1))
  :ni "M-s-<return>"       (cmd! (org-insert-subheading 1) (evil-insert 1))
  ;; Insert a heading while currently working a bullet list
  :nie "C-M-s-<return>"     (cmd! (org-previous-visible-heading 1)
                                  (+org/insert-item-below 1))

  "M-s-SPC"            'org-capture

  :n "z n"             'doom/toggle-narrow-to-buffer ; org-tree-to-indirect-buffer
                                                     ; The original might be better?

  "H-n"                'org-next-visible-heading
  "H-p"                'org-previous-visible-heading
  "H-r"                (cmd! (+org/refile-to-file nil "daily.org"))
  "H-R"                '+org/refile-to-file
  ;; "H-a"                'org-archive-subtree
  "H-a"                'gh/open-or-pop-to-agenda

  "C-<"                'org-do-promote
  "C->"                'org-do-demote
  "C-M-<"              'org-promote-subtree
  "C-M->"              'org-demote-subtree

  "s-k"                'org-insert-link

  "C-M-y"              'org-download-screenshot ; paste
  "C-M-S-y"            'org-download-yank

  ;; Quickly get done Todo states
  ;; This is anti-pattern but efficient
  "H-l"  "C-c C-t d" ; DOING
  "H-k"  "C-c C-t o" ; DONE
  ;; "H-'"

  :niv "s-j"        'org-todo

  :leader "i d"     'gh/org-time-stamp-inactive
  :leader "i c"     'gh/org-insert-checkbox
  :leader "m -"     'org-toggle-item
  :leader "m m S"   'gh/yank-safari-front-url
  :leader "m m s"   'gh/org-insert-safari-front-link

  ;; :leader "a a"   'gh/set-org-agenda-all-files
  ;; :leader "a c"   'gh/set-org-agenda-crowley-files
  )

(map!
    "H-,"         'org-roam-dailies-goto-today
    "H-."         (cmd! (find-file (expand-file-name "daily.org"
                        (expand-file-name org-roam-dailies-directory org-roam-directory))))
    "H-d"         'org-roam-dailies-goto-date
    "H-["         'org-roam-dailies-goto-previous-note
    "H-]"         'org-roam-dailies-goto-next-note

  :leader "SPC"   'org-roam-node-find  ; Also on "s-;"
)

(map! :map org-roam-mode-map
    ;; Add :n to override assignment in +workspaces
        "<f7>"        'org-tags-view
        "<f9>"        'org-agenda-list

        "s-I"         'org-roam-node-insert

    ;; `r` org-roam
    :leader "r i"     'org-roam-node-insert
    :leader "r I"     'org-roam-node-insert-immediate
    :leader "r b"     'org-roam-buffer-toggle
    :leader "r x"     'gh/org-roam-toggle-exclude
)

(map! :map which-key-mode-map
    "M-x <right>"   'which-key-show-next-page-cycle
    "C-x <right>"   'which-key-show-next-page-cycle
    "M-x <left>"   'which-key-show-previous-page-cycle
    "C-x <left>"   'which-key-show-previous-page-cycle

    ;; No further pages here.
    ;; :leader "<right>"   'which-key-show-next-page-cycle
    ;; :leader "<left>"    'which-key-show-previous-page-cycle
    )

(after! ccls
  (setq ccls-executable "~/bin/ccls"
        compile-command (concat "g++ " "\"" (buffer-file-name) "\""))
  (set-lsp-priority! 'ccls 0))

(setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;; (define-key c++-mode-map [f5] #'compile)

(defun gh/compile ()
  (interactive)
  (setq compile-command (concat "g++ " "\"" (buffer-file-name) "\""))
  (compile compile-command))


(map! :map cpp-mode-map
  :leader "c c" 'gh/compile
  )

(map! :map elixir-mode-map
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 :i "s-:"   (cmd! (insert "=> "))
 :i "M-s-." (cmd! (insert "|> "))
 )

(defun  gh/load-and-run ()
  (interactive)
  (haskell-process-load-or-reload)
  (evil-window-next 0)
  ;; (haskell-interactive-mode-history-previous 1)
  )

(map! :map haskell-mode-map
 :i "C-M-;" (cmd! (insert "-> "))
 :i "C-M-:" (cmd! (insert "<- "))
 :i "C-M-=" (cmd! (insert "=> "))
 :i "C-M-+" (cmd! (insert "<= "))

 ; Not being scoped to haskell-mode. Why?
 ; :leader "m l" 'gh/load-and-run
 )

(map! :map ledger-mode-map
      "C-c C-l" 'ledger-mode-clean-buffer
      "C-c C-i" 'gh/ledger-insert-date)

(map! :map markdown-mode-map
    ;; Make m-dash behavior consistent with Mac.
    "M--" 'undefine

    "M-s-<return>"  'markdown-insert-list-item
 :i "M--" "–"
;; Errors on markdown-insert-list-item
;;  :ni   "s-<return>" (cmd! (evil-open-below 1) (markdown-insert-list-item))
 )

(defun gh/org-time-stamp-inactive ()
  (interactive)
  (org-insert-time-stamp (current-time) nil 1))

(defun gh/org-insert-checkbox ()
  "Insert a checkbox list item."
  (interactive)
  (unless (sp-point-in-blank-line)
    (evil-insert-newline-below))
  (insert "- [ ] ")
  (evil-insert-state))

(setq
  org-directory "/Users/gavinhughes/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents/OrgNotes"
  org-archive-location "archive.org::* From %s"
  org-attach-id-dir (concat org-directory "attachments/")
  org-ellipsis " ▼ "
  org-cycle-separator-lines 3
  org-special-ctrl-k t ; What's this?
  ;; Not working 7/13/21
  org-ctrl-k-protect-subtree t
  org-blank-before-new-entry '((heading . nil)
                               (plain-list-item . nil))
  org-appear-trigger 'on-change)
    ;; Other options: https://github.com/awth13/org-appear/blob/master/org-appear.el

(setq auto-save-timeout 30)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(add-hook 'org-mode-hook 'org-fragtog-mode) ; toggle preview when point enters fragment

(require 'org-download)
(after! org
  (setq
    org-download-method 'attach
    org-download-timestamp "%Y%m%d-%H%M%S_"
    org-image-actual-width 300
    org-download-delete-image-after-download 1 ; Delete temp image after download
    org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s"
    org-download-annotate-function #'gh/dont-annotate) ; Don't insert any property info above the link.
  )
(defun gh/dont-annotate (link) "")

(use-package! org-mac-link
  ;; Current version of Outlook doesn't support direct links to messages.
    :after org
    :config
    (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
    (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
    (map! :map org-mode-map          "C-c g"  #'org-mac-grab-link))

;; Logseq
;; (setq org-logseq-dir "~/Library/Mobile Documents/com~apple~CloudDocs/OrgNotes/Roam/")
;; (setq org-agenda-files (directory-files "~/iCloud/OrgNotes" nil "."));;)"~/iCloud/OrgNotes/")
;; (setq org-agenda-inhibit-startup t)
;; (setq org-agenda-dim-blocked-tasks nil)

;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

;; (setq org-roam-capture-templates
;;       '(;; ... other templates
;;         ;; bibliography note template
;;         ("r" "bibliography reference" plain "%?"
;;         :target
;;         (file+head "references/${citekey}.org" "#+title: ${title}\n")
;;         :unnarrowed t)))

;; Don't think this required now. [2023-04-30 Sun]
;; (add-hook 'org-mode-hook #'org-appear-mode)

(after! org
  ;; (load-directory! "my/org-mode")
 (vi-tilde-fringe-mode -1)
 (setq
    company-idle-delay nil ;; no autocompletion
    org-hide-emphasis-markers t
    spell-fu-mode nil

    org-todo-keywords
    '(
        (sequence
        "TODO(u)"
        "[ ](c)"
        "|"
        "DOING(d)"
        "ASSIGNED(s)"
        "DONE(o)"
        "[X](x)"
        "CANCELLED(l)"
        )
        (sequence
        "WIP(w)"
        "PENDING(p)"
        "PAUSED(a)"
        "|"
      ))))

(provide 'sqlite)

(setq org-roam-v2-ack t)
(setq org-roam-directory org-directory)
(setq org-roam-dailies-directory "journals/")
(org-roam-db-autosync-mode)

;; (after! org-roam (load! "~/.doom.d/modules/org-roam-logseq.el"))

(setq org-roam-dailies-capture-templates '(("d" "default" entry
                                            "* %?"
                                        :target (file+head
"%<%Y-%m-%d>.org"
"#+TITLE: %<%A, %-m/%-d/%y>
#+STARTUP: overview
–––
- [[https://crowley-cpt.deltekenterprise.com/cpweb/cploginform.htm?system=CROWLEYCONFIG][Timesheet]]
- code: VOPAD2
–––
"))))

(setq org-roam-capture-templates
    '(("d" "default" plain "%?"
        :target (file+head "pages/${slug}.org" "#+TITLE:   ${title}\n#+STARTUP: overview\n–")
        :unnarrowed t)))

(org-ai-global-mode 1) ; Make it work everywhere.
(add-hook 'org-mode-hook #'org-ai-mode) ; And always on in org.
; See https://platform.openai.com/docs/models
(setq org-ai-default-chat-model "gpt-3.5-turbo")
(setq org-ai-openai-api-token (getenv "OPENAI-API-TOKEN"))

(defun gh/orgai-file-append ()
  "Inserts AI code block at the end of ~/orgai/orgai.org"
  (interactive)
  (setq file "~/orgai/orgai.org")
  (setq cur-window (selected-window))
  (split-window-right)
  (other-window 1)
  (find-file file)
  (goto-char (point-max))
  (newline)
  (insert "#+begin_ai\n[ME]:\n\n#+end_ai")
  (previous-line)
  (evil-insert-state)
  (recenter-top-bottom))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'paredit-mode-hook (lambda () (evil-paredit-mode +1)))

;; [2023-04-30 Sun]
(add-hook 'org-mode-hook (lambda () (flyspell-mode -1)))
(add-hook 'org-mode-hook (lambda () (flyspell-lazy-mode -1)))

(use-package! chatgpt
  :defer t
  :config
  (unless (boundp 'python-interpreter)
    (defvaralias 'python-interpreter 'python-shell-interpreter))
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'bottom :size .5 :ttl nil :quit t :modeline nil)
  :bind ("C-c q" . chatgpt-query))
