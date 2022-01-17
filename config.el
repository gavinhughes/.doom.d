;;; config.el -*- lexical-binding: t; -*-

;; TODO
;;
;; Change image paste directory structure for org image download
;;   set so that it works in the same directory structure as logseq?
;;   see image green propulsion file [2021-12-13 Mon]
;;
;;
;; Start the Emacs server from this instance so that all emacsclient calls are routed here.
;; (if (server-running-p ()) nil (server-start))

;; To reduce the risk of loading outdated byte code files, set load-prefer-newer
;; and enable auto-compile-on-load-mode as early as possible.
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq load-prefer-newer t)


;; These add load paths but don't load all files in them.
;; (add-load-path! "my/global")
;; (add-load-path! "modules")

(load! "my/global/functions.el")

;; Use word wrap in all buffers that minor mode message-mode.
;; https://blog.jethro.dev/posts/migrating_to_doom_emacs/
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(setq mixed-pitch-variable-pitch-cursor nil)

(setq
  shell-file-name "/opt/homebrew/bin/fish"
  mac-right-command-modifier 'hyper
  trash-directory "~/.Trash"
  confirm-kill-emacs nil
    ;; Disable exit confirmation

  user-full-name "Gavin Hughes"
  user-mail-address "gavhug@gmail.com"

  doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
  doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 18)
  doom-serif-font (font-spec :family "DejaVu Serif")

  doom-scratch-initial-major-mode 'org-mode
  undo-limit 80000000                         ; Raise undo-limit to 80Mb
  evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
  auto-save-default t                         ; Nobody likes to loose work, I certainly don't
  inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
  truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
  global-visual-line-mode t                   ; Visual line navigation everywhere.

  ;; ispell-program-name "hunspell"
  display-line-numbers-type 'visual

  ;; Finder "put back" is not supported. If desired, instructions are here
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  delete-by-moving-to-trash t


  ;; https://github.com/jschaf/esup/issues/54
  ;; work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  esup-depth 0
  )

(setq ispell-personal-dictionary "~/.doom.d/aspell.en.pws")

(setq projectile-indexing-method 'native)
(add-to-list 'projectile-globally-ignored-file-suffixes ".org_archive")
;; (add-to-list 'projectile-globally-ignored-directories "*bak")
  ;; Ignores aren't working.  Why?  [2021-12-28 Tue]
  ;; https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
(setq-default major-mode 'org-mode)
  ;; If set to ‘nil’, the major mode is taken from the previously current buffer.
(remove-hook 'text-mode-hook #'spell-fu-mode)
  ;; Focus on writing, not spelling.
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; Don't spread text across the entire screen.
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
(setq
  visual-fill-column-fringes-outside-margins nil
  visual-fill-column-center-text t
  visual-fill-column-width 100
)

;; Maximize frame at startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(map!
 ;; DON'T USE. Assigned in MacOS:
 ;;   C-M-<return>    Magnet maximize window
 ;;   C-M-<space>     Things quick capture

 ;; Undefine unused or reassigned chords
 :n  "O"         'undefined ;; evil-open-above
 :ni "C-d"       'undefined ;; evil-scroll-down
     "M-d"       'undefined ;; kill-word

;; Hypers – Get there quickly
;;   This might be better defined in their category locations than references
;;   with a master list in comments here.
"H-<return>"  '+default/newline-below
  ;; todo. write about the reason for this. Has to do with org mode conflict 's-<return>'
 ;; :ni "s-<return>" (cmd! (message "Use <H-return>"))
"H-a"         (cmd! (find-file "~/Desktop/stack.log"))
"H-c"         (cmd! (find-file "/Users/gavinhughes/.doom.d/config.el"))
"H-d"         'org-roam-dailies-goto-today
"H-t"         'toggle-theme
"H-["         'org-roam-dailies-goto-previous-note
"H-]"         'org-roam-dailies-goto-next-note


 "M-s-SPC" 'org-capture
 "C-M-;"      'yank-from-kill-ring

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
 "s-;"            'org-roam-node-find
 "M-s-s"          (cmd! (save-buffer) (kill-current-buffer))

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
     "M-<return>" '+evil/insert-newline-below
:niv "C-u"       'universal-argument

;; Make `$` behave same as in :normal.
 :v  "$"         (cmd! (evil-end-of-line) (evil-backward-char) (evil-forward-char))

 ;; Add :n to override assignment in +workspaces
     "<f7>"        'org-tags-view
     "<f9>"        'org-agenda-list

 ;; Undefine unused or reassigned chords
 :leader "X"     'undefined
 :leader "b N"   'undefined
 :leader "u"     'undefined ;; Universal argument

 ;; Leaders – place last, otherwise errors.
 :leader "a d"   (cmd! (org-insert-time-stamp (current-time) nil 1))
   ;; Change this to a function: 'org-time-stamp-inactive
 :leader "f m"   'doom/move-this-file
 :leader "b n"   'rename-buffer
 :leader "j d"   'dired-jump
 :leader "<"     '+ivy/switch-workspace-buffer
 :leader ","     'ivy-switch-buffer
 :leader "SPC"   '+ivy/projectile-find-file

 ;; `r` org-roam
 :leader "r r"     'org-roam-node-find
 :leader "r b"     'org-roam-buffer-toggle

 ;; `m` Mac OS
 :leader "m m d"   '+macos/open-in-default-program
 :leader "m m o"   'reveal-in-osx-finder
 :leader "m m S"   'gh/yank-safari-front-url
 :leader "m m s"   'gh/org-insert-safari-front-link

 ;; `t` Toggle
 :leader "t v"   'visual-fill-column-mode
 )

;; ORG-MODE

;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#usingloading-local-packages
;; Better way to load using packages.el?
;; (add-load-path! "my/org-mode")
;; (require 'org-mac-link)

(setq
  org-directory "~/iCloud/OrgNotes/"
  org-attach-id-dir (concat org-directory "attachments/")
  org-ellipsis " ▼ "
  org-cycle-separator-lines 3
  org-special-ctrl-k t
  ;; Not working 7/13/21
  org-ctrl-k-protect-subtree t
  org-blank-before-new-entry '((heading . nil)
                               (plain-list-item . nil))
   ;; https://stackoverflow.com/a/41969519/173162
  org-agenda-files (directory-files-recursively "~/iCloud/OrgNotes/" "\\.org$")
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

  ;; https://www.fromkk.com/posts/preview-latex-in-org-mode-with-emacs-in-macos/
  org-preview-latex-default-process 'dvisvgm
  org-format-latex-options '(:scale 2.0)
  org-startup-with-inline-images 1
  org-startup-with-latex-preview 1
    ;; Can be set per file with #+STARTUP: ‘inlineimages’ or ‘noinlineimages’
 )

(add-hook 'org-mode-hook (lambda ()
                           (setq
                            line-spacing 8
                              ;; Make text easier to read.
                              )))


;; BROKEN
;; For export to .md, .doc, etc.
;; https://github.com/tecosaur/org-pandoc-import
;; (use-package! org-pandoc-import :after org)

;; https://github.com/abo-abo/org-download/blob/master/org-download.el
(require 'org-download)
  ;; org-download is not great. Using the 'attach method, files are
  ;; inserted in the org-attach-id directory under the file property.
  ;; Limitations:
  ;; - Images will not insert unlist under a heading.
(after! org
  (setq
    ;; https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/
    org-download-method 'attach
    org-download-timestamp "%Y%m%d-%H%M%S_"
    org-image-actual-width 300
    org-download-delete-image-after-download 1
      ;; Delete temp image after download
    org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s"
    org-download-annotate-function #'gh/dont-annotate)
      ;; Don't insert any property info above the link.
  )
(defun gh/dont-annotate (link) "")


(add-hook 'org-mode-hook #'org-appear-mode)
(after! org
  ;; (load-directory! "my/org-mode")
 (vi-tilde-fringe-mode -1)
 (setq
    company-idle-delay nil ;; no autocompletion
    org-hide-emphasis-markers t
    spell-fu-mode nil

    org-todo-keywords
    '(
        ;;;; Removing these because they don't play well with logseq
        ;; (sequence
        ;; "[ ](j)"
        ;; "|"
        ;; "[X](k)"
        ;; )
        (sequence
        "TODO(u)"
        "|"
        "DONE(i)"
        "CANCELLED(l)"
        )
        (sequence
        "WIP(w)"
        "PENDING(p)"
        "PAUSED(a)"
        "|"
      ))
org-priority-faces '((?A . (:foreground "dim grey"))
                        (?B . (:foreground "dim grey"))
                        (?C . (:foreground "dim grey")))
org-todo-keyword-faces
'(
        ("TODO" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
        ("WIP" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
        ("DONE" :foreground "grey25" :weight bold :family "DejaVu Sans Mono")
        ("PENDING" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
        ("PAUSED" :foreground "dim grey" :weight bold :family "DejaVu Sans Mono")
        ("[ ]" :foreground "dim grey")
        ("[X]" :foreground "grey25")
        ("CANCELLED" :foreground "grey25" :weight bold :family "DejaVu Sans Mono"))
   ))

(map! :map org-mode-map
  :ni "s-<return>"         (cmd! (+org/insert-item-below 1))
  :ni "S-s-<return>"     (cmd! (+org/insert-item-above 1))
  :ni "M-s-<return>"       (cmd! (org-insert-subheading 1) (evil-insert 1))
  ;; Insert a heading while currently working a bullet list
  :nie "C-M-s-<return>"     (cmd! (org-previous-visible-heading 1) (+org/insert-item-below 1))

  ;; Should all Hypers be defined globally?
  "H-n"                'org-next-visible-heading
  "H-p"                'org-previous-visible-heading
  "H-r"                '+org/refile-to-current-file
  "H-R"                '+org/refile-to-file
  "H-a"                'org-archive-subtree
  ;; "s-."                'org-shiftright
  ;; "s->"                'org-shiftleft
  "H-l"                "C-u C-u C-c C-x C-l" ;; Preview all latex
  "H-L"                "C-u C-c C-x C-l" ;; Un-preview all latex
  "s-M"                'org-refile
  "C-M-y"              'org-download-screenshot
  "C-M-S-y"            'org-download-yank

  ;; Roam
  "s-I"                'org-roam-node-insert
  :niv "s-j"           'org-todo

)

(map! :map haskell-mode-map
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 )

;; Elixir and Phoenix
;;
(map! :map elixir-mode-map
 :i "M-s-;" (cmd! (insert "-> "))
 :i "M-s-:" (cmd! (insert "<- "))
 :i "s-:"   (cmd! (insert "=> "))
 :i "M-s-." (cmd! (insert "|> "))
 )


(map! :map markdown-mode-map
    "M--" 'undefine
 :i "M--" "–" ;; m-dash. Consistent with Mac.
;; Error on markdown-insert-list-item
;;  :ni   "s-<return>" (cmd! (evil-open-below 1) (markdown-insert-list-item))
 )

;; (after! org-roam
;;         :config
;;         (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;; Not working yet. And will probably be in the next release of v2
;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "p")
;;   (let ((args (cons arg args))
;;         (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                   '(:immediate-finish t)))))
;;     (apply #'org-roam-node-insert args)))

(setq org-roam-v2-ack t
      org-roam-directory "~/Library/Mobile Documents/com~apple~CloudDocs/OrgNotes/Roam"

      org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target (file+head "${slug}.org"
                                                         "#+TITLE:   ${title}\n#+STARTUP: show2levels")
                                      :unnarrowed t))
      org-roam-dailies-directory "daily"
      org-roam-dailies-capture-templates '(("d" "default" entry
                                            "* %?"
                                        :target (file+head
"%<%Y-%m-%d>.org"
"#+TITLE: %<%Y-%m-%d>

"))))

(use-package! websocket
    :after org-roam)
(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2#showing-the-number-of-backlinks-for-each-node-in-org-roam-node-find
;; (cl-defmethod org-roam-node-directories ((node org-roam-node))
;;   (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;       (format "(%s)" (car (f-split dirs)))
;;     ""))

;; (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
;;   (let* ((count (caar (org-roam-db-query
;;                        [:select (funcall count source)
;;                                 :from links
;;                                 :where (= dest $s1)
;;                                 :and (= type "id")]
;;                        (org-roam-node-id node)))))
;;     (format "[%d]" count)))

;; (setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")

;; ;; org-roam buffer
;; (setq org-roam-mode-section-functions
;;       (list ;; #'org-roam-backlinks-section
;;             ;; #'org-roam-reflinks-section
;;             ;; #'org-roam-unlinked-references-section
;;             ))
;; (add-to-list 'display-buffer-alist
;;              '("\\*org-roam\\*"
;;                (display-buffer-in-side-window)
;;                (side . right)
;;                (slot . 0)
;;                (window-width . 0.33)
;;                (window-parameters . ((no-other-window . t)
;;                                      (no-delete-other-windows . t)))))

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

(defun gh/ledger-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d"))
  )

(map! :map ledger-mode-map
      "C-c C-l" 'ledger-mode-clean-buffer
      "C-c C-i" 'gh/ledger-insert-date)


;; Experimental
;;
;; Open pdfs with emacs. Better would be PDF Expert. How to do that?
;; (setq org-file-apps (delq (assoc "\\.pdf\\'" org-file-apps) org-file-apps))
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

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

;; https://github.com/hlissner/doom-emacs/issues/581
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

;; ;; Remove bold from links
;; (doom-themes-set-faces nil
;;  '(default ((t (:weight normal, :underline t)))))

;; Remove bold from links
(custom-theme-set-faces
  'user
  '(org-level-1 ((t (:foreground "systemTealColor" :height 1.15)))) ;; systemBlueColor
  '(org-level-2 ((t (:weight bold :foreground "systemBrownColor")))) ;; systemBlueColor
  '(org-level-3 ((t (:foreground "systemTealColor")))) ;; systemBlueColor
  '(org-level-4 ((t (:foreground "systemBrownColor")))) ;; systemBlueColor
  '(org-level-5 ((t (:foreground "systemTealColor")))) ;; systemBlueColor
  '(org-level-6 ((t (:foreground "systemBrownColor")))) ;; systemBlueColor
  '(link ((t (:weight normal :underline "grey37" :foreground "pink1")))))
    ;; "pink1" is here in search of a solution that would undefine the color on a link
    ;; and inherit.


;; https://www.orgroam.com/manual.html#Org_002droam-Protocol
;; Installed. How to use it? [2021-12-13 Mon]
;; (require 'org-roam-protocol)

