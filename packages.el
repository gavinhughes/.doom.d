;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Docs on configuration:
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-doom

(disable-packages! evil-snipe)

(package! openwith)
(package! org-download)
  ;; Dependencies: pngpaste
(package! csv-mode)
(package! osx-dictionary)
(package! dired+)
  ;; To reuse the same buffer when navigating the dir tree
  ;; TODO Not yet implemented
(package! lorem-ipsum)
(package! exec-path-from-shell)
(package! reveal-in-osx-finder)
(package! auto-compile)
(package! dired-narrow)
  ;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
(unpin! org-roam)
;; (package! org-roam-ui)
;; (package! org-pandoc-import
;;   ;; https://github.com/tecosaur/org-pandoc-import
;;   :recipe (:host github
;;            :repo "tecosaur/org-pandoc-import"
;;            :files ("*.el" "filters" "preprocessors")))
(package! org-appear)
  ;; Auto-show markup symbols (=, *, etc) in org

;; org-mac-link removed from org. applescript portion of file not working.
;; Homepage: https://gitlab.com/aimebertrand/org-mac-link
;; Try installing it again locally if it gets updated. [2021-11-26 Fri]
;; ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#usingloading-local-packages

;; (package! org-roam-bibtex
;;   :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using bibtex-completion via the `biblio` module
;; (unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! powerthesaurus)
(package! org-mac-link)
(package! org-logseq :recipe (:host github :repo "llcc/org-logseq"))

;;;; Not currently used:
;; (package! org-anki)
