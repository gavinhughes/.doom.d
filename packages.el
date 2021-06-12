;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Disable unwanted Doom packages.
(disable-packages! evil-snipe)

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! openwith)
(package! org-download)
(package! csv-mode)
;; (package! octave)
(package! osx-dictionary)
(package! visual-fill-column)
(package! dired+) ;; To reuse the same buffer when navigating the dir tree

;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
(package! dired-narrow)

(package! company-org-roam
   :recipe (:host github :repo "jethrokuan/company-org-roam"))
