(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" default))
 '(ledger-reports
   '(("bal" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Dropbox/Organizations/Dauin\\ Point/Finance/Accounting/ledger/main.ledger bal")
     ("reg" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Dropbox/Organizations/Dauin\\ Point/Finance/Accounting/ledger/main.ledger reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-todo-keywords nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
