(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d1c002d1c557e1b0b785afadf326adc0a1e2778908b7b9c4c97eeae4772deab" "101c9e913c8ed6780b2abbf428d53f0067204bc3a742d1c25019995829798e37" "09b004f23da806ebe50b4631652982900440fa613e80f6b5b4f070e2376bb96c" "5f1ebefef0ec3ad5676f5c8e1b3892ea146406f3619f4afe3081efb0d0fc1f8b" "a2ee13df58cea4fa095456cb860dccf81955de729838b251cec2174bf35a3f82" "bffbc2a0eac90a11e5d9e58b5f8aeb97240884ee836ba5f032bb07ebede17537" "61a992d0df80c8d88efff15826442a5bc1c4734dbcfcc6dc5363e2eeab256701" "f1317ab19d449da705ed140cd76654c7aa53ae8c5fe551210b0692bc43368c17" "2ad0583c3af1ac5493255724f729a00212b1470db58760e486a6d9bad550752b" "8ea9492542c8ce8462490f9ea764c3fc8ed51441e1de6acaa628215dfa0dd109" "e8e6ff6d9500cdbbe3c58c4554240131f157ff1b9e130336f66ace4351445cef" "44e3403aae214d6f3ecac09be996ad18182358066d3756e7656800b30689a653" "2e3dd72101f4a4d0fbc9b7c91e67cbd6bc1f937edf584727fdee6f4452b146d2" "79f08b4d9c2c4630ed6148be37744dcd1759904283725764575a1332280a56c6" "69c2111d77319055ce78ad4f6d73bbf465077584f4738730bef04206f0d3d140" "c9c3380ce4ddabc5284d5bb0283f9015f1196068887f40fe4ce596c608a82d0a" "46968cd0e07154315dffcd9afaf88868396ffa4a473441d079d508903f9b2a7c" "57fa4d2a35e5f3f30f43e56d4e7298af2ffe001909763b6843974c7dfeb51e32" "bd9caea039e97f9d4be0f6a5fc38edbd5238ce7c681e0aa324c9c4a3ff54d324" "2b39972135accb45d7839c3a2dcf599e9cd1d0c5a79349b1bfff3ace27a98d82" "1a5a35e51b0c314377a34e27be6b9342b2c447303636e65d1b23e932d46bc877" "e3ffcb9123c14cb36bbac983eb361b820273a631a0e38f56123b615ddec0d28a" "42cb33a6c884256d5484352a8ef5e6e33460179d20ef3f969ebd7c0813e8b8c2" "138526c53416e43971e6296196962db3a7151e4d901ef238a5714764a96bfa29" "6ea1dafb2d7e09cc840ab94e314bddf777bfc6d0b4242ebd6507d5d372f5c6b7" "ee07ee80ecfb1af9b8fcbe2e30336d835c3e81017f61ccd8a7f706bb2ed1c9e3" "eaaf013969ee938f1ac07c3f819feeb9afb9ea846e291c7f13532ee9f1cb42a6" "cd85c145606d564da8e89a8e77d612317142651af89a176f71a163d64af73207" "63f13a3499d6a491a0b543a3cd371056a2c731cf9eb9849ecca562dcdb4c5d93" "4e82fe6fd80af7bf147b60d6185d7b9a70376c8e1ef393bed25abde496b3e2bf" "393d946e55ffcd32f5288d00a9feb34e4e95be4cecec94fcee3790eb7e3d9b81" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(ledger-reports
   '(("Annulment - reg" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Library/Mobile\\ Documents/com\\~apple\\~CloudDocs/Dauin\\ Point/Finance/Accounting/ledger/main.ledger reg \"Expense\" and %case=2015-15066")
     ("Annulment - bal" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Library/Mobile\\ Documents/com\\~apple\\~CloudDocs/Dauin\\ Point/Finance/Accounting/ledger/main.ledger bal \"Expense\" and %case=2015-15066")
     ("Spec Perf - reg" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Library/Mobile\\ Documents/com\\~apple\\~CloudDocs/Dauin\\ Point/Finance/Accounting/ledger/main.ledger reg \"Expense\" and %case=2018-15313")
     ("Spec Perf - Bal" "ledger [[ledger-mode-flags]] -f /Users/gavinhughes/Library/Mobile\\ Documents/com\\~apple\\~CloudDocs/Dauin\\ Point/Finance/Accounting/ledger/main.ledger bal \"Expense\" and %case=2018-15313")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(safe-local-variable-values
   '((eval progn
      (visual-line-mode -1)
      (setq truncate-lines t))))
 '(warning-suppress-types '((comp) (comp) (comp)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:weight normal :underline "grey37" :foreground "pink1"))))
 '(org-level-1 ((t (:foreground "systemTealColor" :height 1.15))))
 '(org-level-2 ((t (:weight bold :foreground "systemBrownColor"))))
 '(org-level-3 ((t (:foreground "systemTealColor"))))
 '(org-level-4 ((t (:foreground "systemBrownColor"))))
 '(org-level-5 ((t (:foreground "systemTealColor"))))
 '(org-level-6 ((t (:foreground "systemBrownColor")))))
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'narrow-to-region 'disabled nil)
