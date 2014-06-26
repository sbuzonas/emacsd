;; Ignore any errors within this file and continue to attempt starting emacs.
;; Errors here are likely to be garbage added by a bug in a third party script.
(ignore-errors "local")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote bully))
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(circe t)
 '(clean-modes t)
 '(compilation-message-face (quote default))
 '(convenience t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tango-2)))
 '(custom-safe-themes (quote ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "0449a71c940c57f767774e30d7bf28b64456f431510d8cde29e86657a2602df6" "eead1779f4b497bf3df2f66b9209782c4d8cb896f3a45de049dc4218311b9e3b" "d63be37656ec4837b98780d9144239a7898bd6e3511583a8bc2c634c16687f36" "81e530e0d46ee1bf8cfdd58124cfd0df8c391753d6b29bab929392782a2b2dd2" "9a60d8bf511d915a08aa16f97bf2c8b11d55a54ef32118424db7d73d9d7d0401" "22bf74c2702369cbacc0a2a54afc0719cb06e5bd9db464e55a7f58f117ebd388" "7252c495b82c37f219f3f308d9353d533a930a69d0c3d0feb44263b4f086ac82" "31c6f4997e5af3aca46e98af2e34415f66796da87655be2152274a2244a97007" "bc2a933966724faef466dee08ede7eb4328894c9b369967688e865215d4f6a4f" "a4368d0d9d25d658dadfcf2933a3e38ff6314e482f541f30b79a25a5990d9c31" "5b6a7f2a00275a5589b14fa23ff1699785d9f7c1722ee9f79ec1b7de92fa0935" "fc7b606c048bff33dd4fc1c1814c46ec24e889cbbf30643e53095015f9361c3a" "985c570ce713a74e08e8aae8b7a35cf1a4bb89457ac629c5136a6c673af10e6d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4")))
 '(dired t)
 '(editorconfig t)
 '(fci-rule-color "#383838")
 '(git t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(ido t)
 '(ido-ubiquitous-command-overrides (quote ((disable exact "execute-extended-command") (enable prefix "wl-") (enable-old prefix "Info-") (enable exact "webjump") (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'") (disable prefix "org-") (disable prefix "magit-") (disable prefix "tmm-") (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit-theme\\)-theme\\'"))))
 '(ido-ubiquitous-function-overrides (quote ((disable exact "read-file-name") (disable exact "read-file-name-internal") (disable exact "read-buffer") (disable exact "gnus-emacs-completing-read") (disable exact "gnus-iswitchb-completing-read") (disable exact "grep-read-files") (enable exact "bookmark-completing-read") (enable-old exact "webjump-read-choice") (enable-old exact "webjump-read-url-choice") (disable exact "isearchp-read-unicode-char") (disable exact "org-completing-read") (disable exact "org-completing-read-no-i") (disable exact "org-iswitchb-completing-read") (disable exact "org-icompleting-read") (enable exact "read-char-by-name") (disable exact "Info-read-node-name") (disable exact "tmm-menubar"))))
 '(large-file-warning-threshold nil)
 '(magit-use-overlays nil)
 '(org t)
 '(org-agenda-files nil)
 '(php t)
 '(php-completion-file "/Users/sbuzonas/.emacs.d/php-completion-file")
 '(php-executable "/usr/local/bin/php")
 '(php-manual-path "/Users/sbuzonas/docs/php-chunked-xhtml")
 '(php-mode-coding-style (quote symfony2))
 '(send-mail-function nil)
 '(server t)
 '(server-kill-new-buffers t)
 '(shell t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smooth-scolling t)
 '(smtpmail-smtp-server "mail.carnegielearning.com")
 '(smtpmail-smtp-service 25)
 '(sr-speedbar-skip-other-window-p t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tmux t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
