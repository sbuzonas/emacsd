;;** 56 Other (info "(emacs)Amusements")

(when section-amusements (message "56 Other Amusements...")

(GNUEmacs
    ;; get rid of the Games in the Tools menu
    (define-key menu-bar-tools-menu [games] nil))

(message "56 Other Amusements... Done"))
