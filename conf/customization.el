;;** 57 (info "(emacs)Customization")

(when section-customization (message "57 Customization...")

;; inhibit the initial startup message in the `*scratch*' buffer
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ;; limit serving to catch infinite recursions for you before they
;; ;; cause actual stack overflow in C, which would be fatal for Emacs
;; (setq max-lisp-eval-depth (* 40 max-lisp-eval-depth))  ; 40 * 400 (default)

;; ;; limit on number of Lisp variable bindings & unwind-protects
;; (setq max-specpdl-size (* 40 max-specpdl-size))  ; 40 * 1 M (default)

;; ;; speed up things by preventing garbage collections
;; (setq gc-cons-threshold (* 40 gc-cons-threshold))  ; 40 * 400 KB (default)

;; make Gnus fast
(setq gc-cons-threshold 3500000)
    ; from http://www.emacswiki.org/emacs/GnusSpeed

;; don't display messages at start and end of garbage collection (as it hides
;; too many interesting messages)
(setq garbage-collection-messages nil)


;;*** 57.3 (info "(emacs)Variables")

;; file local variables specifications are obeyed, without query -- RISKY!
(setq enable-local-variables t)

;; obey `eval' variables -- RISKY!
(setq enable-local-eval t)

;; record safe values for some local variables
(setq safe-local-variable-values
      '((TeX-master . t)
        (balloon-help-mode . -1)
        (flyspell-mode . t)
        (flyspell-mode . -1)
        (ispell-local-dictionary . "en_US")
        (ispell-local-dictionary . "fr_FR")
        (ispell-mode . t)
        (nuweb-auto-index-scrap)
        (nuweb-source-mode . "mercury")
        (nuweb-source-mode . "sql")
        (org-export-latex-title-command . "\\maketitle[logo=Forem]")))

;; Have a look at (info "(emacs)Directory Variables")


;;*** 57.4 Customizing (info "(emacs)Key Bindings")

;; the keys `C-c LETTER' are reserved for user functions

;; print the key bindings in a tabular form
;; [from http://www-xray.ast.cam.ac.uk/~gmorris/dotemacs.html]
(GNUEmacs
    (defun my-keytable (arg)
      "Print the key bindings in a tabular form."
      (interactive "sEnter a modifier string:")
      (with-output-to-temp-buffer "*Key table*"
        (let* ((i 0)
               (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                           "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                           "<return>" "<down>" "<up>" "<right>" "<left>"
                           "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                           "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                           "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                           "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-"
                           "_" "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":"
                           "\"" "<" ">" "," "." "/" "?"))
               (n (length keys))
               (modifiers (list "" "S-" "C-" "M-" "M-C-"))
               (k))
          (or (string= arg "") (setq modifiers (list arg)))
          (setq k (length modifiers))
          (princ (format " %-10.10s |" "Key"))
          (let ((j 0))
            (while (< j k)
              (princ (format " %-28.28s |" (nth j modifiers)))
              (setq j (1+ j))))
          (princ "\n")
          (princ (format "_%-10.10s_|" "__________"))
          (let ((j 0))
            (while (< j k)
              (princ (format "_%-28.28s_|"
                             "_______________________________"))
              (setq j (1+ j))))
          (princ "\n")
          (while (< i n)
            (princ (format " %-10.10s |" (nth i keys)))
            (let ((j 0))
              (while (< j k)
                (let* ((binding
                        (key-binding (read-kbd-macro (concat (nth j modifiers)
                                                             (nth i keys)))))
                       (binding-string "_"))
                  (when binding
                    (if (eq binding 'self-insert-command)
                        (setq binding-string (concat "'" (nth i keys) "'"))
                      (setq binding-string (format "%s" binding))))
                  (setq binding-string
                        (substring binding-string 0 (min (length
                                                          binding-string) 28)))
                  (princ (format " %-28.28s |" binding-string))
                  (setq j (1+ j)))))
            (princ "\n")
            (setq i (1+ i)))
          (princ (format "_%-10.10s_|" "__________"))
          (let ((j 0))
            (while (< j k)
              (princ (format "_%-28.28s_|"
                             "_______________________________"))
              (setq j (1+ j))))))
      (delete-window)
      (hscroll-mode)
      (setq truncate-lines t)))


;; You can get a list of all the disabled functions by typing
;; `M-: (let(lst)(mapatoms(lambda(x)(if(get x 'disabled)(push x lst))))lst) RET'


(defmacro rloop (clauses &rest body)
  (if (null clauses)
      `(progn ,@body)
    `(loop ,@(car clauses) do (rloop ,(cdr clauses) ,@body))))

(defun all-bindings ()
  (interactive)
  (message "all-bindings: wait a few seconds please...")
  (let ((data
         (with-output-to-string
           (let ((bindings '()))
             (rloop ((for C in '("" "C-"))       ; Control
                     (for M in '("" "M-"))       ; Meta
                     (for A in '("" "A-"))       ; Alt
                     (for S in '("" "S-"))       ; Shift
                     (for H in '("" "H-"))       ; Hyper
                     (for s in '("" "s-"))       ; super
                     (for x from 32 to 127))
                    (let* ((k (format "%s%s%s%s%s%s%c" C M A S H s x))
                           (key (ignore-errors (read-kbd-macro k))))
                      (when key
                        (push
                         (list k
                               (format "%-12s  %-12s  %S\n" k key
                                       (or
                                        ;; (string-key-binding key)
                                        ;; What is this string-key-binding?
                                        (key-binding key))))
                         bindings))))
             (dolist (item
                      (sort bindings
                            (lambda (a b)
                              (or (< (length (first a))
                                     (length (first b)))
                                  (and (= (length (first a))
                                          (length (first b)))
                                       (string< (first a)
                                                (first b)))))))
               (princ (second item)))))))
    (switch-to-buffer (format "Keybindings in %s" (buffer-name)))
    (erase-buffer)
    (insert data)
    (goto-char (point-min))
    (values)))


;;*** 57.5 The (info "(emacs)Syntax") Table

;; The syntax table contains information that tells Emacs how to operate on
;; text, words, sentences etc. It will make Emacs know enough about all the
;; symbols in a buffer. Syntax table is used for example for word motion
;; (`M-f'), spell-checking of words, expansion commands of abbrevs, etc.

;; See `C-h f current-word' and whether characters such as `-' and `_' are
;; considered part of the word (depending on the current major mode).

;; now '-' is not considered a word-delimiter
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?- "w")))

(message "57 Customization... Done"))
