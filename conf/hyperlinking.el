;;** 54 (info "(emacs)Hyperlinking") and Navigation Features ]

(when section-hyperlinking (message "54 Hyperlinking and Navigation Features...")

;; I use an excellent package called `webjump' to store my bookmarks. It
;; also has provisions for generating search strings for the search sites as
;; well.


;;*** HTML Tidy

(try-require 'tidy)

;; For other modes (like `html-helper-mode') simply change the variables
;; `html-mode-hook' and `html-mode-map' to whatever is appropriate e.g.

;; FIXME html-helper-mode should be in `auto-mode-alist' as well?
(defun my-html-helper-mode-hook ()
  "Customize my html-helper-mode."
  (tidy-build-menu html-helper-mode-map)
  (local-set-key (kbd "C-c C-c") 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-helper-mode-hook 'my-html-helper-mode-hook)


;;*** pass a URL to a WWW browser

;; display the current buffer in the default Windows WWW browser
(try-require 'browse-url)
(autoload 'browse-url-at-mouse "browse-url")

;; default browser started when you click on some URL in the buffer
(if window-system
    (if running-ms-windows
        (setq browse-url-browser-function 'browse-url-default-windows-browser)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program (executable-find "firefox")))
  (setq browse-url-browser-function 'w3m-browse-url))

;; ;; shortcut to view the current file in browser
;; (define-key html-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)

;; (setq browse-url-browser-function
;;       '(("file:///usr/share/doc/hyperspec/" . w3m-browse-url)
;;         ("emacswiki.org" . w3m-browse-url)
;;         ("lispdoc.com" . w3m-browse-url)
;;         ( "." . browse-url-firefox)))
;; that let me use w3m for EmacsWiki/Common Lisp documentation and
;; Firefox otherwise.

(defun rgr/browse (url)
  "If prefix is specified use the system default browser else use the
configured emacs one"
  (if current-prefix-arg
      (when url (browse-url-default-browser url))
    (if  url (browse-url url) (call-interactively 'browse-url))
    ))

(defun rgr/browse-url (&optional url)
  "browse the url passed in"
  (interactive)
  (setq url (or url
                (w3m-url-valid (w3m-anchor))
                (browse-url-url-at-point)
                (region-or-word-at-point)))
  (setq url (read-string (format "Url \"%s\" :" url) url nil url))
  (rgr/browse url))

(global-set-key (kbd "<f4>") 'rgr/browse-url)

;; Cursor on url. Hit f4 to open using your emacs browser (whatever
;; that is configured to) or C-u f4 to open in your desktop
;; browser (firefox here).
;; It also works in w3m buffers e.g in html rendered emails.
;; You might need to include thingatpt+.el


(defun get-tinyurl (long-url)
  "Gets URL and makes it short"
  (interactive "sLong URL: ")
  (let* ((tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))


;;*** Emacs-w3m

;; Emacs/W3 is dead, long live Emacs-w3m
;; Emacs-w3m is a terrific text-based web and file browser

;; You can obtain a snapshot from
;; http://cvs.namazu.org/emacs-w3m.tar.gz?view=tar

;; `w3m-browse-url' asks Emacs-w3m to browse a URL.

;; When JavaScript is needed or the "design" is just too bad, use another
;; browser: you can open the page in your graphical browser (at your own
;; risk) by hitting `M' (`w3m-view-url-with-external-browser').
;; For what "risk" means, please see: (info "(emacs-w3m)Gnus")

;; (for Win32, use the Cygwin version of the executable)
(setq w3m-command (executable-find "w3m"))
(when (and w3m-command
           (file-executable-p w3m-command))
(try-idle-require 'w3m)  ; w3m slows down the startup process dramatically
(eval-after-load 'w3m
    '(progn

;;**** 3.1 Browsing Web Pages

    ;; add key binding
    (global-set-key (kbd "C-c w w") 'w3m)

    ;; go ahead, just try it
    (defun my-w3m-goto-url ()
      "Type in directly the URL I would like to visit (avoiding to hit `C-k')."
      (interactive)
      (let ((w3m-current-url ""))
        (call-interactively 'w3m-goto-url)))

    (define-key w3m-mode-map (kbd "U") 'my-w3m-goto-url)

    ;; fix inappropriate key bindings for moving from place to place in a
    ;; page
    (define-key w3m-mode-map (kbd "<up>") 'previous-line)
    (define-key w3m-mode-map (kbd "<down>") 'next-line)
    (define-key w3m-mode-map (kbd "<left>") 'backward-char)
    (define-key w3m-mode-map (kbd "<right>") 'forward-char)

    (define-key w3m-mode-map (kbd "<tab>") 'w3m-next-anchor)

    ;; moving from page to page
    (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)


;;**** 3.5 Using Tabs

    (define-key w3m-mode-map (kbd "<C-tab>") 'w3m-next-buffer)
    (define-key w3m-mode-map [(control shift iso-lefttab)] 'w3m-previous-buffer)

    (defun w3m-new-tab ()
      (interactive)
      (w3m-copy-buffer nil nil nil t))

    (define-key w3m-mode-map (kbd "C-t") 'w3m-new-tab)

    (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)


;;**** 5.1 General Variables

    ;; send referers only when both the current page and the target page are
    ;; provided by the same server
    (setq w3m-add-referer 'lambda)

    ;; home page
    (setq w3m-home-page "http://www.emacswiki.org/")

    ;; number of steps in columns used when scrolling a window horizontally
    (setq w3m-horizontal-shift-columns 1)  ; 2

    ;; proxy settings
    (when (string= (upcase (system-name)) "PC3701")
      (eval-after-load "w3m"
        '(setq w3m-command-arguments
               (nconc w3m-command-arguments
                      '("-o" "http_proxy=http://proxy:8080"))))
                                        ; FIXME https_proxy for HTTPS support
      (setq w3m-no-proxy-domains '("local.com" "sysdoc")))


;;**** 5.2 Image Variables

    ;; always display images
    (setq w3m-default-display-inline-images t)

    ;; show favicon images if they are available
    (setq w3m-use-favicon t)


;;**** 5.4 Cookie Variables

    ;; functions for cookie processing
    (when (try-require 'w3m-cookie)
        ;; ask user whether accept bad cookies or not
        (setq w3m-cookie-accept-bad-cookies 'ask)

        ;; list of trusted domains
        (setq w3m-cookie-accept-domains
              '("google.com" "google.be"
                "yahoo.com" ".yahoo.com" "groups.yahoo.com"
                "www.dyndns.org")))

    ;; enable cookies (to use sites such as Gmail)
    (setq w3m-use-cookies t)


;;**** 5.14 Other Variables

    ;; functions convenient to access web search engines
    (when (try-require 'w3m-search)
      (global-set-key (kbd "C-c w s") 'w3m-search)
      (add-to-list 'w3m-search-engine-alist
                   '("teoma" "http://www.teoma.com/search.asp?t=%s" nil)))


    (defun google (what)
      "Use google to search for WHAT."
      (interactive "sSearch: ")
      (save-window-excursion
        (delete-other-windows)
        (let ((dir default-directory))
          (w3m-browse-url (concat "http://www.google.com/search?q="
                                  (w3m-url-encode-string what)))
          (cd dir)
          (recursive-edit))))
    (global-set-key (kbd "C-c g s") 'google)


    ;; list of content types, regexps (matching a url or a file name), commands
    ;; to view contents, and filters to override the content type specified at
    ;; first
    (setq w3m-content-type-alist
          (append '(("text/html" "\\.xhtml\\'" nil nil))
                  w3m-content-type-alist))

    ;; toggle a minor mode showing link numbers
    (when (try-require 'w3m-lnum)

      (defun my-w3m-go-to-linknum ()
        "Turn on link numbers and ask for one to go to."
        (interactive)
        (let ((active w3m-link-numbering-mode))
          (when (not active) (w3m-link-numbering-mode))
          (unwind-protect
              (w3m-move-numbered-anchor (read-number "Anchor number: "))
            (when (not active) (w3m-link-numbering-mode))
            (w3m-view-this-url))))

      (define-key w3m-mode-map (kbd "f") 'my-w3m-go-to-linknum)

      ;; enable link numbering mode by default
      (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)))))


;;**** 9.1 Turning Gnus into a web browser!

;; `nnshimbun' is a Gnus back end, but it is distributed with emacs-w3m, not
;; Gnus, exceptionally. `Nnshimbun' allows you to turn Gnus into an
;; exceptionally useful web browser. You can skim through the articles on a
;; newspaper's web server without having to see all the advertisement. You can
;; read articles in mailing list archives as if you were subscribed to the
;; list. You can also read submissions in bulletin boards, etc...


;;*** Web search

(when (and (try-require 'browse-url) t)
;;           (try-require 'url))

;; from Glenn Morris

    (defvar my-google-maxlen 200
      "Maximum string length of search term.")

    (defvar my-google-url "www.google.com"
      "Base URL for Google search.")

    (defvar my-google-groups-url "groups.google.com"
      "Base URL for groups Google search.")

    (defun my-google-search-region (prefix start end)
      "Create a search URL and send it to the web browser.
    With prefix argument, use groups URL."
      (interactive "P\nr")
      (if (> (- end start) my-google-maxlen)
          (message "Search string too long!")
        (let ((query (buffer-substring-no-properties start end)))
          (browse-url
           (concat "http://"
                   (if prefix (concat my-google-groups-url "/groups")
                     (concat my-google-url "/search"))
                   "?q=" (url-hexify-string query))))))

    (defvar my-url-maxlen 100
      "Maximum length of string to send to browser as URL.")

    ;; `find-file-at-point' does this, essentially
    (defun my-url-at-point (start end)
      "Send the highlighted URL to the web browser."
      (interactive "r")
      (if (> (- end start) my-url-maxlen)
          (message "URL too long!")
        (browse-url (buffer-substring-no-properties start end))))

;; (require 'url)
;;
;; (defvar google-search-maxlen 50
;;   "Maximum string length of search term.  This prevents you from accidentally
;; sending a five megabyte query string to Netscape.")
;;
;; (defun google-it (search-string)
;;   "Search for SEARCH-STRING on Google."
;;   (interactive "sSearch for: ")
;;   (browse-url (concat "http://www.google.com/search?client=xemacs&q="
;;                   (url-hexify-string
;;                     (encode-coding-string search-string 'utf-8)))))
;;
;; (defun google-search-selection ()
;;   "Create a Google search URL and send it to your web browser."
;;   (interactive)
;;   (let (start end term url)
;;     (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
;;         (progn
;;           (setq start (region-beginning)
;;                 end   (region-end))
;;           (if (> (- start end) google-search-maxlen)
;;               (setq term (buffer-substring start (+ start google-search-maxlen)))
;;             (setq term (buffer-substring start end)))
;;           (google-it term))
;;       (beep)
;;       (message "Region not active"))))



    (defun my-google-search ()
      "Prompt for a query in the minibuffer, launch the web browser and query
    Google."
      (interactive)
      (let ((query (read-from-minibuffer "Google Search: ")))
        (browse-url (concat "http://" my-google-url "/search?q="
                            (url-hexify-string query)))))

    (defun my-google-search-word-at-point ()
      "Google the word at point."
      (interactive)
      (browse-url (concat "http://" my-google-url "/search?q="
                          (word-at-point))))

    (defun my-google-search-file (file)
      "Use Google to search for a file named FILE."
      (interactive "sSearch for file: ")
      (w3m-browse-url
       (concat "http://" my-google-url "/search?q="
               (w3m-url-encode-string
                (concat "+intitle:\"index+of\" "
                        "-inurl:htm -inurl:html -inurl:php "
                        file)))))

    (defvar my-google-prefix-map (make-sparse-keymap)
      "Keymap for my Google commands.")

;;;     (global-set-key [(meta s)] 'my-google-search-region)

    (global-set-key (kbd "C-c g") my-google-prefix-map)
    (define-key my-google-prefix-map "g" 'my-google-search)
    (define-key my-google-prefix-map (kbd "RET") 'my-google-search)
    (define-key my-google-prefix-map "w" 'my-google-search-word-at-point)
    (define-key my-google-prefix-map "r" 'my-google-search-region)
    (define-key my-google-prefix-map "u" 'my-url-at-point)


    (defun lookup-word-definition-in-w3m ()
      "Look up the word's definition in a emacs-w3m.\n
If a region is active (a phrase), lookup that phrase."
      (interactive)
      (let (myword
            myurl)
        (setq myword
              (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (thing-at-point 'symbol)))
        (setq myword (replace-regexp-in-string " " "%20" myword))
        (setq myurl (concat "http://www.answers.com/main/ntquery?s=" myword))
        (w3m-browse-url myurl)))
    (define-key my-google-prefix-map "a" 'lookup-word-definition-in-w3m)

    (defun lookup-wikipedia ()
      "Look up the word's in Wikipedia.\n
This command generates a url for Wikipedia.com and switches
you to browser.
If a region is active (a phrase), lookup that phrase."
      (interactive)
      (let (myword
            myurl)
        (setq myword
              (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (thing-at-point 'symbol)))
        (setq myword (replace-regexp-in-string " " "_" myword))
        (setq myurl (concat "http://en.wikipedia.org/wiki/" myword))
        (w3m-browse-url myurl))))


;;*** Babel

(GNUEmacs
    (let ((my/path-to-url (concat local-site-lisp-directory "url.el"))
          (my/path-to-babel (concat local-site-lisp-directory "babel.el")))
          ;; http://github.com/juergenhoetzel/babel/blob/master/babel.el
      (if (and (file-readable-p my/path-to-url)
               (file-readable-p my/path-to-babel))
          (progn
            ;; Uniform Resource Locator retrieval tool
            (require 'url my/path-to-url)

            ;; interface to web translation services such as Babelfish
            (require 'babel my/path-to-babel)))))


(when (try-require 'org-toodledo)
  ;; FIXME depends on url (problem = conflict with other url packages)
  (setq org-toodledo-userid "td4bf295821172f")
  (setq org-toodledo-password "default"))


;;*** (info "(emacs-goodies-el)htmlize")

;; HTML-ize font-lock buffers
(when (try-require 'htmlize)
    ;; For Emacs 23 users: in order to avoid "Invalid face" errors, you need
    ;; to use the version made available by Carsten in `org-mode/contrib/lisp'
    ;; directory

    ;; output type of generated HTML
    (setq htmlize-output-type 'css)

    ;; override output type `inline-css' used for htmlizing a region
    (defun htmlize-region-for-paste (beg end)
      "Htmlize the region and return just the HTML as a string.
This forces the `css' style and only returns the HTML body, but
without the BODY tag. This should make it useful for inserting
the text to another HTML buffer."
      (let* ((htmlize-output-type 'css)  ; was `inline-css'
             (htmlbuf (htmlize-region beg end)))
        (unwind-protect
            (with-current-buffer htmlbuf
              (buffer-substring (plist-get htmlize-buffer-places 'content-start)
                                (plist-get htmlize-buffer-places 'content-end)))
          (kill-buffer htmlbuf))))

    ;; charset declared by the resulting HTML documents
    (setq htmlize-html-charset "utf-8")

    ;; non-ASCII characters (codes in the 128-255 range) are copied to HTML
    ;; without modification -- if your HTML is in Unicode
    (setq htmlize-convert-nonascii-to-entities nil)

    ;; key binding
    (global-set-key (kbd "M-P") 'htmlize-buffer))

;; quick print preview (to Web browser) with `htmlize-view-buffer'
(GNUEmacs
    ;; view current buffer as html in web browser
    (when (try-require 'htmlize-view)

        ;; add "Quick Print" entry to file menu
        (htmlize-view-add-to-files-menu)))

        ;; Now, you can print from the browser in (complete) Unicode,
        ;; using your system's capabilities

(message "54 Hyperlinking and Navigation Features... Done"))
