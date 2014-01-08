;;** Emacs Display

(when section-emacs-display (message "XX Emacs Display...")

;;*** Faces

;; You can get text properties of any char by typing `C-u C-x ='

;; Under Windows, you can get the current font string by typing
;; `(insert (format "\n%S" (w32-select-font)))' followed by `C-x C-e'

;; You can find the current font by typing
;; `M-x ielm RET (frame-parameters) RET'
;; see the line `font'

;; To check if some font is available in Emacs do following:
;;    1.   Switch to the `*scratch*' buffer.
;;    2.   Type `(prin1-to-string (x-list-fonts "font-you-want-to-check or
;;         pattern"))'.
;;    3.   Place the cursor after the last closing paren and hit
;;         `C-j'. List of the names of available fonts matching given
;;         pattern will appear in the current buffer (`*scratch*').
;;    4.   For listing of all available fonts, use
;;         `(prin1-to-string (x-list-fonts "*"))' or
;;         `(dolist (i (x-list-fonts "*")) (princ i) (terpri))'
;;         for a better output.

;; Format: "-a-b-c-d-e-f-g-h-i-j-k-l-"
;; where
;;
;; a = foundry
;;
;; b = font family <<<
;;
;; c = weight
;;     Valid options: `bold', `demibold', `light', `medium', `normal'.
;;
;; d = slant
;;     Valid options: `i' for italic and `r' for roman.
;;
;; e = set width
;;     Ignored by NT-Emacs.
;;
;; f = pixels
;;     Nominal font height in pixels. (Eg. 13 pixels roughly corresponds to
;;     10 points (a point is 1/72 of an inch) on a 96dpi monitor, so the
;;     font spec above is selecting a 10 point bold Courier font)
;;
;; g = points in tenths of a point
;;     10 point is 100
;;
;; h = horiz resolution in dpi
;;     I think these numbers represent the "design resolution" of the font -
;;     on X, fonts are typically designed for 75dpi or 100dpi screens (under
;;     Windows,most monitors are assumed to be 96dpi I believe). NT-Emacs
;;     ignores these values.
;;
;; i = vertical resolution in dpi
;;     I think these numbers represent the "design resolution" of the font -
;;     on X, fonts are typically designed for 75dpi or 100dpi screens (under
;;     Windows,most monitors are assumed to be 96dpi I believe). NT-Emacs
;;     ignores these values.
;;
;; j = spacing
;;     Spacing as in mono-spaced or proportionally spaced.
;;     Values are `c' (constant) or `m' (monospace) to mean fixed-width or
;;     `p' for proportionally spaced.
;;
;; k = average width in tenths of a pixel
;;
;; l = character set
;;     NT-Emacs understands: ansi, oem, symbol to refer to the standard
;;     Windows character sets (the first two, at least, are locale
;;     dependant). "iso8859" and "iso8859-1" are accepted as synonyms for
;;     ansi.

;; Use `xfontsel' utility (or the command-line `xlsfonts') to try out
;; different fonts. After choosing a font, click the select button in
;; `xfontsel' window. This will copy font name you choose to copy & paste
;; buffer.
;; Edit your `~/.Xresources' file to have a line with "Emacs.font".
;; Then do a `xrdb -merge ~/.Xresources' or restart your X11 to validate the
;; modification. I let emacs do this for me:

(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (when (or (string= file ".Xdefaults")
              (string= file ".Xresources"))
      (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
      (message (format "Merged %s into X resource database" file)))))

(add-hook 'after-save-hook 'merge-x-resources)

;; Now Emacs should start with that font.

;; For reasons unknown to me,'emacs' takes a long file to change fonts in an X
;; environment.
;;
;; Rather than using (set-default-font ...) in .emacs, stick the font
;; definition in your .Xresources file (key 'Emacs*font') and then use 'xrdb
;; -load' to activate it. You will find that startup time is greatly improved!

;; avoid Emacs hanging for a while changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; the real color theme functions
(when (and window-system (try-require 'color-theme))

    ;; initialize the color theme package
    (if (fboundp 'color-theme-initialize)
        (color-theme-initialize))

    ;; color themes will be installed for all frames
    (setq color-theme-is-global t)

    ;; set my default color theme
    (when (try-require 'color-theme-cd)
      (color-theme-cd))
    (when (try-require 'color-theme-fni)
      (color-theme-fni)))  ; `color-theme-print' allows to keep what you see

;; allow any scalable font
(when running-ms-windows
    (setq scalable-fonts-allowed t))

(defun font-exists-p (font)
  "Test if FONT is available."
  (GNUEmacs23
   ;; FIXME list-fonts is void
   (if (null (list-fonts (font-spec :family font)))
              ;; 2008-02-26 function of the new font backend (Emacs 23),
              ;; instead of `x-list-fonts'
       nil
     t)))

;; set default font for all frames
(GNUEmacs
    (cond (running-ms-windows
           (if (font-exists-p "Consolas")
               (modify-all-frames-parameters
                '((font . "-outline-Consolas-normal-r-normal-normal-11-82-96-96-c-*-*-*")))
             ;; '((font . "-microsoft-consolas-medium-r-*-*-*-110-*-*-*-*-iso8859-1")))
             (modify-all-frames-parameters
              '((font . "-outline-Courier New-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")))))

          (running-gnu-linux
           (if (font-exists-p "Consolas")
               (modify-all-frames-parameters '((font . "Consolas-8")))
                                        ; short notation for Emacs23
             (modify-all-frames-parameters '((font . "DejaVu Sans Mono-9")))))))





;; Fonts that have a good UTF-8 coverage are:
;;
;;    + DejaVu Sans Mono
;;    + DejaVu Sans
;;    + FreeMono (FreeSans, FreeSerif)
;;    + Monospace
;;    - Arial Unicode (MS?)
;;    - Bitstream Vera Sans Mono
;;    - Lucida Sans (Typewriter?) Unicode
;;
;; None of them has all four variants, some have regular (medium) and bold
;; or light and regular, one regular and oblique.

;; To see if anti-aliasing is active, use `xmag' or any of the other
;; magnifier applications. The fonts should have gray edges.

(defvar font-cycle-index 0)
(defconst font-cycle-ring
  (if running-ms-windows
      '(
        "-*-Courier New-*-*-*-*-12-90-*-*-*-*-*-*"
        )
    ;; else
    '(
      "-Misc-Fixed-Medium-R-SemiCondensed-*-13-*-*-*-*-*-*-*"
      )
    ))

(defun font-cycle-next ()
  "Cycle between a list of fonts."
  (interactive)
  (let ((len (length font-cycle-ring))
        (next-index (+ font-cycle-index 1)))
    (if (= next-index len)
        (setq next-index 0))
    (setq font-cycle-index next-index)
    (message (concat "Setting default font to `"
                     (nth font-cycle-index font-cycle-ring) "'"))
    (set-default-font (nth font-cycle-index font-cycle-ring) t)
    (set-frame-font (nth font-cycle-index font-cycle-ring) t)))

(global-set-key (kbd "M-+") 'font-cycle-next)


(XEmacs
    (setq options-save-faces t))

;; convenience functions for face customization
(try-require 'face-list)

;; Cycle Font Sizes
;; Commands to zoom font size (like in Firefox)? In GNU Emacs 23, there are
;; `C-x C-+', `C-x C--' and `C-x C-0' (reset to defaults) for changing font
;; size.


;; Automatically switch to dark background after sunset
;; and to light background after sunrise.
;; See www.jurta.org/emacs/dotemacs.en.html

(load-theme 'tango-2 t)

(message "XX Emacs Display..."))
