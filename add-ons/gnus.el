(defaddon gnus
  nil
  (require-package 'bbdb)

  (setq ldap-directory-lookup nil)

  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)

  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

  (setq bbdb-use-pop-up nil
        bbdb-notice-auto-save-file t
        bbdb-pop-up-target-lines 10
        bbdb-send-mail-style 'message)

  (when (file-exists-p "~/.gnus-config.el")
    (load-file "~/.gnus-config.el"))

  (when (file-exists-p "~/.ldap-config.el")
    (load-file "~/.ldap-config.el"))

  (require 'eudc)
  (require 'eudcb-bbdb)
  (require 'eudcb-mab)

  (eudc-set-server "localhost" 'bbdb t)
  (setq eudc-inline-expansion-servers 'hotlist)
  (add-to-list 'eudc-server-hotlist '("localhost" . bbdb) t)
  (setq eudc-default-return-attributes nil
        eudc-strict-return-matches nil)
  (eudc-protocol-set 'eudc-inline-query-format
                     '((firstname)
                       (lastname)
                       (firstname lastname)
                       (net))
                     'bbdb)
  (eudc-protocol-set 'eudc-inline-expansion-format
                     '("%s %s <%s>" firstname lastname net)
                     'bbdb)
  (MacOSX
   (add-to-list 'eudc-server-hotlist '("localhost" . mab) t)
   (eudc-protocol-set 'eudc-inline-query-format
                      '((name)
                        (email))
                      'mab)
   (eudc-protocol-set 'eudc-inline-expansion-format
                      '("%s <%s>" name email)
                      'mab))

  (when (and (boundp 'ldap-directory-lookup) ldap-directory-lookup)
    (add-to-list 'eudc-server-hotlist '("exchange-server" . ldap) t)
    (eudc-protocol-set 'eudc-inline-query-format
                       '(;(cn)
                         ;(cn cn)
                         ;(cn cn cn)
                         ;(sn)
                         ;(givenname)
                         ;(surname)
                         ;(givenname surname)
                         ;(fullname)
                         ;(uid)
                         ;(name)
                         ;(surname)
                         (mail))
                         ;(mailnickname))
                       'ldap)
    (eudc-protocol-set 'eudc-inline-expansion-format
                       '("%s %s <%s>" givenname surname mail)
                       'ldap))

  (defun slb-eudc-expand-inline()
    (interactive)
    (if (eq eudc-protocol 'ldap)
        (progn (move-end-of-line 1)
               (insert "*")
               (unless (condition-case nil
                           (eudc-expand-inline)
                         (error nil))
                 (backward-delete-char-untabify 1)))
      (eudc-expand-inline)))

  (eval-after-load "message"
    '(define-key message-mode-map (kbd "TAB") 'slb-eudc-expand-inline))
  (eval-after-load "sendmail"
    '(define-key mail-mode-map (kbd "TAB") 'slb-eudc-expand-inline))
  (eval-after-load "post"
    '(define-key post-mode-map (kbd "TAB") 'slb-eudc-expand-inline)))
