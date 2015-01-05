(defun fg/string-starts-with (string prefix)
  "Return t if STRING starts with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
		     string)
       t))

(defun fg/string-ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
		     string)
       t))

(provide 'fg-string)
