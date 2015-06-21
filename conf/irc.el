(when (fboundp 'advice-add)
  (advice-add 'start-irc :before #'fg/load-secrets))
