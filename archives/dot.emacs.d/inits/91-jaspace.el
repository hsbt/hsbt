(require 'jaspace)
(setq jaspace-alternate-jaspace-string "□")
(setq jaspace-alternate-eol-string "↓\n")
(setq jaspace-highlight-tabs t)
(dolist (hook (list
              'text-mode-hook
              'yaml-mode-hook
              'ruby-mode-hook
              'haml-mode-hook
              'sass-mode-hook
              'coffee-mode-hook
              'feature-mode-hook))
(add-hook hook (lambda () (jaspace-mode t))))
