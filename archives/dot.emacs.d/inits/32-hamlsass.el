(require 'haml-mode)
(add-hook 'haml-mode-hook
   '(lambda ()
       (setq tab-width 2)
       (setq indent-tabs-mode nil)))

(require 'sass-mode)
(add-hook 'sass-mode-hook
   '(lambda ()
       (setq tab-width 2)
       (setq indent-tabs-mode nil)))
