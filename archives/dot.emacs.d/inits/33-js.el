(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'coffee-cleanup-whitespace) nil))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

(require 'js2-mode)
