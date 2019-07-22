(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'eval-last-sexp)
             (local-set-key (kbd "C-c e")   'eval-region)
             (setq indent-tabs-mode nil)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'eval-last-sexp)
             (local-set-key (kbd "C-c e")   'eval-region)
             (setq indent-tabs-mode nil)))
