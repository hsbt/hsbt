;; linum を有効にする奴

;; (setq linum-format "%4d")(dolist (hook (list
;;               'emacs-lisp-mode-hook
;;               'lisp-interaction-mode-hook
;;               'lisp-mode-hook
;; 	      'sh-mode-hook
;; 	      'ruby-mode-hook
;; 	      'haml-mode-hook
;; 	      'sass-mode-hook
;; 	      'coffee-mode-hook
;; 	      'yaml-mode-hook))
;; (add-hook hook (lambda () (linum-mode t))))

(global-linum-mode)
(setq linum-format "%4d")

(global-set-key (kbd "M-n") 'linum-mode)
