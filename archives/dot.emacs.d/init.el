;; load-path
(let ((default-directory "~/.emacs.d/plugins"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")
