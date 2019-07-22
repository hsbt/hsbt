;; exec-path
(dolist (dir (list
      "~/.rbenv/shims"
      "/usr/local/bin"
      "/usr/bin"
      "/bin"))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; ELPA
;;(setq debug-on-error t)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install-elisp
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;(setq auto-install-wget-command "wget")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; てきとーにバイトコンパイルしてくれる
;; (require 'auto-async-byte-compile)
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
