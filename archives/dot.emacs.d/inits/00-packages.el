;; exec-path
(dolist (dir (list
      "/usr/local/bin"
      "/usr/bin"
      "/bin"
      "/Users/hsbt/.homebrew/bin"))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; install-elisp
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;(setq auto-install-wget-command "wget")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; ELPA
(when
  (load (expand-file-name "~/.emacs.d/elpa/package.el")) 
  (package-initialize))

;; てきとーにバイトコンパイルしてくれる
(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
