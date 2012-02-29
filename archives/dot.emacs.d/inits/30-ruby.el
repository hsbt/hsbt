(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

(require 'inf-ruby)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)))

;; ido-mode
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)
(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode t) 

(require 'feature-mode)
(setq feature-default-language "ja")
(setq feature-default-i18n-file "~/.emacs.d/plugins/feature-mode/i18n.yml")
