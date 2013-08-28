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

;; http://willnet.in/13
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; ido-mode
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)
(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode t) 

(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
  (lambda () (rinari-launch)))

(require 'feature-mode)
(setq feature-default-language "ja")
(setq feature-default-i18n-file "~/.emacs.d/plugins/feature-mode/i18n.yml")

(require 'ruby-end)