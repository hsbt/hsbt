(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; You can edit user-customizable variables by typing the following command.
;;     M-x customize-group [RET] jsx-mode
(custom-set-variables
 '(jsx-indent-level 2)
 ;; '(jsx-cmd-options '("--add-search-path" "/path/to/search-path"))
 '(jsx-use-flymake t)
 '(jsx-syntax-check-mode "compile"))

(defun jsx-mode-init ()
  (define-key jsx-mode-map (kbd "C-c d") 'jsx-display-popup-err-for-current-line)
  (when (require 'auto-complete nil t)
    (auto-complete-mode t)))

(add-hook 'jsx-mode-hook 'jsx-mode-init)
