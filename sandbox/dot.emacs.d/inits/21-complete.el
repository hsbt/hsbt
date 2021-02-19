;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-sources '(ac-source-abbrev
                   ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ac-source-dictionary))

;; auto-complete 標準外の補完モード
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'javascript-mode)
