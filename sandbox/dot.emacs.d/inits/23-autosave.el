;; antipop special
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5)
(auto-save-buffers-enhanced t)

;; バックアップファイルいらない
(progn
  (setq auto-save-list-file-name nil)
  (setq auto-save-list-file-prefix nil)
  (setq make-backup-files nil))

;; 適当に古いバッファを消してくれる
(require 'tempbuf)
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
