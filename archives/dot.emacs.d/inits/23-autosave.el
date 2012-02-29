(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)
(progn
  (setq auto-save-list-file-name nil)
  (setq auto-save-list-file-prefix nil)
  (setq make-backup-files nil))

;; クリップボード履歴を kill-ring 送りにする
(require 'clipboard-to-kill-ring)
(clipboard-to-kill-ring t)

;; 履歴の拡張
(require 'recentf-ext)
(setq recentf-max-saved-items 3000)

;; 適当に古いバッファを消してくれる
(require 'tempbuf)
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
