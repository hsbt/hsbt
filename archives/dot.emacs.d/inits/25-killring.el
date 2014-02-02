;; クリップボード履歴を kill-ring 送りにする
(require 'clipboard-to-kill-ring)
(clipboard-to-kill-ring t)

;; 履歴の拡張
(require 'recentf-ext)
(setq recentf-max-saved-items 3000)
