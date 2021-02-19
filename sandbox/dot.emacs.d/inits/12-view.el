;; view-mode
(setq view-read-only t)
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "kj" 'view-mode)

;; 書き込み権限が無いときは最初から view-mode
(require 'viewer)
(viewer-stay-in-setup)

;; 書き込み不可能な時は赤くする
(setq viewer-modeline-color-unwritable "tomato")
(viewer-change-modeline-color-setup)

;; (require 'auto-highlight-symbol-config)
;; (require 'highlight-symbol)
;; (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
;; (global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
;; (global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)
