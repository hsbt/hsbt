;; ウィンドウの設定
(if window-system 
    (progn
      (setq initial-frame-alist
            '((width . 80) (height . 50)))
      (set-background-color "Black")
      (set-foreground-color "White")
      (set-cursor-color "Gray")
      (set-frame-parameter nil 'alpha 85)))

;; Menloを使う
(set-face-attribute 'default nil
                 :family "Menlo"
                 :height 120)
(set-fontset-font
 (frame-parameter nil 'font)
 'japanese-jisx0208
 '("Hiragino Maru Gothic Pro" . "iso10646-1"))
(set-fontset-font
 (frame-parameter nil 'font)
 'japanese-jisx0212
 '("Hiragino Maru Gothic Pro" . "iso10646-1"))

;; Ricty を使う
;; (set-face-attribute 'default nil
;;                     :family "Ricty"
;;                     :height 160)
;; (set-fontset-font
;;  nil
;;  'japanese-jisx0208
;;  (font-spec :family "Ricty"))
;; (set-fontset-font
;;  nil
;;  'japanese-jisx0212
;;  (font-spec :family "Ricty"))

;; フォントサイズ変更の割合
(setq face-font-rescale-alist
      '(("^-apple-hiragino.*" . 1.2)))

;; WindMove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
(setq windmove-wrap-around t)

;; スクロールバーはいらない
(set-scroll-bar-mode nil)

;; ツールバーはいらない
(tool-bar-mode 0)

;; ダイアログは出さない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; D&D で新しくフレームを作らない
(setq ns-pop-up-frames nil)

;; 起動時のメッセージを消す
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; タイトルフォーマット
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 時間表示の変更
(progn
  (setq display-time-24hr-format t)
  (setq display-time-format "%Y-%m-%d(%a) %H:%M")
  (setq display-time-day-and-date t)
  (setq display-time-interval 30)
  (display-time))

;; 縦分割でも折り返す
(setq truncate-partial-width-windows nil)

;; C-kで行全体を削除
(setq kill-whole-line t)

;; 矩形モード
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;;; 縦二分割 (IMAKADOスタイル)
(split-window-horizontally)

;; カーソル移動位置の繰り返しを拡張
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; 同じファイル名の表示を見やすく
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
