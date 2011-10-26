;; user settings
(setq user-full-name "SHIBATA Hiroshi")
(setq user-mail-address "shibata.hiroshi@gmail.com")

;; gz とかを開けるようにする
(auto-compression-mode t)

;; 補完でCSを区別しない
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完で除外するパターン
(add-to-list 'completion-ignored-extensions ".svn/")

;; 選択はキーボードだけにする
(setq pc-select-selection-keys-only t)

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; ^Mが邪魔
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; スクロールは1行
(setq scroll-step 1)

;; 対応する括弧を強調
(show-paren-mode t)

;; 現在行を強調
(global-hl-line-mode 1)
(set-face-background 'hl-line "dark slate gray")

;; ベルはうるさい
(setq ring-bell-function 'ignore)

;; とにかく utf-8 にする
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority
    (cons 'utf-8
        (get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; 自動で無駄なスペースを消す
(defun toggle-delete-trailing-whitespace-setting ()
  (interactive)
  (cond ((find 'delete-trailing-whitespace before-save-hook)
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (
         (add-hook 'before-save-hook 'delete-trailing-whitespace))))

;; shebang があるなら実行権限を勝手に付ける
(add-hook 'after-save-hook 'my-chmod-script)
(defun my-chmod-script() (interactive) (save-restriction (widen)
 (let ((name (buffer-file-name)))
  (if (and (not (string-match ":" name))
           (not (string-match "/\\.[^/]+$" name))
           (equal "#!" (buffer-substring 1 (min 3 (point-max)))))
     (progn (set-file-modes name (logior (file-modes name) 73))
            (message "Wrote %s (chmod +x)" name))))))

;; クライアント用のサーバー
(server-start)

;; クライアントを終了するとき終了するかどうかを聞かない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; 終了時にプロセスが残っていてもとにかく殺す
(defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))
