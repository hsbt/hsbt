(defun memo ()
  (interactive)
    (add-change-log-entry 
     nil
     (expand-file-name "~/Dropbox/Documents/Changelog")))
(define-key ctl-x-map "m" 'memo)

(add-hook 'change-log-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq left-margin 2)))
