;; https://github.com/emacs-helm/helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "C-¥") 'helm-mini)

;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; https://github.com/syohex/emacs-helm-ag
(require 'helm-ag)
(define-key global-map (kbd "C-^") 'helm-ag)

;; https://github.com/emacs-helm/helm-ls-git
(require 'helm-ls-git)
(define-key global-map (kbd "C-:") 'helm-ls-git-ls)
