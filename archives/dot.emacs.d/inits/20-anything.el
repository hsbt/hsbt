(require 'anything-config)
(require 'anything-migemo)
(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-recentf
        anything-c-source-files-in-current-dir+
        anything-c-source-file-name-history))

(require 'color-moccur)
(setq moccur-split-word t)
(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2
      anything-c-moccur-higligt-info-line-flag t
      anything-c-moccur-enable-auto-look-flag t
      anything-c-moccur-enable-initial-pattern t)

(require 'anything-project)
(global-set-key (kbd "C-c C-f") 'anything-project)
(ap:add-project
 :name 'ruby
 :look-for '("Gemfile" "Rakefile") ; or
 :include-regexp '("\\.rb$" "\\.haml$" "\\.coffee$" "\\.js$"))

;; emacs commands
(define-key global-map (kbd "M-x")
  (lambda ()
    "Execute emacs commands in anything"
    (interactive)
    (anything '(anything-c-source-emacs-commands))))

(global-set-key (kbd "C-¥") 'anything)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "C-s") 'anything-c-moccur-occur-by-moccur)

;; (require 'sha1-el)
;; (require 'anything-hatena-bookmark)
;; (define-key global-map (kbd "C-c C-a b") 'anything-hatena-bookmark)

;; (require 'anything-dabbrev-expand)
;; (global-set-key (kbd "M-p") 'anything-dabbrev-expand)
;; (define-key anything-dabbrev-map (kbd "M-p") 'anything-dabbrev-find-all-buffers)
