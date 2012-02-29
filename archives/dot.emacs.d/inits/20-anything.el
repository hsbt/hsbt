(require 'anything-config)
(require 'anything-migemo)
(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-recentf
        anything-c-source-files-in-current-dir+
        anything-c-source-file-name-history))

(global-set-key (kbd "C-¥") 'anything)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(setq anything-allow-skipping-current-buffer t)

(require 'color-moccur)
(setq moccur-split-word t)
(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2
      anything-c-moccur-higligt-info-line-flag t
      anything-c-moccur-enable-auto-look-flag t
      anything-c-moccur-enable-initial-pattern t)
;(global-set-key (kbd "C-s") 'anything-c-moccur-occur-by-moccur)

(require 'anything-project)
(global-set-key (kbd "C-c C-f") 'anything-project)
(ap:add-project
 :name 'ruby
 :look-for '("Gemfile" "Rakefile") ; or
 :include-regexp '("\\.rb$" "\\.yml$" "\\.haml$" "\\.coffee$" "\\.js$" "\\.scss"))

(require 'anything-rdefs)
(setq ar:command "~/.rbenv/versions/2.0.0-dev/bin/rdefs")
(add-hook 'ruby-mode-hook
  (lambda ()
    (define-key ruby-mode-map (kbd "C-@") 'anything-rdefs)))

(define-key global-map (kbd "M-x")
  (lambda ()
    "Execute emacs commands in anything"
    (interactive)
    (anything '(anything-c-source-emacs-commands))))

(setq anything-c-filelist-file-name "~/.emacs.d/all.filelist")
(global-set-key (kbd "C-;") 'anything-filelist+)
