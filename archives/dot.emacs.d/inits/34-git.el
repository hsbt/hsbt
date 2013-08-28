(delete 'Git vc-handled-backends)

(require 'git-status)
(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "#202020")
     (set-face-foreground 'magit-diff-add "#40ff40")
     (set-face-foreground 'magit-diff-del "#ff4040")
     (set-face-foreground 'magit-diff-file-header "#4040ff")))

(require 'gist)

(require 'git-gutter)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
