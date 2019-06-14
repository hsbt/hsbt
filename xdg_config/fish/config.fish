set -x EDITOR vim
set -x RUBY_CODESIGN hsbt
set -x LANG en_US.UTF-8

set -x DOCKER_BUILDKIT 1
set -x GIT_MERGE_AUTOEDIT no
set -U Z_CMD "j"
set -U Z_DATA $HOME/Dropbox/Configuration/history.dotfiles/dot.$USER.z

source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc

alias l lsd
alias la "lsd -a"
alias ll "lsd -la"
alias lt "lsd --tree"
alias cat bat
alias e code-insiders
alias bx "bundle exec"
alias mk "make -C .x86_64-darwin -j"
alias all-ruby "docker run --rm -t rubylang/all-ruby /all-ruby/all-ruby"

function g --wraps git
  hub $argv;
end

function gemsearch
  csearch $argv | sed 's/^\/Users\/hsbt\/Documents\/github\.com\/akr\/gem-codesearch\/latest-gem\///g';
end

function history-merge --on-event fish_preexec
  history --save
  history --merge
end

eval (direnv hook fish)
