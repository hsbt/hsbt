alias l lsd
alias la "lsd -a"
alias ll "lsd -la"
alias lt "lsd --tree"
alias cat bat
alias e code-insiders
alias bx "bundle exec"
alias mk "make -C .x86_64-darwin -j"
alias all-ruby "docker run --rm -t rubylang/all-ruby /all-ruby/all-ruby"

source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc

function export
  set arr (echo $argv|tr = \n)
  set -gx $arr[1] $arr[2]
end

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

function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  printf '%s ' (__fish_git_prompt)
  echo -n '> '
end

eval (direnv hook fish)
