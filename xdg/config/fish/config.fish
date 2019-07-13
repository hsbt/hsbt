alias ls lsd
alias cat bat

alias l ls
alias ll "ls -la"
alias e code-insiders

alias bx "bundle exec"
alias mk "make -C .x86_64-darwin -j"
alias all-ruby "docker run --rm -t rubylang/all-ruby /all-ruby/all-ruby"
alias s3cmd "s3cmd --config $HOME/.config/s3cmd"
alias mvn "mvn -gs $HOME/.config/maven/settings.xml"

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

source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc

eval (direnv hook fish)
