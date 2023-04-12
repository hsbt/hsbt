function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  if set -q RBENV_VERSION
    echo -n ' '
    set_color red
    echo -n $RBENV_VERSION
  end
  set_color normal
  echo -e (__fish_git_prompt)
  echo -n '$ '
end

function export
  set arr (echo $argv|tr = \n)
  set -gx $arr[1] $arr[2]
end

function g --wraps git
  hub $argv;
end

function history-merge --on-event fish_preexec
  history --save
  history --merge
end

source (brew --prefix)/share/google-cloud-sdk/path.fish.inc
source $HOME/.config/op/plugins.sh
eval (direnv hook fish)
zoxide init fish --cmd j | source
