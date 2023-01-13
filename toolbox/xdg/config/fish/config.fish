function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
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

source /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc
source $HOME/.config/op/plugins.sh
eval (direnv hook fish)
zoxide init fish --cmd j | source
