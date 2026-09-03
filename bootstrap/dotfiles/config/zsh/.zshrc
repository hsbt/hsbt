bindkey -e

setopt auto_cd
setopt extended_glob
setopt no_beep
setopt no_flow_control
setopt interactive_comments
setopt print_eight_bit
setopt long_list_jobs
setopt numeric_glob_sort
setopt magic_equal_subst

WORDCHARS='_-'

# 組み込みの backward-kill-word は記号の並びとその手前の単語をまとめて消すので、
# "foo && " が丸ごと消える。記号の並びと単語を別々に消す ^W に差し替える。
__backward_kill_word() {
  emulate -L zsh
  setopt extended_glob
  local left=$LBUFFER rest new removed
  local word="[[:alnum:]${WORDCHARS}]" punct="[^[:alnum:][:space:]${WORDCHARS}]"
  rest=${left%%[[:space:]]##}
  if [[ -z $rest ]]; then
    new=''
  elif [[ ${rest[-1]} == ${~word} ]]; then
    new=${rest%%${~word}##}
  else
    new=${rest%%${~punct}##}
  fi
  removed=${left[${#new}+1,-1]}
  if [[ $LASTWIDGET == __backward_kill_word ]]; then
    CUTBUFFER=$removed$CUTBUFFER
  else
    killring=("$CUTBUFFER" "${(@)killring[1,-2]}")
    CUTBUFFER=$removed
  fi
  LBUFFER=$new
  zle -f kill
}
zle -N __backward_kill_word
bindkey '^W' __backward_kill_word

export TERM=xterm-256color
export GPG_TTY=$(tty)
export REPORTTIME=3
export SKIM_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

# Use portable Ruby instead of System Ruby by macOS
export PATH="/opt/homebrew/Library/Homebrew/vendor/portable-ruby/current/bin:$PATH"

export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/opt/m4/bin:$PATH"
export PATH="/opt/homebrew/opt/bison/bin:$PATH"
# export PATH="/opt/homebrew/opt/binutils/bin:$PATH"
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/findutils/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
# export PATH="/opt/homebrew/opt/libtool/libexec/gnubin:$PATH"

# Prepend cargo and go paths
export PATH="$XDG_DATA_HOME/go/bin:$PATH"
export PATH="$XDG_DATA_HOME/cargo/bin:$PATH"

typeset -U path

autoload zmv

alias wget='wget --hsts-file=$XDG_CACHE_HOME/wget-hsts'
alias gpg='gpg --homedir $XDG_DATA_HOME/gnupg'
alias yarn='yarn --use-yarnrc $XDG_CONFIG_HOME/yarn/config'
alias mvn='mvn -gs $XDG_CONFIG_HOME/maven/settings.xml'
alias zmv='noglob zmv'
alias l='lsd'
alias ll='lsd -la'
alias make='make --no-print-directory --quiet'
alias e='zed .'

c() {
  open "claude://code/new?folder=${${1:-$PWD}:A}"
}

g() {
  # hub だと子プロセスで cd や環境変更が失われるサブコマンドは、
  # cd フック付きの git 関数経由にする。対象が増えたら via_git に足すだけ。
  local -a via_git=(wt)
  if (( ${via_git[(Ie)$1]} )); then
    git "$@"
  else
    hub "$@"
  fi
}

__cd_repository() {
  local repo_path=$({
    zoxide query --list 2>/dev/null | grep "^$GIT_GOGET_ROOT/"
    fd . "$GIT_GOGET_ROOT" -t d --max-depth 3 | sed 's/\/$//'
  } | grep -E "^$GIT_GOGET_ROOT/[^/]+/[^/]+/[^/]+$" | awk '!seen[$0]++' | sk)
  [ -z "$repo_path" ] && { zle reset-prompt; return; }

  local target=$(tmux list-panes -s -F "#{pane_current_path} #{window_index}.#{pane_index}" 2>/dev/null \
    | awk -v path="$repo_path" '$1 == path {print $2; exit}')

  if [ -n "$target" ]; then
    tmux select-window -t "${target%%.*}"
    tmux select-pane -t "$target"
  else
    tmux new-window -c "$repo_path"
  fi
  zle reset-prompt
}

zle -N __cd_repository
bindkey '^g' __cd_repository

__cd_home() {
  local target=$(tmux list-panes -s -F "#{pane_current_path} #{window_index}.#{pane_index}" 2>/dev/null \
    | awk -v path="$HOME" '$1 == path {print $2; exit}')

  if [ -n "$target" ]; then
    tmux select-window -t "${target%%.*}"
    tmux select-pane -t "$target"
  else
    tmux new-window -c "$HOME"
  fi
  zle reset-prompt
}

zle -N __cd_home
bindkey '^h' __cd_home

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/compcache"
zstyle ':completion:*' verbose yes
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}no matches%f'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'

autoload -U compinit
if [[ -n $XDG_CACHE_HOME/zsh/zcompdump(#qN.mh+24) ]]; then
  compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"
else
  compinit -C -d "$XDG_CACHE_HOME/zsh/zcompdump"
fi

source <(carapace _carapace)

_git_via_carapace() {
  words[1]=git
  _carapace_completer
}
compdef _git_via_carapace hub
compdef _git_via_carapace g

source "$HOME/.config/op/plugins.sh"
source "$(mise where gcloud)/path.zsh.inc"
eval "$(zoxide init zsh --cmd j)"
eval "$(atuin init zsh)"
eval "$(starship init zsh)"
# Remove inherited mise paths before activation to prevent duplicates
path=(${path:#*/.local/share/mise/installs/*})
unset __MISE_ORIG_PATH __MISE_DIFF __MISE_SESSION
eval "$(mise activate zsh)"
eval "$(git wt --init zsh)"

# g wt <TAB> でも git-wt の worktree 補完を出す。それ以外は carapace に流す。
_g-wt-wrapper() {
  if (( CURRENT == 2 )); then
    _git_via_carapace
  elif [[ "${words[2]}" == "wt" ]]; then
    shift words
    (( CURRENT-- ))
    _git-wt
  else
    _git_via_carapace
  fi
}
compdef _g-wt-wrapper g

source $GIT_GOGET_ROOT/github.com/zsh-users/zsh-autosuggestions/zsh-autosuggestions.zsh

alias python3="$(uv python find)"
alias python=python3
