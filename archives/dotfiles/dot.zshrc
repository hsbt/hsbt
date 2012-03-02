umask 022
bindkey -e

# Set shell options
setopt always_last_prompt
setopt auto_list
setopt auto_menu
setopt auto_cd
setopt auto_param_keys
setopt auto_name_dirs
setopt auto_remove_slash
setopt cdable_vars
setopt correct
setopt extended_glob
setopt interactive_comments
setopt list_packed
setopt list_types
setopt magic_equal_subst
setopt no_beep
setopt no_flow_control
setopt prompt_subst
setopt print_eight_bit
setopt numeric_glob_sort
setopt rm_star_silent
setopt sun_keyboard_hack
setopt sh_word_split
setopt zle
setopt long_list_jobs

# zsh function path
fpath=($HOME/.zsh.d/zsh-completions $HOME/.zsh.d/zsh-functions $HOME/.zsh.d/zsh-git-escape-magic $HOME/.zsh.d/zsh-manydots-magic $fpath)

# Zsh module
zmodload zsh/files

# ?
autoload zmv
alias zmv='noglob zmv'

# knu special
autoload -Uz git-escape-magic
git-escape-magic

autoload -Uz manydots-magic
manydots-magic

# ls colors
autoload colors; colors
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# autocomp
autoload -U compinit
compinit -d /tmp/$USER.zcompdump
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:sudo:*' command-path 
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"

# Prompt setting
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b] (%a)'
precmd() {
  psvar=()
  LANG=en_US.UTF-8 vcs_info
  [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
  psvar[2]=`rbenv version-name`
  # psvar[2]=`ruby -e 'puts "#{RUBY_VERSION}p#{RUBY_PATCHLEVEL > -1 ? RUBY_PATCHLEVEL : 0}"'`
  [[ -e $PWD/.git/refs/stash ]] && psvar[3]="$(git stash list 2>/dev/null | wc -l) stashed"
}
PROMPT=$'[%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[red]%}%M%{$reset_color%}] %{$fg[cyan]%}%d %1(V|%F{green}%1v%3(V| - %3v|)%f |)%2(V|%F{red}(%2v%)%f|)\n%{$fg[yellow]%}%#%{$reset_color%} '
RPROMPT=''

# History
setopt extended_history
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_save_nodups
setopt hist_reduce_blanks
setopt inc_append_history

HISTFILE=$DROPBOX/config/dotfiles/history/dot.zsh-history
HISTSIZE=10000000
SAVEHIST=$HISTSIZE

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

source $HOME/.zsh.d/zaw/zaw.zsh
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10 # use 10 lines for filter-select
zstyle ':filter-select' max-lines -10 # use $LINES - 10 for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search yes # see below

# homebrew
export PATH="$HOME/.homebrew/bin:$PATH"
export PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
export LDFLAGS="-L$HOME/.homebrew/lib"
export CPPFLAGS="-I$HOME/.homebrew/include"
export CFLAGS=$CPPFLAGS

# rsruby
export R_HOME=/Library/Frameworks/R.framework/Resources

# rbenv
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# perlbrew
export PERLBREW_ROOT=$HOME/.perlbrew
source $HOME/.perlbrew/etc/bashrc

# pythonbrew
source $HOME/.pythonbrew/etc/bashrc

# Java
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home

# MySQL
export MYSQL_HISTFILE=$DROPBOX/config/dotfiles/history/dot.mysql_history
export MYSQL_PS1='\u@\h[\d]> '

# aliases & function
source $HOME/.zsh.d/function
source $HOME/.zsh.d/aliases
source `brew --prefix`/etc/autojump.zsh
