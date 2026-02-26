setopt auto_cd

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/Library/Caches" # macOS default

export HOMEBREW_FORBIDDEN_FORMULAE="node npm pnpm yarn python"

export IRBRC="$XDG_CONFIG_HOME/irb/irbrc"
export GEMRC="$XDG_CONFIG_HOME/gem/gemrc"
export GEM_HOME="$XDG_DATA_HOME/gem"
export GEM_SPEC_CACHE="$XDG_CACHE_HOME/gem"
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"
export MIX_HOME="$XDG_DATA_HOME/mix"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME/aws/credentials"
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME/aws/config"
export PYLINTHOME="$XDG_CACHE_HOME/pylint"
export HELM_HOME="$XDG_DATA_HOME/helm"
export KREW_ROOT="$XDG_DATA_HOME/krew"
export VIMINIT=":source $XDG_CONFIG_HOME/vim/vimrc"
export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
export VSCODE_PORTABLE="$XDG_DATA_HOME/vscode"
export _ZO_DATA_DIR="$XDG_DATA_HOME/zoxide"
export TEALDEER_CONFIG_DIR="$XDG_CONFIG_HOME/tealdeer"
export CSEARCHINDEX="$XDG_CACHE_HOME/csearchindex"
export TERMINFO="$XDG_DATA_HOME/terminfo"
export TERMINFO_DIRS="$XDG_DATA_HOME/terminfo:/usr/share/terminfo"
export AZURE_CONFIG_DIR="$XDG_CONFIG_HOME/azure"
export CONDARC="$XDG_CONFIG_HOME/conda/condarc"
export PUB_CACHE="$XDG_CACHE_HOME/pub-cache"
export KUBECONFIG="$XDG_CONFIG_HOME/kube/config"
export GOMODCACHE="$XDG_CACHE_HOME/go-mod"
export GOBIN="$XDG_DATA_HOME/go/bin"
export ANALYZER_STATE_LOCATION_OVERRIDE="$XDG_CACHE_HOME/dart_server"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export VCPKG_DEFAULT_BINARY_CACHE="$XDG_CACHE_HOME/vcpkg/archives"
export VCPKG_DOWNLOADS="$XDG_CACHE_HOME/vcpkg/downloads"

alias s3cmd='s3cmd --config $XDG_CONFIG_HOME/s3cmd'
alias wget='wget --hsts-file=$XDG_CACHE_HOME/wget-hsts'
alias gpg='gpg --homedir $XDG_DATA_HOME/gnupg'
alias yarn='yarn --use-yarnrc $XDG_CONFIG_HOME/yarn/config'
alias mvn='mvn -gs $XDG_CONFIG_HOME/maven/settings.xml'

export LANG=en_US.UTF-8
export TERM=xterm-256color
export GPG_TTY=$(tty)
export GIT_MERGE_AUTOEDIT=no
export MISE_ENV_FILE=.envrc
export LESSHISTFILE=-
export CC='sccache clang'
export RUSTC_WRAPPER="$CARGO_HOME/bin/sccache"
export RUBY_CODESIGN=hsbt
export RUBYOPT=-w
export RUBY_MN_THREADS=1
export RUBY_YJIT_ENABLE=1
export RUBY_CONFIGURE_OPTS=--disable-install-doc
export MAKEFLAGS="-j$(sysctl -n hw.logicalcpu)"
export PKG_CONFIG_PATH="/opt/homebrew/opt/imagemagick/lib/pkgconfig:/usr/lib/pkgconfig"
export GIT_GOGET_ROOT="$HOME/Documents"
export SKIM_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export HELIX_RUNTIME="$GIT_GOGET_ROOT/github.com/helix-editor/helix/runtime"
export EDITOR=hx

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

alias l='lsd'
alias ll='lsd -la'
alias make='make --no-print-directory --quiet'

alias e='zed .'
g() {
  hub "$@"
}

__cd_repository() {
  local repo_path=$(fd . "$GIT_GOGET_ROOT" -t d --max-depth 3 | sk)
  cd "$repo_path" || return
  zle reset-prompt
}

zle -N __cd_repository
bindkey '^g' __cd_repository

source "$HOME/.config/op/plugins.sh"
source "$(mise where gcloud)/path.zsh.inc"
eval "$(zoxide init zsh --cmd j)"
eval "$(atuin init zsh)"
eval "$(starship init zsh)"
eval "$(mise activate zsh)"

alias python3="$(uv python find)"
alias python=python3

source $GIT_GOGET_ROOT/github.com/zsh-users/zsh-autosuggestions/zsh-autosuggestions.zsh
zstyle ':completion:*:*:*:*:*' menu select

autoload -U compinit && compinit
zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
source <(carapace _carapace)
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'
