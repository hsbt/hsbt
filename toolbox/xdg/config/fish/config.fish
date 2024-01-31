set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_STATE_HOME $HOME/.local/state
set -x XDG_CACHE_HOME $HOME/Library/Caches # macOS default

set -x IRBRC $XDG_CONFIG_HOME/irb/irbrc
set -x GEMRC $XDG_CONFIG_HOME/gem/gemrc
set -x GEM_HOME $XDG_DATA_HOME/gem
set -x GEM_SPEC_CACHE $XDG_CACHE_HOME/gem
set -x BUNDLE_USER_CONFIG $XDG_CONFIG_HOME/bundle
set -x BUNDLE_USER_CACHE $XDG_CACHE_HOME/bundle
set -x BUNDLE_USER_PLUGIN $XDG_DATA_HOME/bundle
set -x RBENV_ROOT $XDG_DATA_HOME/rbenv
set -x RUBY_BUILD_CACHE_PATH $XDG_CACHE_HOME/rbenv
set -x MIX_HOME $XDG_DATA_HOME/mix
set -x GNUPGHOME $XDG_DATA_HOME/gnupg
set -x NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
set -x AWS_SHARED_CREDENTIALS_FILE $XDG_CONFIG_HOME/aws/credentials
set -x AWS_CONFIG_FILE $XDG_CONFIG_HOME/aws/config
set -x PYLINTHOME $XDG_CACHE_HOME/pylint
set -x HELM_HOME $XDG_DATA_HOME/helm
set -x KREW_ROOT $XDG_DATA_HOME/krew
set -x VIMINIT ":source $XDG_CONFIG_HOME"/vim/vimrc
set -x NODE_REPL_HISTORY $XDG_DATA_HOME/node_repl_history
set -x VSCODE_PORTABLE "$XDG_DATA_HOME"/vscode
set -x _ZO_DATA_DIR $XDG_DATA_HOME/zoxide
set -x TEALDEER_CONFIG_DIR $XDG_CONFIG_HOME/tealdeer
set -x CSEARCHINDEX $XDG_CACHE_HOME/csearchindex
set -x TERMINFO $XDG_DATA_HOME/terminfo
set -x TERMINFO_DIRS $XDG_DATA_HOME/terminfo:/usr/share/terminfo
set -x AZURE_CONFIG_DIR $XDG_CONFIG_HOME/azure
set -x CONDARC $XDG_CONFIG_HOME/conda/condarc
set -x PUB_CACHE $XDG_CACHE_HOME/pub-cache
set -x KUBECONFIG $XDG_CONFIG_HOME/kube/config
set -x GOMODCACHE $XDG_CACHE_HOME/go-mod
set -x GOBIN $HOME/.local/bin
set -x ANALYZER_STATE_LOCATION_OVERRIDE $XDG_CACHE_HOME/dart_server

set -x LANG en_US.UTF-8
set -x TERM xterm-256color
set -x GPG_TTY (tty)
set -x EDITOR vim
set -x GIT_MERGE_AUTOEDIT no
set -x LESSHISTFILE -
set -x RUBY_CODESIGN hsbt
set -x RUBYOPT -w
set -x RUBY_MN_THREADS 1
set -x RUBY_CONFIGURE_OPTS --disable-install-doc
set -x PKG_CONFIG_PATH /opt/homebrew/opt/imagemagick/lib/pkgconfig:/usr/lib/pkgconfig
set -x GHQ_ROOT $HOME/Documents
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --glob "!.git/*"'

# fish_add_path /opt/homebrew/opt/binutils/bin
# fish_add_path /opt/homebrew/opt/libtool/libexec/gnubin
fish_add_path /opt/homebrew/opt/coreutils/libexec/gnubin
fish_add_path /opt/homebrew/opt/findutils/libexec/gnubin
fish_add_path /opt/homebrew/opt/gnu-sed/libexec/gnubin
fish_add_path /opt/homebrew/opt/gnu-tar/libexec/gnubin
fish_add_path /opt/homebrew/opt/make/libexec/gnubin
fish_add_path /opt/homebrew/opt/grep/libexec/gnubin
fish_add_path /opt/homebrew/opt/bison/bin
fish_add_path /opt/homebrew/sbin # Move to the top of PATH again
fish_add_path /opt/homebrew/bin # Move to the top of PATH again
fish_add_path $KREW_ROOT/bin
fish_add_path $XDG_DATA_HOME/cargo/bin # Move to the top of PATH again
fish_add_path $XDG_DATA_HOME/npm/bin
fish_add_path $RBENV_ROOT/bin
fish_add_path $GEM_HOME/bin
fish_add_path $HOME/.local/bin
fish_add_path $HOME/Documents/github.com/hsbt/hsbt/toolbox/exe

abbr -a -U -- l lsd
abbr -a -U -- ll "lsd -la"
abbr -a -U -- e "code-insiders -a ."
abbr -a -U -- all-ruby "podman run --rm -it ghcr.io/ruby/all-ruby /all-ruby/all-ruby"
abbr -a -U -- s3cmd "s3cmd --config $HOME/.config/s3cmd"
abbr -a -U -- wget "wget --hsts-file=$XDG_CACHE_HOME/wget-hsts"
abbr -a -U -- gpg "gpg --homedir $XDG_DATA_HOME/gnupg"
abbr -a -U -- yarn "yarn --use-yarnrc $XDG_CONFIG_HOME/yarn/config"
abbr -a -U -- mvn "mvn -gs $XDG_CONFIG_HOME/maven/settings.xml"

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
