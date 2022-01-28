set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_CACHE_HOME $HOME/.cache

set -x IRBRC $XDG_CONFIG_HOME/irb/irbrc
set -x GEMRC $XDG_CONFIG_HOME/gem/gemrc
set -x GEM_HOME $XDG_DATA_HOME/gem
set -x GEM_SPEC_CACHE $XDG_CACHE_HOME//gem
set -x BUNDLE_USER_CONFIG $XDG_CONFIG_HOME/bundle
set -x BUNDLE_USER_CACHE $XDG_CACHE_HOME/bundle
set -x BUNDLE_USER_PLUGIN $XDG_DATA_HOME/bundle
set -x RBENV_ROOT $XDG_DATA_HOME/rbenv
set -x CARGO_HOME $XDG_DATA_HOME/cargo
set -x MIX_HOME $XDG_DATA_HOME/mix
set -x GNUPGHOME $XDG_DATA_HOME/gnupg
set -x NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
set -x GOPATH $XDG_DATA_HOME/go
set -x GOBIN $HOME/.local/bin
set -x AWS_SHARED_CREDENTIALS_FILE $XDG_CONFIG_HOME/aws/credentials
set -x AWS_CONFIG_FILE $XDG_CONFIG_HOME/aws/config
set -x MAVEN_USER_HOME $XDG_CACHE_HOME/maven
set -x MAVEN_CONFIG "-gs $XDG_CONFIG_HOME/maven/settings.xml"
set -x PYLINTHOME $XDG_CACHE_HOME/pylint
set -x HELM_HOME $XDG_DATA_HOME/helm
set -x KREW_ROOT $XDG_DATA_HOME/krew
set -x VIMINIT ":source $XDG_CONFIG_HOME"/vim/vimrc
set -x NODE_REPL_HISTORY $XDG_DATA_HOME/node_repl_history
set -x VSCODE_PORTABLE "$XDG_DATA_HOME"/vscode

set -x LANG en_US.UTF-8
set -x GPG_TTY (tty)
set -x EDITOR vim
set -x DOCKER_BUILDKIT 1
set -x GIT_MERGE_AUTOEDIT no
set -x LESSHISTFILE -
set -x GO111MODULE on
set -x RUBY_CODESIGN hsbt
set -x RUBY_CONFIGURE_OPTS --disable-install-doc
set -x PKG_CONFIG_PATH /usr/local/opt/imagemagick@6/lib/pkgconfig:/usr/lib/pkgconfig
set -x _ZO_DATA_DIR $XDG_DATA_HOME/zoxide
set -x TEALDEER_CACHE_DIR $XDG_CACHE_HOME/tldr
set -x CLOUDSDK_PYTHON /usr/local/opt/python@3.8/libexec/bin/python
set -x CARGO_NET_GIT_FETCH_WITH_CLI true

set -x PATH $GOBIN $CARGO_HOME/bin $RBENV_ROOT/bin $KREW_ROOT/bin $PATH
set -x PATH /usr/local/sbin $PATH
set -x PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
set -x PATH /usr/local/opt/findutils/libexec/gnubin $PATH
set -x PATH /usr/local/opt/gnu-sed/libexec/gnubin $PATH
set -x PATH /usr/local/opt/gnu-tar/libexec/gnubin $PATH
set -x PATH /usr/local/opt/make/libexec/gnubin $PATH
set -x PATH /usr/local/opt/grep/libexec/gnubin $PATH
set -x PATH /usr/local/opt/bison/bin $PATH
set -x PATH $HOME/Library/Python/3.9/bin $PATH
set -x PATH $HOME/Documents/github.com/hsbt/hsbt/exe $PATH
