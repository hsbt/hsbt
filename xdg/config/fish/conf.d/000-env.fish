set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_CACHE_HOME $HOME/.cache

set -x GEMRC $XDG_CONFIG_HOME/gem/gemrc
set -x GEM_HOME $XDG_DATA_HOME/gem
set -x GEM_SPEC_CACHE $HOME/.cache/gem
set -x IRBRC $XDG_CONFIG_HOME/irb/irbrc
set -x BUNDLE_USER_CONFIG $XDG_CONFIG_HOME/bundle
set -x BUNDLE_USER_CACHE $XDG_CACHE_HOME/bundle
set -x BUNDLE_USER_PLUGIN $XDG_DATA_HOME/bundle
set -x RBENV_ROOT $XDG_DATA_HOME/rbenv
set -x NODENV_ROOT $XDG_DATA_HOME/nodenv
set -x PYENV_ROOT $XDG_DATA_HOME/pyenv
set -x CARGO_HOME $XDG_DATA_HOME/cargo
set -x RUSTUP_HOME $XDG_DATA_HOME/rustup
set -x CSEARCHINDEX $XDG_CACHE_HOME/csearchindex
set -x GNUPGHOME $XDG_DATA_HOME/gnupg
set -x NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
set -x GOPATH $XDG_DATA_HOME/go
set -x AWS_SHARED_CREDENTIALS_FILE $XDG_CONFIG_HOME/aws/credentials
set -x AWS_CONFIG_FILE $XDG_CONFIG_HOME/aws/config
set -x MAVEN_USER_HOME $XDG_CACHE_HOME/maven
set -x MAVEN_CONFIG "-gs $XDG_CONFIG_HOME/maven/settings.xml"

set -x LANG en_US.UTF-8
set -x EDITOR vim
set -x DOCKER_BUILDKIT 1
set -x GIT_MERGE_AUTOEDIT no
set -x LESSHISTFILE -
set -x RUBY_CODESIGN hsbt
set -U Z_CMD "j"
set -U ZO_CMD "jo"

set -x PATH $GOPATH/bin $CARGO_HOME/bin $RBENV_ROOT/bin $NODENV_ROOT/bin $PYENV_ROOT/bin $PATH
set -x PATH $HOME/Library/Developer/Toolchains/Custom.xctoolchain/usr/bin $PATH
set -x PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
set -x PATH /usr/local/opt/findutils/libexec/gnubin $PATH
set -x PATH /usr/local/opt/gnu-sed/libexec/gnubin $PATH
set -x PATH /usr/local/opt/gnu-tar/libexec/gnubin $PATH
set -x PATH /usr/local/opt/grep/libexec/gnubin $PATH
set -x PATH /usr/local/opt/bison/bin $PATH
