export LANG=en_US.UTF-8

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/Library/Caches" # macOS default

export GNUPGHOME="$XDG_DATA_HOME/gnupg"

export IRBRC="$XDG_CONFIG_HOME/irb/irbrc"
export GEMRC="$XDG_CONFIG_HOME/gem/gemrc"
export GEM_HOME="$XDG_DATA_HOME/gem"
export GEM_SPEC_CACHE="$XDG_CACHE_HOME/gem"
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"
export RUBOCOP_OPTS="--config $XDG_CONFIG_HOME/rubocop/config.yml"

export RUBY_CODESIGN=hsbt
export RUBYOPT=-w
export RUBY_MN_THREADS=1
export RUBY_YJIT_ENABLE=1
export RUBY_CONFIGURE_OPTS=--disable-install-doc
export MISE_ENV_FILE=.envrc
export CC='sccache clang'
export MAKEFLAGS="-j$(sysctl -n hw.logicalcpu)"
export PKG_CONFIG_PATH="/opt/homebrew/opt/imagemagick/lib/pkgconfig:/usr/lib/pkgconfig"

# macOS defaults the open-files soft limit to 256, which sccache exhausts
# under parallel builds: a bare `make -j` overrides MAKEFLAGS and floods the
# sccache server until it dies with EMFILE. The hard limit is unlimited, so
# raise the soft limit without sudo.
ulimit -n 65536

# bison and m4 stay duplicated in .zshrc because path_helper in
# /etc/zprofile demotes .zshenv entries behind system paths in login shells
export PATH="/opt/homebrew/opt/m4/bin:$PATH"
export PATH="/opt/homebrew/opt/bison/bin:$PATH"
export PATH="$XDG_DATA_HOME/mise/shims:$PATH"
export PATH="$GEM_HOME/bin:$PATH"
