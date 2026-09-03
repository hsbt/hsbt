# Every exported variable belongs here, not in .zshrc: .zshrc is skipped by
# non-interactive shells, so a tool started from a script or a hook would
# otherwise fall back to its default and recreate the dotfile under $HOME
# that the relocation exists to avoid. .zshrc keeps only what needs a tty.

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
export GEM_REPAIR_RUBIES=3.3-dev,3.4-dev,4.0-dev,ruby-dev
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"
export RUBOCOP_OPTS="--config $XDG_CONFIG_HOME/rubocop/config.yml"

export MIX_HOME="$XDG_DATA_HOME/mix"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME/aws/credentials"
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME/aws/config"
export PYLINTHOME="$XDG_CACHE_HOME/pylint"
export HELM_HOME="$XDG_DATA_HOME/helm"
export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
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
export CLAUDE_CONFIG_DIR="$XDG_CONFIG_HOME/claude"
export COPILOT_HOME="$XDG_CONFIG_HOME/copilot"
export PSQLRC="$XDG_CONFIG_HOME/pg/psqlrc"
export EDITRC="$XDG_CONFIG_HOME/editrc"
export LESSHISTFILE=-

# Terraform hard-codes ~/.terraform.d for checkpoint_{cache,signature} with no
# path override, so disable the checkpoint (version phone-home) entirely
export CHECKPOINT_DISABLE=1

export EDITOR=hx
export GIT_GOGET_ROOT="$HOME/Documents"
export HELIX_RUNTIME="$GIT_GOGET_ROOT/github.com/helix-editor/helix/runtime"
export GIT_MERGE_AUTOEDIT=no
export HOMEBREW_FORBIDDEN_FORMULAE="node npm pnpm yarn python"

export RUBY_CODESIGN=hsbt
export RUBYOPT=-w
export RUBY_MN_THREADS=1
export RUBY_YJIT_ENABLE=1
export RUBY_CONFIGURE_OPTS=--disable-install-doc
export MISE_ENV_FILE=.envrc
export CC='sccache clang'
export RUSTC_WRAPPER="$CARGO_HOME/bin/sccache"
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
