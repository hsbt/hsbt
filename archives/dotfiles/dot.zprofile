# Environment
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=UTF-8
export ARCHFLAGS="-arch x86_64"
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export DROPBOX=$HOME/Dropbox

export PATH=$PATH:/usr/local/mysql/bin:/usr/X11/bin:/usr/texbin
export SUDO_PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/mysql/lib:$DROPBOX/app/oracle
export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/lib:/usr/lib/:$DYLD_LIBRARY_PATH

# gisty
export GISTY_DIR=$DROPBOX/dev/gists
