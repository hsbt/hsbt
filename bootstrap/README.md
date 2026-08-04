# Bootstrap

One command sets up a machine: Homebrew packages from `Brewfile`, dotfile
symlinks from `dotfiles/`, and the `launchctl setenv` agents.

```
$ cd bootstrap && mise trust && mise bootstrap
```

Dotfiles are symlinked file by file into `~/.config` and `~/.local/bin`,
so editing either side edits the repository checkout directly. Run the
apply from the canonical checkout, not from a worktree; the links point
into whichever checkout applied them. The first apply on a machine that
still has the old copied files needs `--force`:

```
$ mise bootstrap dotfiles apply --force
```

## TODO

* Bootstrap to `git clone` to *env software.
* Invoke fisher commands.
* Alacritty and other xdg configurations.
* ...
