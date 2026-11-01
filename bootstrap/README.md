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

## Windows

The same command sets up Windows after `scoop install mise`: scoop and
winget packages from `scoopfile.json` and `winget.json`, the user
environment variables from `system/windows-env.rb`, and the shared
dotfiles plus the Windows-only ones under `dotfiles/windows/`. Visual
Studio Build Tools are installed separately with their C++ components.
Real symlinks need Developer Mode. Run it
in Windows Terminal, not from Claude Desktop, which redirects writes under
AppData and would hide the helix link from everything else.

```
> cd V:\github.com\hsbt\hsbt\bootstrap; mise trust; mise bootstrap --force-dotfiles
```

`$PROFILE` stays in OneDrive as a stub that dot-sources
`~/.config/powershell/profile.ps1`, and `~/.gitconfig` keeps only the
`safe.directory` entries of the machine.

V: is a Dev Drive in `C:\Users\hsbt\DevDrive.vhdx`. The `Mount DevDrive`
task from `system/mount-devdrive.xml` attaches it at boot. Registering
the task needs an elevated PowerShell:

```
> Register-ScheduledTask -TaskName 'Mount DevDrive' -Xml (Get-Content V:\github.com\hsbt\hsbt\bootstrap\system\mount-devdrive.xml -Raw)
```

## TODO

* Bootstrap to `git clone` to *env software.
* Invoke fisher commands.
* Alacritty and other xdg configurations.
* ...
