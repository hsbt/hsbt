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

The same command sets up Windows: scoop and winget packages from
`scoopfile.json` and `winget.json`, the user environment variables from
`system/windows-env.rb`, and the shared dotfiles plus the Windows-only
ones under `dotfiles/windows/`. Visual Studio Build Tools are installed
separately with their C++ components.

Git comes from winget rather than scoop. scoop keeps it behind a
`current` junction, which the SSH sessions of Claude Desktop refuse to
follow (see below). scoop needs git to add its buckets and mise itself
comes from scoop, so install Git first, then scoop, then mise from a
new terminal:

```
> winget install Git.Git
> scoop install mise
```

Turn Developer Mode on before the first run. Without it Windows refuses
the symlinks, and `dotfiles.windows_symlinks` in `mise.toml` makes the
apply fail instead of falling back to copies. The value below is 1 when
it is on. Turning it on needs an administrator, in the Settings page that
`ms-settings:developers` opens:

```
> (Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock).AllowDevelopmentWithoutDevLicense
```

Run the bootstrap from PowerShell in Windows Terminal, not from Claude
Desktop, which redirects the folders it creates under AppData and would
hide the helix link from everything else. The commands here are
PowerShell, and cmd.exe reads `trust;` below as a task name.

```
> cd V:\github.com\hsbt\hsbt\bootstrap; mise trust; mise bootstrap --force-dotfiles
```

The bootstrap installs PowerShell 7 from its MSI (see
`system/windows-pwsh.rb`). On a machine that has only the MSIX one from
an earlier bootstrap, remove it first:

```
> winget uninstall --id Microsoft.PowerShell
```

Claude Code runs its hooks with the msys2 bash that
`CLAUDE_CODE_GIT_BASH_PATH` points to. The 2026-09-27 msys2 in scoop
ships msys2-runtime 3.6.10-5, which never returns from a WindowsApps
alias such as the `python3` Store stub (msys2/msys2-runtime#372), so a
hook that looks for Python holds every prompt for ten minutes. Update
the runtime to 3.6.10-6 or later from the `msys2` shell after the
bootstrap, and again whenever scoop reinstalls msys2:

```
$ pacman -Syu
$ pacman -Q msys2-runtime
```

Claude Desktop runs its SSH sessions under RedirectionGuard, which
refuses to follow a junction or symlink that a non-elevated process
created. scoop's `current` junctions, the rbmanager Ruby and the dotfile
links all come from the bootstrap in Windows Terminal, so on a machine
reached over SSH recreate them from an elevated shell, which a plain ssh
login already is. Do the same after a later `scoop update`, `rb install`
or `rb use`. The apply leaves links that already point at the right file
alone, so delete the dotfile links before it:

```
> scoop reset *
> rb use <version>
> Get-ChildItem ~\.config, $env:APPDATA\helix -Recurse -Force -Attributes ReparsePoint | Where-Object Target -like 'V:\github.com\hsbt\hsbt\*' | ForEach-Object { $_.Delete() }
> cd V:\github.com\hsbt\hsbt\bootstrap; mise bootstrap dotfiles apply
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

WSL and ChatGPT are MSIX packages that refuse to install without an
administrator, even with UAC prompts turned off, so they are in
`winget-admin.json` rather than `winget.json`. Import it from an elevated
PowerShell after the bootstrap:

```
> winget import -i V:\github.com\hsbt\hsbt\bootstrap\winget-admin.json --no-upgrade
```

## TODO

* Bootstrap to `git clone` to *env software.
* Invoke fisher commands.
* Alacritty and other xdg configurations.
* ...
