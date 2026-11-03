# config

Personal configuration files.

## Installation

This repo must be cloned to `~/config` for OpenCode to work properly (the instructions path uses `~` expansion).

```bash
git clone git@github.com:benbellick/config.git ~/config
~/config/setup.sh
```

The setup script links the tracked application configuration into the locations
expected by macOS and Linux. Existing files and symlinks are left untouched, so
managed workspace configuration is not overwritten.

## Emacs packages

Packages are declared in `.emacs` using `use-package` with `:straight t` or a
custom `:straight` recipe. Built-in and local packages do not need that keyword.
Straight installs missing packages and their dependencies at startup.

Run `~/config/setup.sh` before starting Emacs so `emacs/early-init.el` is linked.
This disables automatic activation of old `package.el` installations without
removing their files. The first startup downloads Straight and the declared packages.

- Upgrade packages with `M-x straight-pull-all`.
- After removing a declaration, restart Emacs and run
  `M-x straight-remove-unused-repos` to clean up unused repositories.
