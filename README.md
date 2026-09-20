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
