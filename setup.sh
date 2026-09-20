#!/usr/bin/env bash

set -euo pipefail

config_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

link_config() {
  local source=$1
  local target=$2

  if [[ -L "$target" ]]; then
    if [[ "$(readlink "$target")" == "$source" ]]; then
      printf 'Already linked: %s\n' "$target"
    else
      printf 'Skipping existing symlink: %s -> %s\n' "$target" "$(readlink "$target")"
    fi
    return
  fi

  if [[ -e "$target" ]]; then
    printf 'Skipping existing path: %s\n' "$target"
    return
  fi

  mkdir -p "$(dirname "$target")"
  ln -s "$source" "$target"
  printf 'Linked: %s -> %s\n' "$target" "$source"
}

link_config "$config_dir/nvim" "$HOME/.config/nvim"
link_config "$config_dir/.emacs" "$HOME/.emacs"
link_config "$config_dir/.vimrc" "$HOME/.vimrc"
link_config "$config_dir/ghostty/config" "$HOME/.config/ghostty/config"
link_config "$config_dir/.zshrc" "$HOME/.zshrc"
link_config "$config_dir/.gitconfig" "$HOME/.gitconfig"

if [[ "$(uname -s)" == "Darwin" ]]; then
  link_config "$config_dir/.aerospace.toml" "$HOME/.aerospace.toml"
fi
