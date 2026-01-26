#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_DIR="$HOME/.claude"

echo "Installing AI config from $SCRIPT_DIR"

# Create Claude directory if it doesn't exist
mkdir -p "$CLAUDE_DIR"

# Backup and symlink function
backup_and_link() {
    local target="$1"
    local link="$2"
    
    if [ -e "$link" ] || [ -L "$link" ]; then
        if [ -L "$link" ]; then
            echo "  Removing existing symlink: $link"
            rm "$link"
        else
            local backup="${link}.backup.$(date +%Y%m%d%H%M%S)"
            echo "  Backing up existing file: $link -> $backup"
            mv "$link" "$backup"
        fi
    fi
    
    echo "  Creating symlink: $link -> $target"
    ln -s "$target" "$link"
}

echo ""
echo "Setting up Claude Code symlinks..."
backup_and_link "$SCRIPT_DIR/ai/claude/CLAUDE.md" "$CLAUDE_DIR/CLAUDE.md"
backup_and_link "$SCRIPT_DIR/ai/claude/agents" "$CLAUDE_DIR/agents"
backup_and_link "$SCRIPT_DIR/ai/claude/settings.json" "$CLAUDE_DIR/settings.json"

echo ""
echo "Done!"
echo ""
echo "Notes:"
echo "  - OpenCode uses OPENCODE_CONFIG_DIR env var (set in .zshrc)"
echo "  - Make sure to source your .zshrc or restart your shell"
echo "  - Run 'source ~/.zshrc' to apply changes"
