---
name: workspaces-xdg-open
description: Diagnose and repair xdg-open (and $BROWSER) not opening URLs on the local Mac from Datadog workspaces. Use when xdg-open, gh browse, OAuth login links, or "open in browser" silently do nothing or fail inside a workspace session.
---

# Workspaces xdg-open

## How it works

On a workspace, `xdg-open` links to `workspaces-tool-helper`. It writes the URL to `$WORKSPACES_DAEMON_SOCKET` (`~/.local/state/workspaces/daemon.sock`), which is an SSH `RemoteForward` back to the local `workspaces` daemon at `~/.local/state/workspaces/daemon.sock`. The daemon opens the browser and logs to `~/.local/state/workspaces/daemon.log`.

## Usual cause

The workspace sshd unlinks and rebinds the remote socket for every new SSH connection that carries the forward. When that newer connection exits, the socket file stays but nothing listens on it. Long-lived sessions (herdr, tmux over ssh) then get `Connection refused`. Any one-off `ssh workspace-<name> ...` can cause this, including your own diagnostic commands.

Never diagnose with a plain `ssh workspace-<name>`. Probe through an existing control master with `-o ClearAllForwardings=yes`, which the script does.

## Fix

Run the script. It finds live control masters (herdr `/tmp/hssh-$UID/*`, `~/.ssh/ws-*.socket`, wmux), checks each workspace's remote socket, and re-binds the forward (`-O cancel` then `-O forward`) only where it is stale.

```bash
~/config/ai/pi/skills/workspaces-xdg-open/scripts/fix-xdg-open.sh --check   # report only
~/config/ai/pi/skills/workspaces-xdg-open/scripts/fix-xdg-open.sh           # repair all
~/config/ai/pi/skills/workspaces-xdg-open/scripts/fix-xdg-open.sh investigation
```

`-O forward` alone is a no-op because the master still thinks it owns the forward. The cancel is required.

## If the script doesn't fix it

- If the local daemon is not listening, run `workspaces daemon start` and check `launchctl list | grep dog.infra.workspace.daemon`.
- If no control master is found, the session is a plain non-multiplexed ssh. Reconnect it.
- Check `daemon.log` for the request. `invalid or non-http(s) URL` means a file path was passed, and the daemon only opens http(s) URLs. `ssh agent unreachable` lines involve the localhost port-forward feature (1Password agent), not plain URL opens.
- On the remote, `ls -l $(which xdg-open)` should point at `/opt/dogbrew/bin/workspaces-tool-helper`.
