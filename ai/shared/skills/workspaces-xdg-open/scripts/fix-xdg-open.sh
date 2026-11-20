#!/usr/bin/env bash
# Probe and repair the workspaces daemon socket forward used by xdg-open.
#
# Usage: fix-xdg-open.sh [--check] [workspace-name...]
#   --check   report only; do not re-bind any forwards
#   names     limit to these workspaces (WORKSPACE_NAME), default all found
#
# Works only through existing SSH control masters, and passes
# ClearAllForwardings to every probe, so it never clobbers the socket itself.

set -uo pipefail

check_only=false
if [[ "${1:-}" == "--check" ]]; then
  check_only=true
  shift
fi
wanted=("$@")

local_socket="$HOME/.local/state/workspaces/daemon.sock"

remote_probe='printf "%s\n%s\n" "$WORKSPACE_NAME" "$WORKSPACES_DAEMON_SOCKET"
python3 - "$WORKSPACES_DAEMON_SOCKET" <<EOF 2>/dev/null || echo DEAD
import socket, sys
s = socket.socket(socket.AF_UNIX)
s.settimeout(3)
s.connect(sys.argv[1])
print("LISTENING")
EOF'

mux() {
  ssh -S "$1" -o ClearAllForwardings=yes -o BatchMode=yes workspace "${@:2}" 2>/dev/null
}

is_wanted() {
  [[ ${#wanted[@]} -eq 0 ]] && return 0
  local name
  for name in "${wanted[@]}"; do [[ "$name" == "$1" ]] && return 0; done
  return 1
}

if ! python3 -c "import socket,sys;s=socket.socket(socket.AF_UNIX);s.settimeout(3);s.connect(sys.argv[1])" "$local_socket" 2>/dev/null; then
  echo "local daemon not listening on $local_socket; run: workspaces daemon start" >&2
  exit 1
fi

# Long-lived masters first, so they are preferred for carrying the forward.
shopt -s nullglob
candidates=(/tmp/hssh-"$(id -u)"/* "$HOME"/.ssh/ws-*.socket "$HOME"/.local/state/wmux/agent/ssh-*.sock)

declare -A master_for status_for remote_socket_for
for control in "${candidates[@]}"; do
  [[ -S "$control" ]] || continue
  ssh -S "$control" -O check workspace >/dev/null 2>&1 || continue
  output=$(mux "$control" "$remote_probe") || continue
  { read -r name; read -r remote_socket; read -r state; } <<<"$output"
  [[ -n "$name" ]] && is_wanted "$name" || continue
  [[ -n "${master_for[$name]:-}" ]] && continue
  master_for[$name]=$control
  remote_socket_for[$name]=$remote_socket
  status_for[$name]=$state
done

if [[ ${#master_for[@]} -eq 0 ]]; then
  echo "no live SSH control masters to workspaces found" >&2
  exit 1
fi

exit_code=0
for name in "${!master_for[@]}"; do
  control=${master_for[$name]}
  if [[ "${status_for[$name]}" == "LISTENING" ]]; then
    echo "$name: ok"
    continue
  fi
  if $check_only; then
    echo "$name: STALE (master $control)"
    exit_code=2
    continue
  fi
  forward="${remote_socket_for[$name]}:$local_socket"
  ssh -S "$control" -O cancel -R "$forward" workspace >/dev/null 2>&1
  ssh -S "$control" -O forward -R "$forward" workspace >/dev/null 2>&1
  state=$(mux "$control" "$remote_probe" | sed -n 3p)
  if [[ "$state" == "LISTENING" ]]; then
    echo "$name: repaired via $control"
  else
    echo "$name: STILL BROKEN via $control; reconnect that session" >&2
    exit_code=1
  fi
done
exit $exit_code
