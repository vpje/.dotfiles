#!/usr/bin/env bash
# Resolve a logical workspace "slot" (1-6) to the correct absolute workspace ID
# depending on which location's monitors are currently connected, then dispatch
# it to hyprland.
#
# This exists because Hyprland's `workspace = N, monitor:desc:...` rules only keep
# the FIRST rule parsed for a given N - reusing the same N across locations (e.g.
# Home 4K and Work screen 1 both wanting workspace 1) silently breaks the second
# location. See monitors.conf for the full explanation and the actual unique IDs.
#
# Usage: wsgo.sh <slot 1-6> [dispatcher]
#   dispatcher defaults to "workspace"; pass "movetoworkspace" to move the focused
#   window instead of just switching.

set -euo pipefail

slot="${1:?usage: wsgo.sh <slot> [dispatcher]}"
dispatcher="${2:-workspace}"

home4k="Samsung Electric Company U32J59x HTPK701161"
work1="Dell Inc. DELL U2412M 0FFXD45J3TCS"
homelg="LG Electronics LG FHD 503AXFL0E323"
work2="Dell Inc. DELL 2209WA H735H9B928YL"

descs="$(hyprctl monitors -j | jq -r '.[].description')"

is_connected() { grep -qxF "$1" <<<"$descs"; }

target="$slot"

case "$slot" in
  1|2|3)
    if ! is_connected "$home4k" && is_connected "$work1"; then
      target=$((slot + 10))
    fi
    ;;
  4|5|6)
    if ! is_connected "$homelg" && is_connected "$work2"; then
      target=$((slot + 10))
    fi
    ;;
esac

exec hyprctl dispatch "$dispatcher" "$target"
