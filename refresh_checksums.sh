#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MANIFEST_TMP="$ROOT/.PACKAGE_MANIFEST.tmp"
SHA_TMP="$ROOT/.SHA256SUMS.tmp"
trap 'rm -f "$MANIFEST_TMP" "$SHA_TMP"' EXIT

: > "$MANIFEST_TMP"
: > "$SHA_TMP"

while IFS= read -r -d '' path; do
  rel=".${path#$ROOT}"
  if [[ -L "$path" ]]; then
    printf 'l|%s|%s\n' "$rel" "$(readlink "$path")" >> "$MANIFEST_TMP"
  elif [[ -d "$path" ]]; then
    printf 'd|%s\n' "$rel" >> "$MANIFEST_TMP"
  elif [[ -f "$path" ]]; then
    size="$(stat -c '%s' "$path")"
    printf 'f|%s|%s\n' "$size" "$rel" >> "$MANIFEST_TMP"
    sha256sum "$path" | sed "s|  $ROOT/|  ./|" >> "$SHA_TMP"
  fi
done < <(find "$ROOT" \
  -path "$ROOT/runs" -prune -o \
  -name SHA256SUMS -prune -o \
  -name PACKAGE_MANIFEST.txt -prune -o \
  -name .PACKAGE_MANIFEST.tmp -prune -o \
  -name .SHA256SUMS.tmp -prune -o \
  -print0)

LC_ALL=C sort -o "$MANIFEST_TMP" "$MANIFEST_TMP"
LC_ALL=C sort -o "$SHA_TMP" "$SHA_TMP"
mv -f "$MANIFEST_TMP" "$ROOT/PACKAGE_MANIFEST.txt"
mv -f "$SHA_TMP" "$ROOT/SHA256SUMS"
trap - EXIT

echo "CHECKSUM_REFRESH_PASS files=$(wc -l < "$ROOT/SHA256SUMS")"
