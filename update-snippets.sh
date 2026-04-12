#!/bin/bash
set -euo pipefail

lang_for_ext() {
  case "$1" in
    ml) echo "ocaml" ;;
    hs) echo "haskell" ;;
    py) echo "python" ;;
    js) echo "javascript" ;;
    ts) echo "typescript" ;;
    sh) echo "bash" ;;
    *)  echo "$1" ;;
  esac
}

update_file() {
  local md_file="$1"
  local md_dir
  md_dir="$(dirname "$md_file")"
  local tmpfile
  tmpfile="$(mktemp)"
  trap "rm -f '$tmpfile'" RETURN

  local skipping=false

  while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" =~ ^'<!-- snippet: '(.+)' -->'$ ]]; then
      local spec="${BASH_REMATCH[1]}"
      local filepath start end

      if [[ "$spec" =~ ^(.+)#L([0-9]+)-L([0-9]+)$ ]]; then
        filepath="${BASH_REMATCH[1]}"
        start="${BASH_REMATCH[2]}"
        end="${BASH_REMATCH[3]}"
      else
        filepath="$spec"
        start=""
        end=""
      fi

      local resolved="$md_dir/$filepath"
      if [[ ! -f "$resolved" ]]; then
        echo "warning: $filepath not found (referenced in $md_file)" >&2
        echo "$line" >> "$tmpfile"
        skipping=true
        continue
      fi

      local ext="${filepath##*.}"
      local lang
      lang="$(lang_for_ext "$ext")"

      echo "$line" >> "$tmpfile"
      echo '```'"$lang" >> "$tmpfile"
      if [[ -n "$start" && -n "$end" ]]; then
        sed -n "${start},${end}p" "$resolved" >> "$tmpfile"
      else
        cat "$resolved" >> "$tmpfile"
      fi
      echo '```' >> "$tmpfile"

      skipping=true
    elif [[ "$line" == '<!-- /snippet -->' ]]; then
      echo "$line" >> "$tmpfile"
      skipping=false
    elif [[ "$skipping" == false ]]; then
      echo "$line" >> "$tmpfile"
    fi
  done < "$md_file"

  mv "$tmpfile" "$md_file"
}

if [[ $# -eq 0 ]]; then
  echo "usage: update-snippets.sh FILE..." >&2
  exit 1
fi

for f in "$@"; do
  update_file "$f"
done
