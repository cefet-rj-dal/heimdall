#!/usr/bin/env bash
# Reads NUL-separated paths on standard input and fails when one of them
# cannot be checked out on Windows, or when a text file still holds merge
# conflict markers. Used by .githooks/pre-commit and by the repository-checks
# workflow.
status=0
marker='^(<{7}|>{7})( |$)'
while IFS= read -r -d '' path; do
  case "$path" in
    *[':*?"<>|\']*)
      echo "Invalid file name on Windows: $path" >&2
      status=1
      ;;
  esac
  if [ -f "$path" ] && grep -Iq . "$path" 2>/dev/null && grep -nE "$marker" "$path" >/dev/null 2>&1; then
    echo "Unresolved merge conflict markers in: $path" >&2
    grep -nE "$marker" "$path" | head -5 >&2
    status=1
  fi
done
exit $status
