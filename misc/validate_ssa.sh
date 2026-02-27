#!/usr/bin/env bash
# validate_ssa.sh — compare qbe vs sqbe-generated assembly for .ssa files
#
# Usage:
#   misc/validate_ssa.sh file.ssa [qbe_path]
#   misc/validate_ssa.sh all     [qbe_path]
#
# Must be run from the sqbe repo root.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

QBE="${2:-$REPO_ROOT/src/qbe/qbe}"
TRANSLATOR="$SCRIPT_DIR/ssa_to_api.py"
SRC_INCLUDE="$REPO_ROOT/src"

if [[ ! -x "$QBE" ]]; then
  echo "error: qbe not found at $QBE" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# validate one file; echoes PASS / FAIL / SKIP
# ---------------------------------------------------------------------------
validate_one() {
  local ssa="$1"
  local base
  base="$(basename "$ssa" .ssa)"

  local ref_s="/tmp/sqbe_ref_${base}.s"
  local gen_c="/tmp/sqbe_gen_${base}.c"
  local gen_exe="/tmp/sqbe_gen_${base}"
  local gen_s="/tmp/sqbe_gen_${base}.s"

  # Check for skip directive in file header
  if head -3 "$ssa" | grep -q '^# skip'; then
    echo "SKIP $ssa  (# skip directive)"
    return
  fi

  # Translate; exit code 2 means multi-way phi (unsupported)
  local rc=0
  python3 "$TRANSLATOR" "$ssa" > "$gen_c" 2>/tmp/sqbe_err_${base}.txt || rc=$?
  if [[ $rc -ne 0 ]]; then
    if [[ $rc -eq 2 ]]; then
      echo "SKIP $ssa  (3+ way phi)"
    else
      echo "FAIL $ssa  (translate error: $(head -1 /tmp/sqbe_err_${base}.txt))"
    fi
    return
  fi

  # Compile the generated C
  if ! clang -I "$SRC_INCLUDE" "$gen_c" -o "$gen_exe" 2>/tmp/sqbe_err_${base}.txt; then
    echo "FAIL $ssa  (compile error: $(head -1 /tmp/sqbe_err_${base}.txt))"
    return
  fi

  # Run to produce sqbe assembly
  if ! "$gen_exe" "$gen_s" 2>/tmp/sqbe_err_${base}.txt; then
    echo "FAIL $ssa  (runtime error: $(cat /tmp/sqbe_err_${base}.txt | head -1))"
    return
  fi

  # Generate reference assembly
  if ! "$QBE" -o "$ref_s" "$ssa" 2>/tmp/sqbe_err_${base}.txt; then
    echo "FAIL $ssa  (qbe error: $(cat /tmp/sqbe_err_${base}.txt | head -1))"
    return
  fi

  # Compare
  if diff -q "$ref_s" "$gen_s" > /dev/null 2>&1; then
    echo "PASS $ssa"
  else
    echo "FAIL $ssa  (assembly differs)"
    diff "$ref_s" "$gen_s" | head -20 || true
  fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
TARGET="${1:-}"

if [[ -z "$TARGET" ]]; then
  echo "usage: $0 <file.ssa|all> [qbe_path]" >&2
  exit 1
fi

if [[ "$TARGET" == "all" ]]; then
  pass=0; fail=0; skip=0
  for ssa in "$REPO_ROOT"/src/qbe/test/[!_]*.ssa; do
    result=$(validate_one "$ssa") || true
    echo "$result"
    case "$result" in
      PASS*) pass=$((pass+1)) ;;
      SKIP*) skip=$((skip+1)) ;;
      *)     fail=$((fail+1)) ;;
    esac
  done
  echo ""
  echo "Results: $pass passed, $fail failed, $skip skipped"
else
  validate_one "$TARGET"
fi
