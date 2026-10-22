#!/usr/bin/env bash
# Regression test for the infinite loop in s:count_brackets (raise/splice hang
# when a bracket appears inside a trailing comment, char literal, etc.).
#
# Runs every test in cases/manifest.txt headlessly against the vim-sexp checkout
# this script lives in, loading nothing else (no vimrc, no packages), in up to
# three environments:
#
#   vim        Vim, legacy syntax engine
#   nvim-ts    Neovim with a Clojure Treesitter parser (skipped if none found)
#   nvim-syn   Neovim with g:sexp_prefer_legacy_syntax = 1
#
# Usage: test/count_brackets_hang.sh [options]
#   --vim PATH       Vim executable (default: vim; "none" to skip)
#   --nvim PATH      Neovim executable (default: nvim if on PATH; "none" to skip)
#   --parser-rtp DIR Directory containing parser/clojure.so for Neovim. By default
#                    the script looks under ~/.local/share/nvim for one, e.g. as
#                    installed by nvim-treesitter's :TSInstall clojure.
#   --timeout SECS   Per-test limit before declaring a hang (default: 10)
#   --show           Print the resulting buffer for every test
#   --only PATTERN   Run only tests whose file or command matches PATTERN (grep -E)
#
# Requirements: bash, GNU timeout (or gtimeout), and `script` (util-linux or BSD)
# to give Vim a pseudo-terminal. Neovim runs with --headless and needs neither.
#
# Exit status is non-zero if any test hangs, errors, or fails its expectation.
# To see the failures this guards against, run it on the commit before the fix.

set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CASES=$ROOT/test/cases

VIM=vim
NVIM=$(command -v nvim || echo none)
PARSER_RTP=
TIMEOUT=10
SHOW=0
ONLY=

while [ $# -gt 0 ]; do
    case $1 in
        --vim) VIM=$2; shift ;;
        --nvim) NVIM=$2; shift ;;
        --parser-rtp) PARSER_RTP=$2; shift ;;
        --timeout) TIMEOUT=$2; shift ;;
        --show) SHOW=1 ;;
        --only) ONLY=$2; shift ;;
        -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
    shift
done

die() { echo "error: $*" >&2; exit 2; }

if command -v timeout >/dev/null; then TIMEOUT_CMD=timeout
elif command -v gtimeout >/dev/null; then TIMEOUT_CMD=gtimeout
else die "need GNU timeout (coreutils) on PATH"; fi

# `script` differs between util-linux and BSD/macOS.
if [ "$VIM" != none ]; then
    command -v script >/dev/null || die "need \`script\` to give Vim a pseudo-terminal"
    if script --version 2>/dev/null | grep -q util-linux; then SCRIPT_STYLE=linux
    else SCRIPT_STYLE=bsd; fi
fi

TMP=$(mktemp -d "${TMPDIR:-/tmp}/vim-sexp-test.XXXXXX")
trap 'rm -rf "$TMP"' EXIT

# Vim option values need ' doubled inside a single-quoted string, and runtimepath
# entries need fnameescape() for spaces and commas.
vimquote() { printf "%s" "${1//\'/\'\'}"; }
rtp_cmd() { printf "exe 'set rtp^=' . fnameescape('%s')" "$(vimquote "$1")"; }

# Locate a Clojure Treesitter parser for Neovim.
if [ "$NVIM" != none ] && [ -z "$PARSER_RTP" ]; then
    so=$(find "${XDG_DATA_HOME:-$HOME/.local/share}/nvim" -path '*/parser/clojure.so' -print 2>/dev/null | head -1)
    [ -n "$so" ] && PARSER_RTP=$(dirname "$(dirname "$so")")
fi

# run_one ENV FILE LINE COL PLUG OPTION OUTFILE  ->  prints exit code
run_one() {
    local env=$1 file=$2 line=$3 col=$4 plug=$5 option=$6 out=$7
    local drive=$TMP/drive.vim
    {
        echo "call cursor($line, $col)"
        echo "exe \"normal \\<Plug>($plug)\""
        echo "exe 'w! ' . fnameescape('$(vimquote "$out")')"
        echo "qa!"
    } > "$drive"

    local -a opts
    opts=(-n -N -u NONE --cmd 'set loadplugins' --cmd 'set packpath=' --cmd "$(rtp_cmd "$ROOT")")
    case $env in
        nvim-ts)  opts+=(--cmd "$(rtp_cmd "$PARSER_RTP")") ;;
        nvim-syn) opts+=(--cmd 'let g:sexp_prefer_legacy_syntax = 1') ;;
    esac
    [ -n "$option" ] && opts+=(--cmd "let $option")
    opts+=(--cmd 'filetype plugin indent on' --cmd 'syntax on' -S "$drive" "$CASES/$file")

    rm -f "$out"
    if [ "$env" = vim ]; then
        # Vim needs a terminal. Wrap the command in a script so quoting survives `script`.
        local wrapper=$TMP/wrapper.sh
        { echo '#!/usr/bin/env bash'; printf 'exec %q' "$VIM"; printf ' %q' "${opts[@]}"; echo; } > "$wrapper"
        chmod +x "$wrapper"
        if [ "$SCRIPT_STYLE" = linux ]; then
            "$TIMEOUT_CMD" "$TIMEOUT" script -qec "$wrapper" /dev/null </dev/null >/dev/null 2>&1
        else
            "$TIMEOUT_CMD" "$TIMEOUT" script -q /dev/null "$wrapper" </dev/null >/dev/null 2>&1
        fi
    else
        "$TIMEOUT_CMD" "$TIMEOUT" "$NVIM" --headless "${opts[@]}" </dev/null >/dev/null 2>&1
    fi
    echo $?
}

# Confirm Neovim can actually parse Clojure with the given parser directory.
nvim_has_parser() {
    local probe=$TMP/probe.lua
    echo 'local ok = pcall(vim.treesitter.language.add, "clojure"); io.write(ok and "yes" or "no"); os.exit(0)' > "$probe"
    "$TIMEOUT_CMD" "$TIMEOUT" "$NVIM" --headless -u NONE --cmd "$(rtp_cmd "$PARSER_RTP")" -l "$probe" 2>/dev/null </dev/null | grep -q yes
}

envs=()
[ "$VIM" != none ] && envs+=(vim)
if [ "$NVIM" != none ]; then
    if [ -n "$PARSER_RTP" ] && nvim_has_parser; then
        envs+=(nvim-ts)
    else
        echo "note: no Clojure Treesitter parser found for Neovim; skipping nvim-ts." >&2
        echo "      Install one (e.g. :TSInstall clojure with nvim-treesitter) or pass" >&2
        echo "      --parser-rtp DIR where DIR/parser/clojure.so exists." >&2
    fi
    envs+=(nvim-syn)
fi
[ ${#envs[@]} -gt 0 ] || die "nothing to run: no Vim or Neovim"

failures=0
total=0
for env in "${envs[@]}"; do
    case $env in
        vim) label="$($VIM --version 2>/dev/null | head -1 | cut -d'(' -f1 | xargs), legacy syntax" ;;
        nvim-ts) label="$($NVIM --version | head -1) + Treesitter ($PARSER_RTP)" ;;
        nvim-syn) label="$($NVIM --version | head -1), legacy syntax forced" ;;
    esac
    echo "### $label"
    while read -r file line col plug expect option; do
        case $file in ''|'#'*) continue ;; esac
        [ -n "$ONLY" ] && ! printf '%s %s' "$file" "$plug" | grep -Eq "$ONLY" && continue
        : "${expect:=changed}"
        out=$TMP/out.clj
        rc=$(run_one "$env" "$file" "$line" "$col" "$plug" "${option:-}" "$out")
        total=$((total + 1))
        if [ "$rc" = 124 ]; then
            result="HANG (killed after ${TIMEOUT}s)"
        elif [ "$rc" != 0 ] || [ ! -f "$out" ]; then
            result="ERROR (exit $rc)"
        elif cmp -s "$out" "$CASES/$file"; then
            result="FAIL (buffer unchanged; mapping did nothing)"
        elif [ "$expect" = noindent ] && ! sed -n 2p "$out" | grep -q '^    '; then
            result="FAIL (list was reindented despite $option)"
        else
            result=ok
        fi
        [ "$result" = ok ] || failures=$((failures + 1))
        printf '%-36s %-19s %s\n' "$file" "$plug" "$result"
        if [ $SHOW = 1 ] && [ -f "$out" ]; then sed 's/^/    | /' "$out"; fi
    done < "$CASES/manifest.txt"
    echo
done

echo "$total tests, $failures failures"
[ $failures -eq 0 ]
