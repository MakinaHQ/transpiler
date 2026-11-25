#!/usr/bin/env bash
# Prints the Merkle root and file sha256 of every rootfile in a makina-integrations
# checkout, so transpiler changes that alter them show up as a diff.
#
# Usage: scripts/integration-roots.sh <transpiler-bin> <integrations-dir>
#
# One sorted line per entry: <check> <rootfile> <root> <sha256>
#   A  every committed rootfile: root from `root --rootfile`, sha256 of the committed file.
#   B  newest rootfile per machine/chain, rebuilt from its caliber.yaml with the prod
#      token list: root and sha256 of the generated TOML.
# A failing transpiler run is recorded with root ERR (and sha256 - for B) and its
# stderr is forwarded, so existing failures are part of the baseline.
set -euo pipefail
export LC_ALL=C

transpiler=$(realpath "$1")
cd "$2"
token_list=token-lists/prod-token-list.json
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

report_error() {
    echo "::warning::$1 failed:" >&2
    cat "$tmp/err" >&2
}

{
    for f in machines/*/*/rootfiles/*.toml; do
        sha=$(sha256sum <"$f" | cut -d' ' -f1)
        if root=$("$transpiler" root --rootfile "$f" 2>"$tmp/err"); then
            echo "A $f $root $sha"
        else
            report_error "root --rootfile $f"
            echo "A $f ERR $sha"
        fi
    done

    for d in machines/*/*/rootfiles; do
        files=("$d"/*.toml)
        latest=${files[-1]}
        caliber=$(dirname "$d")/caliber.yaml
        generated=$tmp/generated.toml
        # -t and -o are not global flags, so they must precede the subcommand.
        if "$transpiler" -t "$token_list" -o "$generated" -i "$caliber" transpile >/dev/null 2>"$tmp/err" &&
            root=$("$transpiler" -t "$token_list" -i "$caliber" root 2>"$tmp/err"); then
            echo "B $latest $root $(sha256sum <"$generated" | cut -d' ' -f1)"
        else
            report_error "rebuilding $caliber"
            echo "B $latest ERR -"
        fi
        rm -f "$generated"
    done
} | sort
