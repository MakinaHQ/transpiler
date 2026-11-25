#!/usr/bin/env bash
# Renders a Markdown summary of the differences between two outputs of
# integration-roots.sh (snapshot first). Exits 1 when any entry differs.
#
# Usage: scripts/integration-roots-report.sh <snapshot> <current>
set -euo pipefail
export LC_ALL=C

ref=$(sed -n 's/^integrations-ref: //p' "$1")

awk -v ref="$ref" -v max_rows=60 '
function short(h) { return length(h) > 18 ? substr(h, 1, 10) "…" substr(h, length(h) - 5) : h }
function change(a, b) { return a == b ? "`" short(a) "`" : "`" short(a) "` → `" short(b) "`" }
/^integrations-ref:/ { next }
{
    key = $1 " " $2
    if (!(key in seen)) { seen[key] = 1; keys[++n] = key }
    if (FILENAME == ARGV[1]) { old_root[key] = $3; old_sha[key] = $4 }
    else { new_root[key] = $3; new_sha[key] = $4 }
}
END {
    for (i = 1; i <= n; i++) {
        k = keys[i]
        if (!(k in new_root)) kind = "removed"
        else if (!(k in old_root)) kind = "added"
        else if (old_root[k] != new_root[k]) kind = "**root**"
        else if (old_sha[k] != new_sha[k]) kind = "file only"
        else continue
        if (kind == "**root**") roots++; else if (kind == "file only") files++; else other++
        split(k, parts, " ")
        check = parts[1] == "A" ? "committed" : "rebuilt"
        o_root = k in old_root ? old_root[k] : "-"; n_root = k in new_root ? new_root[k] : "-"
        o_sha = k in old_sha ? old_sha[k] : "-"; n_sha = k in new_sha ? new_sha[k] : "-"
        row = "| `" parts[2] "` | " check " | " kind " | " change(o_root, n_root) " | " change(o_sha, n_sha) " |"
        if (kind == "**root**") root_rows[roots] = row; else other_rows[files + other] = row
        changed++
    }
    # Root changes first, as they matter most.
    for (i = 1; i <= roots; i++) rows[i] = root_rows[i]
    for (i = 1; i <= files + other; i++) rows[roots + i] = other_rows[i]

    print "<!-- integration-roots -->"
    print "### Integration rootfiles"
    print ""
    if (!changed) {
        printf "✅ All %d entries match the snapshot for MakinaHQ/makina-integrations@%s.\n", n, substr(ref, 1, 12)
        exit 0
    }
    printf "❌ %d of %d entries differ from the snapshot for MakinaHQ/makina-integrations@%s: ", changed, n, substr(ref, 1, 12)
    printf "%d root changes, %d file-only changes", roots, files
    if (other) printf ", %d added or removed", other
    print "."
    print ""
    print "*committed* is `root --rootfile` on the committed file; *rebuilt* is the newest rootfile per machine/chain transpiled from its `caliber.yaml`."
    print ""
    print "| Rootfile | Check | Change | Root | File sha256 |"
    print "| --- | --- | --- | --- | --- |"
    for (i = 1; i <= changed && i <= max_rows; i++) print rows[i]
    if (changed > max_rows) printf "\n…and %d more; see the job log for the full diff.\n", changed - max_rows
    print ""
    print "If this change is intended, bump `integrations-ref` if needed, regenerate `tests/integration-roots.snapshot` (see README) and commit it."
    exit 1
}
' "$1" "$2"
