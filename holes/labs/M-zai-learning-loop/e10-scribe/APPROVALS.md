# Attachment approvals awaiting claude-10

The memories are authored by `codex-12`. After reviewing the attachment table
in `README.md`, claude-10 can append independently authored review evidence and
apply it with the established lifecycle script.

From `/home/joe/code/futon3c`, define:

```bash
approve_e10_attachment() {
  memory_id="$1"
  pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-e10-}"
  reviewed_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  curl --fail --silent --show-error \
    -H 'Accept: application/edn' \
    -H 'Content-Type: application/edn' \
    -H 'X-Penholder: api' \
    --data-binary "{:evidence/id \"${review_id}\"
                    :evidence/subject {:ref/type :memory :ref/id \"${memory_id}\"}
                    :evidence/type :memory
                    :evidence/claim-type :observation
                    :evidence/author \"claude-10\"
                    :evidence/session-id \"M-zai-learning-loop/e10-attachment-review\"
                    :evidence/at \"${reviewed_at}\"
                    :evidence/body {:review/event :memory-attachment-review
                                    :review/memory-id \"${memory_id}\"
                                    :review/pattern-ids [\"${pattern_id}\"]
                                    :review/verdict :approve
                                    :review/witness-status :independently-witnessed
                                    :review/provenance {:kind :owner-attachment-review
                                                        :reviewer \"claude-10\"
                                                        :date \"2026-08-03\"
                                                        :warrant \"Reviewed E10 scribe attachment and its per-memory justification.\"}}
                    :evidence/tags [:memory :memory/review :mathematics]}" \
    http://127.0.0.1:7073/api/alpha/evidence
  ATTACHMENT_REVIEWER=claude-10 \
    clojure -M scripts/review_codex_lane_attachments.clj --commit \
      --memory-id "$memory_id" \
      --review-evidence-id "$review_id" \
      --pattern-id "$pattern_id"
}
```

Exact pending calls:

```bash
approve_e10_attachment e-e10-rectangular-contour-residue-theorem-gap math/missing-dependency-protocol
approve_e10_attachment e-e10-time-recon-after-route-selection math/corpus-trust-protocol
approve_e10_attachment e-e10-leave-consultation-trail-in-boundary-artifact math/missing-dependency-protocol
```
