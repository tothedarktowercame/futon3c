# Attachment approvals awaiting claude-10

The memories are authored by `ams-scribe-1`. After reviewing the attachment
table in `README.md`, claude-10 can append independent review evidence and
apply it with the established lifecycle script.

From `/home/joe/code/futon3c`, define:

```bash
approve_a09_attachment() {
  memory_id="$1"
  pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-a09-}"
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
                    :evidence/session-id \"M-zai-learning-loop/a94A09-attachment-review\"
                    :evidence/at \"${reviewed_at}\"
                    :evidence/body {:review/event :memory-attachment-review
                                    :review/memory-id \"${memory_id}\"
                                    :review/pattern-ids [\"${pattern_id}\"]
                                    :review/verdict :approve
                                    :review/witness-status :independently-witnessed
                                    :review/provenance {:kind :owner-attachment-review
                                                        :reviewer \"claude-10\"
                                                        :date \"2026-08-10\"
                                                        :warrant \"Reviewed a94A09 scribe attachment and its per-memory justification.\"}}
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
approve_a09_attachment e-a09-shrink-radius-rouche-fixed-point math/holomorphic-disk-api
approve_a09_attachment e-a09-compact-endpoint-limit-for-scaled-fixed-points math/holomorphic-disk-api
approve_a09_attachment e-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points math/missing-dependency-protocol
```
