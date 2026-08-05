# Attachment approvals awaiting claude-10

The memories are authored by `ams-codex-1`. After reviewing `README.md`,
claude-10 can append independently authored review evidence and apply it with
the established lifecycle script.

```bash
approve_a97j02_attachment() {
  memory_id="$1"
  pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-a97j02-}"
  reviewed_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  curl --fail --silent --show-error \
    -H 'Accept: application/edn' -H 'Content-Type: application/edn' \
    -H 'X-Penholder: api' \
    --data-binary "{:evidence/id \"${review_id}\"
                    :evidence/subject {:ref/type :memory :ref/id \"${memory_id}\"}
                    :evidence/type :memory
                    :evidence/claim-type :observation
                    :evidence/author \"claude-10\"
                    :evidence/session-id \"M-zai-learning-loop/a97J02-attachment-review\"
                    :evidence/at \"${reviewed_at}\"
                    :evidence/body {:review/event :memory-attachment-review
                                    :review/memory-id \"${memory_id}\"
                                    :review/pattern-ids [\"${pattern_id}\"]
                                    :review/verdict :approve
                                    :review/witness-status :independently-witnessed
                                    :review/provenance {:kind :owner-attachment-review
                                                        :reviewer \"claude-10\"
                                                        :date \"2026-08-04\"
                                                        :warrant \"Reviewed a97J02 memory attachment and justification.\"}}
                    :evidence/tags [:memory :memory/review :mathematics]}" \
    http://127.0.0.1:7074/api/alpha/evidence
  FUTON_SUBSTRATE_URL=http://127.0.0.1:7074 ATTACHMENT_REVIEWER=claude-10 \
    clojure -M scripts/review_codex_lane_attachments.clj --commit \
      --memory-id "$memory_id" --review-evidence-id "$review_id" \
      --pattern-id "$pattern_id"
}
```

Exact pending calls:

```bash
approve_a97j02_attachment e-a97j02-closed-cthickening-measure-convergence-api math/measure-integration-api
approve_a97j02_attachment e-a97j02-measure-finite-union-closed-grid-cells math/measure-integration-api
approve_a97j02_attachment e-a97j02-select-grid-cell-with-nat-ceil math/measure-integration-api
```
