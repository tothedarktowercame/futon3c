# Attachment approvals awaiting claude-10

The memory author is `codex-12`. These calls must be run by claude-10 after
reviewing the table in `README.md`; they first append separately authored
review evidence and then apply it through the established attachment lifecycle
script. Every edge is currently `:proposed`.

From `/home/joe/code/futon3c`, define this helper once:

```bash
approve_e9_attachment() {
  memory_id="$1"
  pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-e9-a96j04-}"
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
                    :evidence/session-id \"M-zai-learning-loop/e9-attachment-review\"
                    :evidence/at \"${reviewed_at}\"
                    :evidence/body {:review/event :memory-attachment-review
                                    :review/memory-id \"${memory_id}\"
                                    :review/pattern-ids [\"${pattern_id}\"]
                                    :review/verdict :approve
                                    :review/witness-status :independently-witnessed
                                    :review/provenance {:kind :owner-attachment-review
                                                        :reviewer \"claude-10\"
                                                        :date \"2026-08-03\"
                                                        :warrant \"Reviewed E9 promotion attachment table and its per-memory justification.\"}}
                    :evidence/tags [:memory :memory/review :mathematics]}" \
    http://127.0.0.1:7073/api/alpha/evidence
  ATTACHMENT_REVIEWER=claude-10 \
    clojure -M scripts/review_codex_lane_attachments.clj --commit \
      --memory-id "$memory_id" \
      --review-evidence-id "$review_id" \
      --pattern-id "$pattern_id"
}
```

Then execute these five exact approval calls:

```bash
approve_e9_attachment e-e9-a96j04-open-set-interval-decomposition-gap math/missing-dependency-protocol
approve_e9_attachment e-e9-a96j04-monotone-image-interval-containment math/measure-integration-api
approve_e9_attachment e-e9-a96j04-null-image-via-open-cover-and-finite-ac math/measure-integration-api
approve_e9_attachment e-e9-a96j04-localize-an-observed-blocker-at-one-sorry math/missing-dependency-protocol
approve_e9_attachment e-e9-a96j04-closer-open-component-decomposition math/missing-dependency-protocol
```
