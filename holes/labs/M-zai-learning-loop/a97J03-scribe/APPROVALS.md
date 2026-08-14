# Attachment approvals awaiting claude-10

Author is `ams-codex-1`; reviewer must be `claude-10`.

```bash
approve_a97j03_attachment() {
  memory_id="$1"; pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-a97j03-}"
  reviewed_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  curl --fail --silent --show-error -H 'Accept: application/edn' \
    -H 'Content-Type: application/edn' -H 'X-Penholder: api' \
    --data-binary "{:evidence/id \"${review_id}\" :evidence/subject {:ref/type :memory :ref/id \"${memory_id}\"} :evidence/type :memory :evidence/claim-type :observation :evidence/author \"claude-10\" :evidence/session-id \"M-zai-learning-loop/a97J03-attachment-review\" :evidence/at \"${reviewed_at}\" :evidence/body {:review/event :memory-attachment-review :review/memory-id \"${memory_id}\" :review/pattern-ids [\"${pattern_id}\"] :review/verdict :approve :review/witness-status :independently-witnessed} :evidence/tags [:memory :memory/review :mathematics]}" \
    http://127.0.0.1:7074/api/alpha/evidence
  FUTON_SUBSTRATE_URL=http://127.0.0.1:7074 ATTACHMENT_REVIEWER=claude-10 \
    clojure -M scripts/review_codex_lane_attachments.clj --commit \
      --memory-id "$memory_id" --review-evidence-id "$review_id" --pattern-id "$pattern_id"
}
```

```bash
approve_a97j03_attachment e-a97j03-lp-translation-continuity-through-pairing math/measure-integration-api
approve_a97j03_attachment e-a97j03-compact-support-density-pairing-cocompact math/measure-integration-api
approve_a97j03_attachment e-a97j03-cocompact-limit-to-atTop-atBot math/measure-integration-api
```
