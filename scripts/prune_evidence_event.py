#!/usr/bin/env python3
"""
Prune evidence entries matching a specific body event from the live evidence store.

Default mode is dry-run. Use --apply to delete matching entries through
Drawbridge against the live futon3c evidence backend. Matching entries are
backed up to JSONL before deletion.
"""

from __future__ import annotations

import argparse
import base64
import json
import sys
import urllib.request
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path


def http_json(url: str, timeout: float = 30.0) -> dict:
    with urllib.request.urlopen(url, timeout=timeout) as response:
        return json.loads(response.read().decode("utf-8"))


def fetch_all_entries(base_url: str) -> list[dict]:
    count_payload = http_json(f"{base_url}/api/alpha/evidence/count")
    count = int(count_payload.get("count", 0))
    if count <= 0:
        return []
    payload = http_json(f"{base_url}/api/alpha/evidence?limit={count}")
    return payload.get("entries", [])


def event_of(entry: dict) -> str:
    body = entry.get("evidence/body")
    if isinstance(body, dict):
        return body.get("event") or "(no event)"
    return ""


def date_of(entry: dict) -> str:
    ts = entry.get("evidence/at") or ""
    return ts[:10] if len(ts) >= 10 else "unknown"


def session_of(entry: dict) -> str:
    return entry.get("evidence/session-id") or "(no session)"


def type_of(entry: dict) -> str:
    return entry.get("evidence/type") or ""


def tags_of(entry: dict) -> set[str]:
    return {str(tag) for tag in (entry.get("evidence/tags") or [])}


def filter_entries(
    entries: list[dict],
    *,
    event: str | None,
    before: str | None,
    session_id: str | None,
    evidence_type: str | None,
    required_tags: list[str],
) -> list[dict]:
    matched = entries
    if event is not None:
        matched = [entry for entry in matched if event_of(entry) == event]
    if evidence_type is not None:
        matched = [entry for entry in matched if type_of(entry) == evidence_type]
    if required_tags:
        matched = [
            entry for entry in matched
            if all(tag in tags_of(entry) for tag in required_tags)
        ]
    if before:
        matched = [entry for entry in matched if (entry.get("evidence/at") or "") < before]
    if session_id:
        matched = [entry for entry in matched if entry.get("evidence/session-id") == session_id]
    return matched


def summarize(entries: list[dict]) -> dict:
    by_date = Counter(date_of(entry) for entry in entries)
    by_session = Counter(session_of(entry) for entry in entries)
    return {
        "count": len(entries),
        "top_dates": by_date.most_common(15),
        "top_sessions": by_session.most_common(15),
    }


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def load_admin_token(explicit: str | None) -> str:
    if explicit:
        return explicit
    return (repo_root_from_script() / ".admintoken").read_text().strip()


def backup_label(event: str | None, evidence_type: str | None, required_tags: list[str]) -> str:
    if event:
        return event
    if evidence_type and required_tags:
        return "__".join([evidence_type, *required_tags])
    if evidence_type:
        return evidence_type
    if required_tags:
        return "__".join(required_tags)
    return "filtered-evidence"


def backup_entries(entries: list[dict], label: str, backup_dir: Path | None) -> Path:
    if backup_dir is None:
        backup_dir = repo_root_from_script() / "tmp" / "evidence-prune-backups"
    backup_dir.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    safe_label = label.replace("/", "_")
    path = backup_dir / f"{stamp}__{safe_label}.jsonl"
    with path.open("w", encoding="utf-8") as handle:
        for entry in entries:
            handle.write(json.dumps(entry, ensure_ascii=False))
            handle.write("\n")
    return path


def apply_delete_batch(drawbridge_url: str, admin_token: str, evidence_ids: list[str]) -> dict:
    ids_json = json.dumps(evidence_ids, separators=(",", ":"))
    encoded = base64.b64encode(ids_json.encode("utf-8")).decode("ascii")
    form = f"""
(do
  (require '[cheshire.core :as json]
           '[futon3c.dev :as dev]
           '[futon3c.evidence.backend :as backend]
           '[futon3c.evidence.store :as store])
  (let [payload-str (String. (.decode (java.util.Base64/getDecoder) "{encoded}") "UTF-8")
        evidence-ids (json/parse-string payload-str)
        evidence-store (or @dev/!evidence-store store/!store)
        result (backend/-delete! evidence-store evidence-ids)]
    {{:attempted (count evidence-ids)
     :result result}}))
""".strip()
    request = urllib.request.Request(
        drawbridge_url,
        data=form.encode("utf-8"),
        headers={
            "x-admin-token": admin_token,
            "Content-Type": "text/plain",
        },
        method="POST",
    )
    with urllib.request.urlopen(request, timeout=120) as response:
        payload = response.read().decode("utf-8").strip()
    return {"raw": payload}


def batched(items: list[str], size: int) -> list[list[str]]:
    return [items[i:i + size] for i in range(0, len(items), size)]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--event")
    parser.add_argument("--no-event", action="store_true")
    parser.add_argument("--type")
    parser.add_argument("--tag", action="append", default=[])
    parser.add_argument("--session-id")
    parser.add_argument("--before")
    parser.add_argument("--base-url", default="http://127.0.0.1:7070")
    parser.add_argument("--drawbridge-url", default="http://127.0.0.1:6768/eval")
    parser.add_argument("--admin-token")
    parser.add_argument("--backup-dir", type=Path)
    parser.add_argument("--batch-size", type=int, default=500)
    parser.add_argument("--show", type=int, default=10)
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()

    event_filter = "(no event)" if args.no_event else args.event
    entries = fetch_all_entries(args.base_url)
    matched = filter_entries(
        entries,
        event=event_filter,
        before=args.before,
        session_id=args.session_id,
        evidence_type=args.type,
        required_tags=args.tag,
    )
    summary = summarize(matched)
    print(
        json.dumps(
            {
                "event": event_filter,
                "type": args.type,
                "tags": args.tag,
                "session_id": args.session_id,
                "before": args.before,
                "matched": summary["count"],
                "top_dates": summary["top_dates"],
                "top_sessions": summary["top_sessions"],
            },
            indent=2,
        )
    )
    for entry in matched[: args.show]:
        print(
            f"- {entry.get('evidence/at')} {session_of(entry)} "
            f"{entry.get('evidence/id')}"
        )

    if not args.apply:
        return 0

    if not matched:
        print("No matching entries.")
        return 0

    backup_path = backup_entries(
        matched,
        backup_label(event_filter, args.type, args.tag),
        args.backup_dir,
    )
    print(f"Backup: {backup_path}")

    evidence_ids = [entry["evidence/id"] for entry in matched if entry.get("evidence/id")]
    token = load_admin_token(args.admin_token)
    batch_results = []
    for idx, batch in enumerate(batched(evidence_ids, args.batch_size), start=1):
        result = apply_delete_batch(args.drawbridge_url, token, batch)
        batch_results.append(
            {
                "batch": idx,
                "size": len(batch),
                "result": result,
            }
        )
        print(json.dumps(batch_results[-1], indent=2))

    remaining = filter_entries(
        fetch_all_entries(args.base_url),
        event=event_filter,
        before=args.before,
        session_id=args.session_id,
        evidence_type=args.type,
        required_tags=args.tag,
    )
    print(
        json.dumps(
            {
                "remaining": len(remaining),
                "batches": len(batch_results),
                "attempted": len(evidence_ids),
            },
            indent=2,
        )
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
