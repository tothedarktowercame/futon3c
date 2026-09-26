#!/usr/bin/env python3
"""Transfer the reviewed local mathematics memory corpus to zone-joe.

The export side performs GET requests only.  The import side refuses the
local futon1b port (7073), and is intended to run over SSH on zone-joe where
the ams-store is loopback-only on port 7074.  Evidence is append-only in the
deployed store: reruns read back and byte-compare an existing id, no-op only
when it is identical, and fail on a conflicting duplicate.  Hyperedges use
the same stricter preflight even though their endpoint supports replacement.

The dump is line-oriented EDN.  Each line is an envelope whose payload is the
verbatim EDN returned by the source store.  Hyperedge payloads retain the four
canonical write fields verbatim; the GET endpoint's derived ``:prop/*`` and
``:hx/ends`` projections are deliberately excluded from the write payload.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any, Iterable, Mapping, Sequence


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_EXPORT_DIR = SCRIPT_DIR / "corpus-export"
DEFAULT_LOCAL_STORE = "http://127.0.0.1:7073"
DEFAULT_REMOTE_STORE = "http://127.0.0.1:7074"
DEFAULT_SSH_HOST = "zone-joe"
PAGE_LIMIT = 500
MAX_COMMITTED_DUMP = 1_000_000
FAMILY_PREFIXES = ("e-e9-", "e-j07-", "e-e10-", "e-j02-", "e-a97j01-", "e-a97j02-")
REPRESENTATIVE_ID = "e-j02-translation-symmdiff-preimage-api"
TAG_PROBE = ("open-set", "interval-decomposition")


class TransferError(RuntimeError):
    """The corpus selection, transport, or verification contract failed."""


def canonical_json_bytes(value: Any) -> bytes:
    return json.dumps(
        value, ensure_ascii=False, sort_keys=True, separators=(",", ":")
    ).encode("utf-8")


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def request_bytes(
    method: str,
    url: str,
    *,
    accept: str,
    content_type: str | None = None,
    body: bytes | None = None,
    timeout: float = 60.0,
) -> tuple[int, bytes]:
    headers = {"Accept": accept}
    if content_type:
        headers["Content-Type"] = content_type
        headers["X-Penholder"] = "api"
    request = urllib.request.Request(url, data=body, headers=headers, method=method)
    for attempt in range(8):
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                return response.status, response.read()
        except urllib.error.HTTPError as exc:
            response_body = exc.read()
            if exc.code == 503 and attempt < 7:
                time.sleep(min(2**attempt, 8))
                continue
            return exc.code, response_body
        except urllib.error.URLError as exc:
            if attempt < 7:
                time.sleep(min(2**attempt, 8))
                continue
            raise TransferError(f"request failed: {method} {url}: {exc}") from exc
    raise AssertionError("retry loop must return or raise")


def get_json(base_url: str, path: str, params: Mapping[str, Any] | None = None) -> Any:
    query = urllib.parse.urlencode(params or {})
    url = f"{base_url.rstrip('/')}{path}" + (f"?{query}" if query else "")
    status, raw = request_bytes("GET", url, accept="application/json")
    if status != 200:
        raise TransferError(f"GET failed status={status} url={url} body={raw[:500]!r}")
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise TransferError(f"non-JSON response from {url}: {raw[:500]!r}") from exc


def get_edn_by_id(base_url: str, kind: str, record_id: str) -> str:
    quoted = urllib.parse.quote(record_id, safe="")
    path = "evidence" if kind == "evidence" else "hyperedge"
    url = f"{base_url.rstrip('/')}/api/alpha/{path}/{quoted}"
    status, raw = request_bytes("GET", url, accept="application/edn")
    if status != 200:
        raise TransferError(
            f"GET-by-id failed kind={kind} id={record_id} status={status} "
            f"body={raw[:500]!r}"
        )
    text = raw.decode("utf-8").strip()
    if "\n" in text or "\r" in text:
        raise TransferError(f"by-id EDN is not one physical line: {kind} {record_id}")
    return text


def maybe_get_json_by_id(base_url: str, kind: str, record_id: str) -> dict[str, Any] | None:
    quoted = urllib.parse.quote(record_id, safe="")
    path = "evidence" if kind == "evidence" else "hyperedge"
    url = f"{base_url.rstrip('/')}/api/alpha/{path}/{quoted}"
    status, raw = request_bytes("GET", url, accept="application/json")
    if status == 404:
        return None
    if status != 200:
        raise TransferError(
            f"GET-by-id preflight failed kind={kind} id={record_id} "
            f"status={status} body={raw[:500]!r}"
        )
    try:
        value = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise TransferError(f"non-JSON by-id response: {kind} {record_id}") from exc
    if not isinstance(value, dict):
        raise TransferError(f"by-id response is not a map: {kind} {record_id}")
    return value


def page_evidence(
    base_url: str,
    *,
    evidence_type: str | None = None,
    tags: Sequence[str] = (),
) -> list[dict[str, Any]]:
    entries: list[dict[str, Any]] = []
    cursor: Mapping[str, str] | None = None
    seen_cursors: set[tuple[str, str]] = set()
    while True:
        params: dict[str, Any] = {"limit": PAGE_LIMIT}
        if evidence_type:
            params["type"] = evidence_type
        if tags:
            params["tags"] = ",".join(tags)
        if cursor:
            params["cursor-at"] = cursor["at"]
            params["cursor-id"] = cursor["id"]
        page = get_json(base_url, "/api/alpha/evidence", params)
        page_entries = page.get("entries")
        if not isinstance(page_entries, list):
            raise TransferError(f"malformed evidence page: {page!r}")
        entries.extend(page_entries)
        next_cursor = page.get("next-cursor")
        if not next_cursor:
            return entries
        key = (str(next_cursor.get("at")), str(next_cursor.get("id")))
        if key in seen_cursors:
            raise TransferError(f"evidence cursor did not advance: {next_cursor!r}")
        seen_cursors.add(key)
        cursor = {"at": key[0], "id": key[1]}


def page_hyperedges(base_url: str, hx_type: str = "memory/assert") -> list[dict[str, Any]]:
    edges: list[dict[str, Any]] = []
    after: str | None = None
    seen: set[str] = set()
    while True:
        params: dict[str, Any] = {"type": hx_type, "limit": PAGE_LIMIT}
        if after:
            params["after"] = after
        page = get_json(base_url, "/api/alpha/hyperedges", params)
        page_edges = page.get("hyperedges")
        if not isinstance(page_edges, list):
            raise TransferError(f"malformed hyperedge page: {page!r}")
        edges.extend(page_edges)
        next_cursor = page.get("next-cursor")
        if not next_cursor:
            return edges
        after = str(next_cursor)
        if after in seen:
            raise TransferError(f"hyperedge cursor did not advance: {after}")
        seen.add(after)


def _skip_ws(text: str, position: int) -> int:
    while position < len(text) and (text[position].isspace() or text[position] == ","):
        position += 1
    return position


def _scan_form(text: str, position: int) -> int:
    position = _skip_ws(text, position)
    if position >= len(text):
        raise TransferError("unexpected end of EDN while scanning a form")
    first = text[position]
    if first == '"':
        position += 1
        escaped = False
        while position < len(text):
            char = text[position]
            position += 1
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                return position
        raise TransferError("unterminated EDN string")
    pairs = {"{": "}", "[": "]", "(": ")"}
    if first in pairs:
        stack = [pairs[first]]
        position += 1
        in_string = False
        escaped = False
        while position < len(text):
            char = text[position]
            position += 1
            if in_string:
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == '"':
                    in_string = False
                continue
            if char == '"':
                in_string = True
            elif char in pairs:
                stack.append(pairs[char])
            elif stack and char == stack[-1]:
                stack.pop()
                if not stack:
                    return position
        raise TransferError("unterminated EDN collection")
    while position < len(text):
        if text[position].isspace() or text[position] in ",{}[]()":
            break
        position += 1
    return position


def edn_map_field(text: str, wanted_key: str) -> str:
    position = _skip_ws(text, 0)
    if position >= len(text) or text[position] != "{":
        raise TransferError("expected an EDN map")
    position += 1
    while True:
        position = _skip_ws(text, position)
        if position >= len(text):
            raise TransferError("unterminated EDN map")
        if text[position] == "}":
            break
        key_start = position
        key_end = _scan_form(text, key_start)
        value_start = _skip_ws(text, key_end)
        value_end = _scan_form(text, value_start)
        if text[key_start:key_end] == wanted_key:
            return text[value_start:value_end]
        position = value_end
    raise TransferError(f"EDN map has no field {wanted_key}")


def canonical_hyperedge_edn(raw: str) -> str:
    fields = (":hx/id", ":hx/type", ":hx/endpoints", ":hx/props")
    return "{" + ", ".join(f"{key} {edn_map_field(raw, key)}" for key in fields) + "}"


def canonical_hyperedge_json(edge: Mapping[str, Any]) -> dict[str, Any]:
    return {
        "hx/id": edge.get("hx/id"),
        "hx/type": edge.get("hx/type"),
        "hx/endpoints": edge.get("hx/endpoints"),
        "hx/props": edge.get("hx/props"),
    }


def edge_roles(edge: Mapping[str, Any]) -> Mapping[str, Any]:
    props = edge.get("hx/props") or {}
    return props.get("roles") or edge.get("prop/roles") or {}


def edge_patterns(edge: Mapping[str, Any]) -> list[str]:
    patterns = edge_roles(edge).get("patterns") or []
    return [str(pattern) for pattern in patterns]


def edge_entry(edge: Mapping[str, Any]) -> str:
    return str(edge_roles(edge).get("entry") or "")


def math_edge(edge: Mapping[str, Any]) -> bool:
    return any(pattern.startswith("math/") for pattern in edge_patterns(edge))


def codexpilot_reviewed(edge: Mapping[str, Any]) -> bool:
    props = edge.get("hx/props") or {}
    review = props.get("review") or {}
    return (
        edge_entry(edge).startswith("e-codexpilot-")
        and math_edge(edge)
        and props.get("attachment-status") == "reviewed"
        and review.get("verdict") == "approve"
    )


def is_open_hunger(entry: Mapping[str, Any]) -> bool:
    body = entry.get("evidence/body") or {}
    level = body.get("requested-memory-level")
    tags = {str(tag) for tag in entry.get("evidence/tags") or []}
    return level == "open-hunger" or "open-hunger" in tags


def review_for_selected_math(
    entry: Mapping[str, Any], selected_memory_ids: set[str]
) -> bool:
    body = entry.get("evidence/body") or {}
    if body.get("review/event") != "memory-attachment-review":
        return False
    memory_id = str(body.get("review/memory-id") or "")
    patterns = [str(pattern) for pattern in body.get("review/pattern-ids") or []]
    return memory_id in selected_memory_ids and any(
        pattern.startswith("math/") for pattern in patterns
    )


def select_corpus(
    memories_and_reviews: Sequence[dict[str, Any]],
    hyperedges: Sequence[dict[str, Any]],
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, str]]]:
    entries_by_id = {
        str(entry.get("evidence/id")): entry
        for entry in memories_and_reviews
        if entry.get("evidence/id")
    }
    reviewed_codexpilot_edges = [edge for edge in hyperedges if codexpilot_reviewed(edge)]
    selected_memory_ids = {
        entry_id
        for entry_id, entry in entries_by_id.items()
        if entry_id.startswith(FAMILY_PREFIXES) or is_open_hunger(entry)
    }
    selected_memory_ids.update(edge_entry(edge) for edge in reviewed_codexpilot_edges)

    skipped: list[dict[str, str]] = []
    for edge in hyperedges:
        entry_id = edge_entry(edge)
        if entry_id.startswith("e-codexpilot-") and math_edge(edge) and not codexpilot_reviewed(edge):
            skipped.append(
                {
                    "id": entry_id,
                    "reason": "codexpilot math attachment is not reviewed+approve",
                }
            )
    missing_ids = sorted(selected_memory_ids - entries_by_id.keys())
    for entry_id in missing_ids:
        skipped.append(
            {"id": entry_id, "reason": "attachment references missing evidence entry"}
        )
    selected_memory_ids.difference_update(missing_ids)

    evidence_entries = [entries_by_id[entry_id] for entry_id in sorted(selected_memory_ids)]
    reviews = [
        entry
        for entry in memories_and_reviews
        if review_for_selected_math(entry, selected_memory_ids)
    ]
    review_ids = {str(entry["evidence/id"]) for entry in reviews}
    evidence_entries.extend(
        entry for entry in sorted(reviews, key=lambda value: str(value["evidence/id"]))
        if str(entry["evidence/id"]) not in selected_memory_ids
    )

    selected_edges = [
        edge
        for edge in hyperedges
        if edge_entry(edge) in selected_memory_ids and math_edge(edge)
    ]
    selected_edges.sort(key=lambda edge: str(edge.get("hx/id")))
    skipped = sorted(
        {item["id"] + "\0" + item["reason"]: item for item in skipped}.values(),
        key=lambda item: (item["id"], item["reason"]),
    )

    if REPRESENTATIVE_ID not in selected_memory_ids:
        raise TransferError(f"representative memory was not selected: {REPRESENTATIVE_ID}")
    if not review_ids:
        raise TransferError("no memory attachment review evidence was selected")
    return evidence_entries, selected_edges, skipped


def envelope(kind: str, record_id: str, payload: str) -> str:
    return (
        f'{{:record/kind :{kind}, :record/id {json.dumps(record_id)}, '
        f":record/payload {payload}}}"
    )


def write_export(local_store: str, export_dir: Path) -> tuple[Path, Path, dict[str, Any]]:
    # All in-scope memories and their lifecycle reviews carry the mathematics
    # tag.  Push this predicate into the bounded query instead of paging the
    # unrelated general memory corpus.
    memories_and_reviews = page_evidence(
        local_store, evidence_type="memory", tags=("mathematics",)
    )
    hyperedges = page_hyperedges(local_store)
    evidence_entries, selected_edges, skipped = select_corpus(
        memories_and_reviews, hyperedges
    )

    records: list[tuple[str, str, str]] = []
    evidence_ids: list[str] = []
    review_ids: list[str] = []
    open_hunger_ids: list[str] = []
    local_json_by_id = {
        str(entry["evidence/id"]): entry for entry in evidence_entries
    }
    evidence_raw = {}
    with ThreadPoolExecutor(max_workers=12) as pool:
        futures = {
            entry_id: pool.submit(get_edn_by_id, local_store, "evidence", entry_id)
            for entry_id in sorted(local_json_by_id)
        }
        evidence_raw = {entry_id: future.result() for entry_id, future in futures.items()}
    for entry in evidence_entries:
        entry_id = str(entry["evidence/id"])
        raw = evidence_raw[entry_id]
        records.append(("evidence", entry_id, raw))
        evidence_ids.append(entry_id)
        if (entry.get("evidence/body") or {}).get("review/event") == "memory-attachment-review":
            review_ids.append(entry_id)
        if is_open_hunger(entry):
            open_hunger_ids.append(entry_id)

    hyperedge_ids: list[str] = []
    edge_ids = [str(edge["hx/id"]) for edge in selected_edges]
    with ThreadPoolExecutor(max_workers=12) as pool:
        futures = {
            edge_id: pool.submit(get_edn_by_id, local_store, "hyperedge", edge_id)
            for edge_id in edge_ids
        }
        edge_raw = {edge_id: future.result() for edge_id, future in futures.items()}
    for edge in selected_edges:
        edge_id = str(edge["hx/id"])
        raw = edge_raw[edge_id]
        records.append(("hyperedge", edge_id, canonical_hyperedge_edn(raw)))
        hyperedge_ids.append(edge_id)

    records.sort(key=lambda record: (record[0] != "evidence", record[1]))
    dump_bytes = ("[\n" + "\n".join(envelope(*record) for record in records) + "\n]\n").encode(
        "utf-8"
    )
    export_dir.mkdir(parents=True, exist_ok=True)
    if len(dump_bytes) > MAX_COMMITTED_DUMP:
        dump_path = Path("/tmp/apm-driver-math-corpus.edn")
        dump_location = "temporary"
    else:
        dump_path = export_dir / "corpus.edn"
        dump_location = "repository"
    dump_path.write_bytes(dump_bytes)

    representative_body = local_json_by_id[REPRESENTATIVE_ID]["evidence/body"]
    tag_probe_ids = sorted(
        entry_id
        for entry_id, entry in local_json_by_id.items()
        if set(TAG_PROBE).issubset(set(entry.get("evidence/tags") or []))
    )
    counts = {
        "memory_entries": len(evidence_ids) - len(review_ids),
        "open_hunger_entries": len(open_hunger_ids),
        "memory_attachment_reviews": len(review_ids),
        "memory_assert_hyperedges": len(hyperedge_ids),
        "evidence_records": len(evidence_ids),
        "total_records": len(records),
    }
    families = Counter(
        next((prefix for prefix in FAMILY_PREFIXES if entry_id.startswith(prefix)), "e-codexpilot-")
        for entry_id in evidence_ids
        if entry_id not in review_ids
    )
    manifest: dict[str, Any] = {
        "schema": "apm-driver.math-corpus-transfer.v1",
        "source_store": local_store,
        "selection": {
            "family_prefixes": list(FAMILY_PREFIXES),
            "codexpilot_rule": "math/* memory/assert attachment with attachment-status=reviewed and review.verdict=approve",
            "open_hunger_included": True,
        },
        "import_idempotence": {
            "mode": "append-only exact-read-before-write",
            "identical_existing": "no-op",
            "conflicting_existing": "fail",
            "reason": "POST /api/alpha/evidence is append-only and returns 409 for duplicate ids",
        },
        "counts": counts,
        "counts_by_family": dict(sorted(families.items())),
        "ids": {
            "evidence": evidence_ids,
            "memory_attachment_reviews": sorted(review_ids),
            "open_hunger": sorted(open_hunger_ids),
            "hyperedges": hyperedge_ids,
        },
        "skipped": skipped,
        "dump": {
            "path": str(dump_path),
            "location": dump_location,
            "bytes": len(dump_bytes),
            "sha256": sha256_bytes(dump_bytes),
        },
        "verification": {
            "representative_id": REPRESENTATIVE_ID,
            "representative_body_sha256": sha256_bytes(
                canonical_json_bytes(representative_body)
            ),
            "tag_query": list(TAG_PROBE),
            "tag_query_expected_ids": tag_probe_ids,
            "review_probe_id": sorted(review_ids)[0],
        },
        "record_sha256": {
            "evidence": {
                entry_id: sha256_bytes(canonical_json_bytes(local_json_by_id[entry_id]))
                for entry_id in sorted(local_json_by_id)
            },
            "hyperedge": {
                str(edge["hx/id"]): sha256_bytes(
                    canonical_json_bytes(canonical_hyperedge_json(edge))
                )
                for edge in selected_edges
            },
        },
    }
    manifest_path = export_dir / "manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return dump_path, manifest_path, manifest


def read_dump(path: Path) -> list[tuple[str, str, str]]:
    raw = path.read_bytes()
    lines = raw.decode("utf-8").splitlines()
    if len(lines) < 2 or lines[0] != "[" or lines[-1] != "]":
        raise TransferError("dump is not the expected line-oriented EDN vector")
    records: list[tuple[str, str, str]] = []
    for line_number, line in enumerate(lines[1:-1], start=2):
        if not line.strip():
            continue
        try:
            kind = edn_map_field(line, ":record/kind").removeprefix(":")
            record_id = json.loads(edn_map_field(line, ":record/id"))
            payload = edn_map_field(line, ":record/payload")
        except (TransferError, json.JSONDecodeError) as exc:
            raise TransferError(f"invalid dump record at line {line_number}: {exc}") from exc
        if kind not in {"evidence", "hyperedge"} or not isinstance(record_id, str):
            raise TransferError(f"invalid dump envelope at line {line_number}")
        records.append((kind, record_id, payload))
    return records


def assert_remote_import_target(store_url: str, allow_local_write: bool = False) -> None:
    parsed = urllib.parse.urlsplit(store_url)
    if parsed.hostname not in {"127.0.0.1", "localhost", "::1"}:
        raise TransferError("import target must be loopback-only")
    if parsed.port == 7073 and not allow_local_write:
        raise TransferError(
            "refusing to POST to the local futon1b port 7073 "
            "(pass --allow-local-write for an explicit reverse mirror)")


def import_dump(store_url: str, dump_path: Path, manifest_path: Path,
                allow_local_write: bool = False) -> None:
    assert_remote_import_target(store_url, allow_local_write)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    raw = dump_path.read_bytes()
    if sha256_bytes(raw) != manifest["dump"]["sha256"]:
        raise TransferError("dump SHA-256 does not match manifest")
    records = read_dump(dump_path)
    if len(records) != manifest["counts"]["total_records"]:
        raise TransferError("dump record count does not match manifest")
    imported = Counter()
    identical = Counter()
    for kind, record_id, payload in records:
        existing = maybe_get_json_by_id(store_url, kind, record_id)
        if existing is not None:
            comparable = canonical_hyperedge_json(existing) if kind == "hyperedge" else existing
            actual_hash = sha256_bytes(canonical_json_bytes(comparable))
            expected_hash = manifest["record_sha256"][kind][record_id]
            if actual_hash != expected_hash:
                raise TransferError(
                    f"conflicting existing record kind={kind} id={record_id}; "
                    f"expected-sha256={expected_hash} actual-sha256={actual_hash}; "
                    "append-only evidence cannot be replaced"
                )
            identical[kind] += 1
            continue
        endpoint = "evidence" if kind == "evidence" else "hyperedge"
        url = f"{store_url.rstrip('/')}/api/alpha/{endpoint}"
        status, response = request_bytes(
            "POST",
            url,
            accept="application/edn",
            content_type="application/edn",
            body=payload.encode("utf-8"),
        )
        if status < 200 or status >= 300:
            raise TransferError(
                f"import failed kind={kind} id={record_id} status={status} "
                f"body={response[:500]!r}"
            )
        imported[kind] += 1
    print(
        "IMPORT "
        f"written-evidence={imported['evidence']} "
        f"written-hyperedges={imported['hyperedge']} "
        f"identical-evidence={identical['evidence']} "
        f"identical-hyperedges={identical['hyperedge']} "
        f"dump-sha256={manifest['dump']['sha256']} OK"
    )


def verify_remote(store_url: str, manifest_path: Path) -> None:
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    expected = manifest["counts"]
    remote_evidence = page_evidence(store_url, evidence_type="memory")
    remote_edges = page_hyperedges(store_url)
    actual_evidence = len(remote_evidence)
    actual_edges = len(remote_edges)
    print(
        f"COUNT-PARITY evidence expected={expected['evidence_records']} "
        f"actual={actual_evidence} {'OK' if actual_evidence == expected['evidence_records'] else 'FAIL'}"
    )
    print(
        f"COUNT-PARITY hyperedges expected={expected['memory_assert_hyperedges']} "
        f"actual={actual_edges} {'OK' if actual_edges == expected['memory_assert_hyperedges'] else 'FAIL'}"
    )
    if actual_evidence != expected["evidence_records"] or actual_edges != expected["memory_assert_hyperedges"]:
        raise TransferError("remote count parity failed")

    verification = manifest["verification"]
    representative_id = verification["representative_id"]
    representative = get_json(
        store_url,
        f"/api/alpha/evidence/{urllib.parse.quote(representative_id, safe='')}",
    )
    body_hash = sha256_bytes(canonical_json_bytes(representative["evidence/body"]))
    expected_hash = verification["representative_body_sha256"]
    print(
        f"BODY-BYTE-COMPARE id={representative_id} local={expected_hash} "
        f"remote={body_hash} {'OK' if body_hash == expected_hash else 'FAIL'}"
    )
    if body_hash != expected_hash:
        raise TransferError("representative body byte comparison failed")

    tags = verification["tag_query"]
    tag_entries = page_evidence(store_url, evidence_type="memory", tags=tags)
    found_ids = sorted(str(entry["evidence/id"]) for entry in tag_entries)
    expected_ids = verification["tag_query_expected_ids"]
    print(
        f"TAG-QUERY tags={','.join(tags)} expected={json.dumps(expected_ids)} "
        f"found={json.dumps(found_ids)} {'OK' if found_ids == expected_ids else 'FAIL'}"
    )
    if found_ids != expected_ids:
        raise TransferError("tag retrieval verification failed")

    review_id = verification["review_probe_id"]
    review = get_json(
        store_url, f"/api/alpha/evidence/{urllib.parse.quote(review_id, safe='')}"
    )
    readable = review.get("evidence/id") == review_id
    print(f"REVIEW-READ id={review_id} {'OK' if readable else 'FAIL'}")
    if not readable:
        raise TransferError("memory attachment review read-back failed")


def run_checked(command: Sequence[str], *, capture: bool = False) -> str:
    result = subprocess.run(
        list(command),
        check=False,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )
    if result.returncode != 0:
        output = result.stdout or ""
        raise TransferError(
            f"command failed exit={result.returncode}: {shlex.join(command)}\n{output}"
        )
    return result.stdout or ""


def transfer(
    local_store: str,
    remote_store: str,
    ssh_host: str,
    export_dir: Path,
) -> dict[str, Any]:
    dump_path, manifest_path, manifest = write_export(local_store, export_dir)
    token = manifest["dump"]["sha256"][:16]
    remote_root = f"/tmp/apm-corpus-transfer-{token}"
    run_checked(["ssh", ssh_host, "mkdir", "-p", remote_root])
    run_checked(
        [
            "scp",
            str(Path(__file__).resolve()),
            str(dump_path),
            str(manifest_path),
            f"{ssh_host}:{remote_root}/",
        ]
    )
    remote_script = f"{remote_root}/{Path(__file__).name}"
    remote_dump = f"{remote_root}/{dump_path.name}"
    remote_manifest = f"{remote_root}/{manifest_path.name}"
    import_output = run_checked(
        [
            "ssh",
            ssh_host,
            "python3",
            remote_script,
            "import",
            "--store",
            remote_store,
            "--dump",
            remote_dump,
            "--manifest",
            remote_manifest,
        ],
        capture=True,
    )
    verify_output = run_checked(
        [
            "ssh",
            ssh_host,
            "python3",
            remote_script,
            "verify",
            "--store",
            remote_store,
            "--manifest",
            remote_manifest,
        ],
        capture=True,
    )
    print(import_output, end="")
    print(verify_output, end="")
    return manifest


def self_test() -> None:
    sample = (
        '{:hx/id "hx-1", :hx/type :memory/assert, '
        ':hx/endpoints ["e-1" "math/x"], '
        ':hx/props {:roles {:entry "e-1"} :hook "line\\nwith } brace"}, '
        ':prop/hook "derived"}'
    )
    canonical = canonical_hyperedge_edn(sample)
    assert edn_map_field(canonical, ":hx/id") == '"hx-1"'
    assert ":prop/hook" not in canonical
    wrapped = envelope("hyperedge", "hx-1", canonical)
    assert edn_map_field(wrapped, ":record/kind") == ":hyperedge"
    assert json.loads(edn_map_field(wrapped, ":record/id")) == "hx-1"
    assert edn_map_field(wrapped, ":record/payload") == canonical
    print("SELF-TEST EDN scanner/envelope OK")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    export_parser = subparsers.add_parser("export")
    export_parser.add_argument("--store", default=DEFAULT_LOCAL_STORE)
    export_parser.add_argument("--export-dir", type=Path, default=DEFAULT_EXPORT_DIR)

    import_parser = subparsers.add_parser("import")
    import_parser.add_argument("--store", default=DEFAULT_REMOTE_STORE)
    import_parser.add_argument("--dump", type=Path, required=True)
    import_parser.add_argument("--manifest", type=Path, required=True)
    import_parser.add_argument("--allow-local-write", action="store_true")

    verify_parser = subparsers.add_parser("verify")
    verify_parser.add_argument("--store", default=DEFAULT_REMOTE_STORE)
    verify_parser.add_argument("--manifest", type=Path, required=True)

    transfer_parser = subparsers.add_parser("transfer")
    transfer_parser.add_argument("--local-store", default=DEFAULT_LOCAL_STORE)
    transfer_parser.add_argument("--remote-store", default=DEFAULT_REMOTE_STORE)
    transfer_parser.add_argument("--ssh-host", default=DEFAULT_SSH_HOST)
    transfer_parser.add_argument("--export-dir", type=Path, default=DEFAULT_EXPORT_DIR)

    subparsers.add_parser("self-test")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if args.command == "export":
        dump_path, manifest_path, manifest = write_export(args.store, args.export_dir)
        print(
            f"EXPORT dump={dump_path} manifest={manifest_path} "
            f"counts={json.dumps(manifest['counts'], sort_keys=True)}"
        )
    elif args.command == "import":
        import_dump(args.store, args.dump, args.manifest,
                    allow_local_write=args.allow_local_write)
    elif args.command == "verify":
        verify_remote(args.store, args.manifest)
    elif args.command == "transfer":
        transfer(args.local_store, args.remote_store, args.ssh_host, args.export_dir)
    elif args.command == "self-test":
        self_test()
    else:
        raise AssertionError(f"unhandled command: {args.command}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except TransferError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
