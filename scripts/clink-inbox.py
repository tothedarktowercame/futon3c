#!/usr/bin/env python3
"""List, read, and acknowledge pull-only Agency inbox deliveries."""

import argparse
import datetime as dt
import json
import os
from pathlib import Path
import sys
import urllib.error
import urllib.request


def inbox_root():
    return Path(os.environ.get(
        "FUTON3C_AGENCY_INBOX_DIR",
        str(Path.home() / ".claude" / "agency-inbox"),
    ))


def agent_directory(agent_id):
    if not agent_id or "/" in agent_id or ".." in agent_id:
        raise ValueError("agent id must be a safe path segment")
    return inbox_root() / agent_id


def delivery_path(agent_id, job_id):
    if not job_id or "/" in job_id or ".." in job_id:
        raise ValueError("job id must be a safe path segment")
    return agent_directory(agent_id) / f"{job_id}.json"


def load_payload(path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def age_ms(created_at):
    created = dt.datetime.fromisoformat(created_at.replace("Z", "+00:00"))
    now = dt.datetime.now(dt.timezone.utc)
    return max(0, int((now - created).total_seconds() * 1000))


def list_deliveries(args):
    directory = agent_directory(args.agent)
    for path in sorted(directory.glob("*.json")):
        payload = load_payload(path)
        first_line = str(payload.get("prompt", "")).splitlines()[0] if payload.get("prompt") else ""
        print(f"{payload.get('job-id', path.stem)}\t{payload.get('from', '')}\t"
              f"{age_ms(payload['created-at'])}ms\t{first_line}")


def read_delivery(args):
    payload = load_payload(delivery_path(args.agent, args.job_id))
    print(json.dumps(payload, indent=2, ensure_ascii=False))


def ack_delivery(args):
    payload = load_payload(delivery_path(args.agent, args.job_id))
    ack_url = payload.get("ack-url")
    if not isinstance(ack_url, str) or not ack_url.startswith("/"):
        raise ValueError("payload has no usable ack-url")
    body = json.dumps({"note": args.note} if args.note is not None else {}).encode("utf-8")
    request = urllib.request.Request(
        args.base_url.rstrip("/") + ack_url,
        data=body,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(request) as response:
        print(response.read().decode("utf-8"))


def parser():
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--agent", required=True)
    result.add_argument(
        "--base-url",
        default=os.environ.get("FUTON3C_AGENCY_URL", "http://127.0.0.1:7070"),
    )
    commands = result.add_subparsers(dest="command", required=True)
    commands.add_parser("list").set_defaults(action=list_deliveries)
    read = commands.add_parser("read")
    read.add_argument("job_id")
    read.set_defaults(action=read_delivery)
    ack = commands.add_parser("ack")
    ack.add_argument("job_id")
    ack.add_argument("--note")
    ack.set_defaults(action=ack_delivery)
    return result


def main():
    args = parser().parse_args()
    try:
        args.action(args)
    except (OSError, ValueError, KeyError, json.JSONDecodeError,
            urllib.error.URLError) as error:
        print(f"clink-inbox: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
