#!/usr/bin/env python3
"""Submit a typed observational payload to one canonical APM role job."""

import argparse
import json
import pathlib
import sys
import urllib.error
import urllib.parse
import urllib.request


def submission_template(schema: dict) -> dict:
    """Materialize the server-declared evidence shape; nulls are leaves."""
    evidence = schema.get("evidence-shape")
    if evidence is None:
        raise ValueError("submission schema omitted evidence-shape")
    return {"command-own-exit": None, "outcome": None,
            "failure-account": [], "evidence": evidence}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--job-id", required=True)
    parser.add_argument("--token", required=True)
    parser.add_argument("--payload", required=True, type=pathlib.Path)
    parser.add_argument("--init", action="store_true")
    parser.add_argument("--agency-base", default="http://localhost:7070")
    args = parser.parse_args()

    endpoint = f"{args.agency_base}/api/alpha/invoke/jobs/{args.job_id}/submission"
    if args.init:
        query = urllib.parse.urlencode({"token": args.token})
        with urllib.request.urlopen(f"{endpoint}?{query}") as response:
            schema = json.load(response)
        template = submission_template(schema)
        args.payload.write_text(json.dumps(template, indent=2, sort_keys=True) + "\n",
                                encoding="utf-8")
        print(f"Wrote {args.payload}; fill every null value before submitting.")
        return 0

    payload = json.loads(args.payload.read_text(encoding="utf-8"))
    body = json.dumps({"token": args.token, "payload": payload}).encode()
    request = urllib.request.Request(
        endpoint,
        data=body,
        headers={"content-type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(request) as response:
            result = json.load(response)
    except urllib.error.HTTPError as error:
        result = json.loads(error.read().decode())
        print(json.dumps(result, indent=2, sort_keys=True), file=sys.stderr)
        return 2
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0 if result.get("ok") else 2


if __name__ == "__main__":
    raise SystemExit(main())
