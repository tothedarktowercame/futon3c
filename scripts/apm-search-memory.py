#!/usr/bin/env python3
"""Run one authenticated APM FTS query and print its persisted receipt."""

import argparse
import json
import urllib.error
import urllib.request


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--job-id", required=True)
    parser.add_argument("--token", required=True)
    parser.add_argument("--query", required=True)
    parser.add_argument("--limit", type=int, default=10)
    parser.add_argument("--agency-base", default="http://localhost:7070")
    args = parser.parse_args()
    endpoint = (f"{args.agency_base}/api/alpha/invoke/jobs/"
                f"{args.job_id}/memory-search")
    body = json.dumps({"token": args.token, "query": args.query,
                       "limit": args.limit}).encode()
    request = urllib.request.Request(
        endpoint, data=body, headers={"content-type": "application/json"},
        method="POST")
    try:
        with urllib.request.urlopen(request) as response:
            result = json.load(response)
    except urllib.error.HTTPError as error:
        result = json.loads(error.read().decode())
        print(json.dumps(result, indent=2, sort_keys=True))
        return 2
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0 if result.get("ok") else 2


if __name__ == "__main__":
    raise SystemExit(main())
