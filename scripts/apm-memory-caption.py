#!/usr/bin/env python3
"""Submit an authenticated applicability observation or caption operation."""

import argparse
import json
import urllib.error
import urllib.request


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("operation", choices=["observe", "propose", "review"])
    parser.add_argument("--job-id", required=True)
    parser.add_argument("--token", required=True)
    parser.add_argument("--payload", required=True)
    parser.add_argument("--agency-base", default="http://localhost:7070")
    args = parser.parse_args()
    suffix, field = {
        "observe": ("memory-applicability", "observation"),
        "propose": ("memory-caption", "caption"),
        "review": ("memory-caption-review", "review"),
    }[args.operation]
    with open(args.payload, encoding="utf-8") as source:
        value = json.load(source)
    body = json.dumps({"token": args.token, field: value}).encode()
    endpoint = (f"{args.agency_base}/api/alpha/invoke/jobs/"
                f"{args.job_id}/{suffix}")
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
