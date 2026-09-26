#!/usr/bin/env python3
"""Read an Agency job trace with identity checking and bounded HTTP diagnostics."""
import argparse
import json
import urllib.error
import urllib.parse
import urllib.request


def read_job(base, job_id):
    url = base.rstrip('/') + '/api/alpha/invoke/jobs/' + urllib.parse.quote(job_id, safe='')
    try:
        with urllib.request.urlopen(url, timeout=30) as response:
            body = json.load(response)
    except urllib.error.HTTPError as error:
        return {'ok': False, 'url': url, 'status': error.code,
                'error': error.read(1024).decode(errors='replace')}
    except (OSError, ValueError) as error:
        return {'ok': False, 'url': url, 'error': str(error)}
    if (not isinstance(body, dict) or body.get('ok') is not True
            or not isinstance(body.get('job'), dict)
            or body['job'].get('job-id') != job_id):
        return {'ok': False, 'url': url, 'error': 'job-response-identity-invalid'}
    return body


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--job-id', required=True)
    parser.add_argument('--agency-base', default='http://localhost:7070')
    args = parser.parse_args()
    result = read_job(args.agency_base, args.job_id)
    print(json.dumps(result, indent=2))
    return 0 if result.get('ok') else 2


if __name__ == '__main__':
    raise SystemExit(main())
