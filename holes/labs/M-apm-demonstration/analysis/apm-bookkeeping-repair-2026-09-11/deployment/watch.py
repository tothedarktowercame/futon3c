"""Bounded reload observer: retry only a confirmed no-op waiting result."""
import argparse
import datetime
import json
from pathlib import Path
import subprocess
import time

root = Path('/home/joe/code/futon3c')
here = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--attempts', type=int, default=180)
args = parser.parse_args()
with (here / 'reload-observations.jsonl').open('a') as log:
    for attempt in range(args.attempts):
        try:
            result = subprocess.run(
                [str(root / 'scripts/proof-eval.sh'), '-f', str(here / 'reload.clj')],
                cwd=root, text=True, capture_output=True, timeout=20)
        except subprocess.TimeoutExpired:
            log.write(json.dumps({'status': 'unknown-timeout',
                                  'at': datetime.datetime.now(datetime.timezone.utc).isoformat()}) + '\n')
            log.flush()
            raise SystemExit('Timed out; inspect applied.edn before retrying')
        log.write(json.dumps({'at': datetime.datetime.now(datetime.timezone.utc).isoformat(),
                              'exit': result.returncode, 'result': result.stdout,
                              'stderr': result.stderr}) + '\n')
        log.flush()
        if result.returncode or ':ok false' in result.stdout or ':status :blocked' in result.stdout:
            print(result.stdout, result.stderr)
            raise SystemExit(2)
        if ':status :loaded' in result.stdout or ':status :already-recorded' in result.stdout:
            print(result.stdout)
            break
        if ':status :waiting' not in result.stdout:
            raise SystemExit('Unexpected result; no retry')
        time.sleep(1)
    else:
        raise SystemExit('No safe tick gap observed; no reload confirmed')
