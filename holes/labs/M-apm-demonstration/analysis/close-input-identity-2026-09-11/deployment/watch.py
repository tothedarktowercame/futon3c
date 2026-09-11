"""Bounded, one-shot canonical reload at a V3 frame boundary; no loop restart."""
import datetime,json,pathlib,subprocess,time
HERE=pathlib.Path(__file__).resolve().parent
# Resolve the canonical repository explicitly: do not depend on service cwd.
ROOT=pathlib.Path('/home/joe/code/futon3c')
stop=time.monotonic()+6*3600
with (HERE/'watch.jsonl').open('a') as log:
    while time.monotonic()<stop:
        try:
            r=subprocess.run([str(ROOT/'scripts/proof-eval.sh'),'-f',str(HERE/'at-boundary.clj')],cwd=ROOT,text=True,capture_output=True,timeout=20)
        except subprocess.TimeoutExpired:
            log.write(json.dumps({'at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'status':'observer-timeout','note':'No automatic retry; inspect applied.edn before further action.'})+'\n');log.flush()
            raise SystemExit(2)
        log.write(json.dumps({'at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'exit':r.returncode,'result':r.stdout,'stderr':r.stderr})+'\n');log.flush()
        if r.returncode or ':status :blocked' in r.stdout or ':ok false' in r.stdout:
            raise SystemExit(2)
        if ':status :loaded' in r.stdout or ':status :already-recorded' in r.stdout:
            raise SystemExit(0)
        if ':status :waiting' not in r.stdout:
            raise SystemExit('Unrecognized result; inspect before retry')
        time.sleep(10)
    log.write(json.dumps({'status':'expired','note':'No boundary reload confirmed within six hours.'})+'\n');log.flush()
    raise SystemExit(3)
