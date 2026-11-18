"""Read legacy or content-addressed Giraffe EDN run files."""
from pathlib import Path
import hashlib

SCHEMA = ['giraffe/run-index', 1]
REF = 'giraffe/object-ref'

def load_run_file(path, edn_load):
    path = Path(path)
    value = edn_load(path.read_text(encoding='utf-8'))
    if not isinstance(value, dict) or value.get('schema') != SCHEMA:
        return value
    root, cache = path.parent.parent, {}
    def resolve(x):
        if isinstance(x, dict) and set(x) == {REF}:
            sha = x[REF]
            if sha not in cache:
                raw = (root / 'objects' / sha[:2] / (sha + '.edn')).read_bytes()
                if hashlib.sha256(raw).hexdigest() != sha:
                    raise ValueError('Giraffe object digest mismatch: ' + sha)
                cache[sha] = edn_load(raw.decode('utf-8'))
            return resolve(cache[sha])
        if isinstance(x, dict): return {k: resolve(v) for k, v in x.items()}
        if isinstance(x, list): return [resolve(v) for v in x]
        return x
    logical = resolve(value['content'])
    if value.get('kind') == 'pass':
        for key in ('readiness', 'cones', 'unblocks'):
            if isinstance(logical.get(key), list): logical[key] = dict(logical[key])
    return logical
