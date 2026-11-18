"""Read legacy or content-addressed Giraffe EDN run files."""
from pathlib import Path
import hashlib
import sqlite3
from contextlib import closing, nullcontext

SCHEMA = ['giraffe/run-index', 1]
REF = 'giraffe/object-ref'

def read_object_bytes(root, sha, suffix='.edn', connection=None):
    """SQLite is authoritative when present; legacy fallback only without a DB."""
    root = Path(root)
    db = root / 'objects.sqlite'
    if connection is not None or db.is_file():
        context = (nullcontext(connection) if connection is not None else
                   closing(sqlite3.connect(db.resolve().as_uri() + '?mode=ro',
                                           uri=True, timeout=30)))
        with context as conn:
            row = conn.execute('SELECT body FROM objects WHERE sha=?', (sha,)).fetchone()
        if row is None:
            raise FileNotFoundError('Missing SQLite object: ' + sha)
        return bytes(row[0])
    return (root / 'objects' / sha[:2] / (sha + suffix)).read_bytes()


def load_run_file(path, edn_load):
    path = Path(path)
    value = edn_load(path.read_text(encoding='utf-8'))
    if not isinstance(value, dict) or value.get('schema') != SCHEMA:
        return value
    root, cache = path.parent.parent, {}
    connection = None
    def resolve(x):
        if isinstance(x, dict) and set(x) == {REF}:
            sha = x[REF]
            if sha not in cache:
                raw = read_object_bytes(root, sha, connection=connection)
                if hashlib.sha256(raw).hexdigest() != sha:
                    raise ValueError('Giraffe object digest mismatch: ' + sha)
                cache[sha] = edn_load(raw.decode('utf-8'))
            return resolve(cache[sha])
        if isinstance(x, dict): return {k: resolve(v) for k, v in x.items()}
        if isinstance(x, list): return [resolve(v) for v in x]
        return x
    db = root / 'objects.sqlite'
    context = (closing(sqlite3.connect(db.resolve().as_uri() + '?mode=ro',
                                      uri=True, timeout=30))
               if db.is_file() else nullcontext(None))
    with context as connection:
        logical = resolve(value['content'])
    if value.get('kind') == 'pass':
        for key in ('readiness', 'cones', 'unblocks'):
            if isinstance(logical.get(key), list): logical[key] = dict(logical[key])
    return logical
