"""Read-only diagnostic of the stopped-frame display, not a repair test.

Run from this directory: python3 display_probe.py > display-observed.json
The job feed is deliberately supplied as readable and empty. All remaining
APM projection inputs are the real stopped campaign files; network helpers
are disabled. Hashes identify the exact display source used.
"""
import hashlib
import json
import pathlib
import sys
from unittest.mock import patch

vox = pathlib.Path('/home/joe/code/voxterm')
sys.path.insert(0, str(vox))
import server

with patch.object(server, '_apm_running_jobs', return_value=[]), \
     patch.object(server, '_jvm_health', return_value=None), \
     patch.object(server, '_substrate_permits', return_value=None), \
     patch.object(server, 'urlopen', side_effect=AssertionError('network forbidden')):
    status = server.apm_status()

out = {
    'diagnostic': 'readable empty job feed against deliberately stopped campaign',
    'source_sha256': {name: hashlib.sha256((vox / name).read_bytes()).hexdigest()
                      for name in ['server.py', 'index.html']},
    'observed': {key: status.get(key) for key in
                 ['campaign', 'frame', 'phase', 'state', 'alert', 'timeline']},
    'limitation': 'Reproduces the projection; not a browser screenshot or live-feed observation.'
}
print(json.dumps(out, indent=2))
