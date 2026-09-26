"""Run actual af3b3b26 record creation and current-code replay in a private JVM."""
import os
from pathlib import Path
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[5]
with tempfile.TemporaryDirectory(prefix='apm-v4-old-pins-') as directory:
    for name, source in [('typed.clj', 'typed_role_submission.clj'),
                         ('review.clj', 'pattern_revision_review.clj')]:
        historical = subprocess.check_output(
            ['git', 'show', 'af3b3b26:src/futon3c/apm/' + source], cwd=REPO)
        (Path(directory) / name).write_bytes(historical)
    environment = dict(os.environ, APM_V4_REPLAY_OLD_ROOT=directory)
    subprocess.run(['clojure', '-Sdeps', '{:paths ["src" "test" "resources"]}',
                    '-M', str(HERE / 'check.clj')],
                   cwd=REPO, env=environment, check=True, timeout=120)
