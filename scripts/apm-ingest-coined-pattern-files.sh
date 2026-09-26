#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

clojure -Sdeps '{:paths ["src" "resources" "library" "dev"]}' -M -e '
(require (quote futon3c.apm.coined-pattern))
(doseq [[path coiner]
        [["holes/labs/M-apm-demonstration/pattern-library-zai-scribe-f34-a95J03.md"
          "f34-zai-scribe"]
         ["holes/labs/M-apm-demonstration/pattern-library-codex-scribe-f35-a95J04.md"
          "f35-codex-scribe"]]]
  (prn (assoc (futon3c.apm.coined-pattern/publish-file! path coiner)
              :path path)))'
