# Evidence corpus mesh replication

This is a file-layer companion to `futon0/README-federate.md`, not a
replacement for memory federation. It is **replication, not backup**: it will
faithfully reproduce any bytes named by a committed manifest, including bad
bytes if a bad hash is committed.

`resources/evidence-corpora.tsv` registers corpora. Only `sha256-v1` is safe to
replicate. The APM manifest is deliberately registered as `apm-no-sha-v1`:
its 14,881 data rows contain problem id, source path, size, mtime, and kind but
no content hash. The command refuses to sync it because neither source
selection nor final verification can satisfy F-4. Upgrading that manifest with
a stable relative path and SHA-256 is a prerequisite to replicating the 1.3 GB
corpus.

Sites are named `ams`, `lon`, `chi`, and `oxf`. SSH aliases are transport
configuration in `resources/evidence-sites.tsv`; they are not durable site
identities. No laptop hostname is a source identity. A node pulls from peers
in registry order and accepts the first candidate whose SHA-256 matches the
manifest. A corrupt local file is reported and moved into the receipt
quarantine before replacement, never silently overwritten.

## Commands

```sh
FUTON3C_SITE=oxf scripts/evidence_mesh_sync.sh verify futon3c-evidence-20260801
FUTON3C_SITE=oxf scripts/evidence_mesh_sync.sh sync futon3c-evidence-20260801
FUTON3C_SITE=oxf scripts/evidence_mesh_sync.sh status futon3c-evidence-20260801
```

Receipts and quarantined mismatches live below
`~/.local/state/futon3c/evidence-replication/`, outside the repository. A
missing latest-receipt marker is printed as `NO_RECEIPT`; silence is not
success. `status` prints the replication factor directly and fails unless at
least two sites verify.

Install the user timer:

```sh
printf 'FUTON3C_SITE=oxf\n' > ~/.config/futon3c/evidence-mesh.env
install -Dm644 scripts/systemd/units/futon3c-evidence-mesh.service \
  ~/.config/systemd/user/futon3c-evidence-mesh.service
install -Dm644 scripts/systemd/units/futon3c-evidence-mesh.timer \
  ~/.config/systemd/user/futon3c-evidence-mesh.timer
systemctl --user daemon-reload
systemctl --user enable --now futon3c-evidence-mesh.timer
```
