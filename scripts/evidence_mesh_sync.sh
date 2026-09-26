#!/usr/bin/env bash
# Pull manifest-addressed evidence from any verifying peer.
#
# This is replication, not backup: it copies the current manifest-addressed
# bytes, including good and bad writes already blessed by that manifest.
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
CORPORA=${FUTON_EVIDENCE_CORPORA:-$REPO_ROOT/resources/evidence-corpora.tsv}
SITES=${FUTON_EVIDENCE_SITES:-$REPO_ROOT/resources/evidence-sites.tsv}
LOCAL_SITE=${FUTON3C_SITE:-}
STATE_ROOT=${XDG_STATE_HOME:-$HOME/.local/state}/futon3c/evidence-replication

usage() {
  cat <<'EOF'
usage:
  evidence_mesh_sync.sh sync <corpus-id> [--dest-root DIR] [--receipt-dir DIR] [--peer SITE ...]
  evidence_mesh_sync.sh verify <corpus-id> [--dest-root DIR]
  evidence_mesh_sync.sh status <corpus-id>
  evidence_mesh_sync.sh sync-all

FUTON3C_SITE must be one of the canonical site names ams/lon/chi/oxf.
Peer transport aliases are configured separately in resources/evidence-sites.tsv.
EOF
}

die() { echo "ERROR: $*" >&2; exit 2; }

expand_home() {
  local home_token=\$HOME/
  if [[ "$1" == "$home_token"* ]]; then
    printf '%s/%s\n' "$HOME" "${1#"$home_token"}"
  else
    printf '%s\n' "$1"
  fi
}

corpus_record() {
  local wanted=$1
  awk -F '\t' -v wanted="$wanted" '$1 == wanted {print; found=1} END {exit !found}' "$CORPORA"
}

site_record() {
  local wanted=$1
  awk -F '\t' -v wanted="$wanted" '$1 == wanted {print; found=1} END {exit !found}' "$SITES"
}

peer_target() {
  local site=$1 rec env_name default_target configured
  rec=$(site_record "$site") || return 1
  IFS=$'\t' read -r _ env_name default_target _ <<< "$rec"
  configured=${!env_name:-}
  if [[ -n "$configured" ]]; then
    printf '%s\n' "$configured"
  elif [[ "$default_target" != "-" ]]; then
    printf '%s\n' "$default_target"
  else
    return 1
  fi
}

manifest_path() {
  local raw=$1
  raw=$(expand_home "$raw")
  if [[ "$raw" = /* ]]; then printf '%s\n' "$raw"; else printf '%s/%s\n' "$REPO_ROOT" "$raw"; fi
}

load_corpus() {
  local id=$1 rec raw_manifest
  rec=$(corpus_record "$id") || die "unknown corpus: $id"
  IFS=$'\t' read -r CORPUS_ID CORPUS_SCHEMA raw_manifest CORPUS_DATA_ROOT <<< "$rec"
  MANIFEST=$(manifest_path "$raw_manifest")
  CORPUS_DATA_ROOT=$(expand_home "$CORPUS_DATA_ROOT")
  if [[ "$CORPUS_SCHEMA" != sha256-v1 ]]; then
    die "$CORPUS_ID uses schema $CORPUS_SCHEMA: no sha256 column; safe mesh replication is unsupported"
  fi
  [[ -f "$MANIFEST" ]] || die "manifest not found: $MANIFEST"
  if [[ "$CORPUS_DATA_ROOT" != /* ]] &&
     awk -F '\t' -v root="$CORPUS_DATA_ROOT" \
       '$1 !~ /^#/ && $1 != "path" && index($1, root) != 1 {bad=1} END {exit bad}' "$MANIFEST"; then
    :
  elif [[ "$CORPUS_DATA_ROOT" != /* ]]; then
    die "$CORPUS_ID manifest contains a path outside registered data root $CORPUS_DATA_ROOT"
  fi
}

manifest_rows() {
  awk -F '\t' '$1 !~ /^#/ && $1 != "path" && NF >= 3 {print $1 "\t" $2 "\t" $3}' "$MANIFEST"
}

resolve_local_path() {
  local root=$1 path=$2
  if [[ "$root" = /* ]]; then
    case "$path" in
      data/evidence/*) printf '%s/%s\n' "${root%/data/evidence/}" "$path" ;;
      *) printf '%s/%s\n' "${root%/}" "$path" ;;
    esac
  else
    printf '%s/%s\n' "$REPO_ROOT" "$path"
  fi
}

verify_tree() {
  local root=$1 path bytes sha file got failures=0 verified=0 missing=0 mismatched=0
  while IFS=$'\t' read -r path bytes sha; do
    file=$(resolve_local_path "$root" "$path")
    if [[ ! -f "$file" ]]; then
      echo "MISSING $path" >&2
      missing=$((missing + 1)); failures=1; continue
    fi
    got=$(sha256sum "$file" | awk '{print $1}')
    if [[ "$got" != "$sha" ]]; then
      echo "HASH_MISMATCH $path expected=$sha got=$got" >&2
      mismatched=$((mismatched + 1)); failures=1; continue
    fi
    verified=$((verified + 1))
  done < <(manifest_rows)
  echo "verified=$verified missing=$missing mismatched=$mismatched"
  return "$failures"
}

remote_has_verified_tree() {
  local site=$1 rec target remote_root checksum_file output
  rec=$(site_record "$site") || return 1
  IFS=$'\t' read -r _ _ _ remote_root <<< "$rec"
  if [[ "$site" == "$LOCAL_SITE" ]]; then
    verify_tree "$remote_root" >/dev/null 2>&1
    return
  fi
  target=$(peer_target "$site") || return 1
  checksum_file=$(mktemp)
  manifest_rows | awk -F '\t' '{print $3 "  " $1}' > "$checksum_file"
  output=$(ssh -o BatchMode=yes -o ConnectTimeout=5 "$target" \
    "cd '$remote_root' && sha256sum -c -" < "$checksum_file" 2>&1) || {
      rm -f "$checksum_file"
      return 1
    }
  rm -f "$checksum_file"
  [[ $(grep -c ': OK$' <<< "$output") -eq $(manifest_rows | wc -l) ]]
}

site_has_receipt() {
  local site=$1 target receipt="$STATE_ROOT/receipts/${CORPUS_ID}-latest.edn"
  if [[ "$site" == "$LOCAL_SITE" ]]; then
    [[ -e "$receipt" || -L "$receipt" ]]
    return
  fi
  target=$(peer_target "$site") || return 1
  ssh -o BatchMode=yes -o ConnectTimeout=5 "$target" \
    "test -e '$receipt' -o -L '$receipt'" </dev/null >/dev/null 2>&1
}

fetch_one() {
  local site=$1 path=$2 expected=$3 destination=$4 rec target remote_root tmp got
  rec=$(site_record "$site") || return 1
  IFS=$'\t' read -r _ _ _ remote_root <<< "$rec"
  tmp=$(mktemp "${destination}.candidate.XXXXXX")
  if [[ "$site" == "$LOCAL_SITE" ]]; then
    rm -f "$tmp"; return 1
  fi
  target=$(peer_target "$site") || { rm -f "$tmp"; return 1; }
  if ! ssh -o BatchMode=yes -o ConnectTimeout=5 "$target" \
      "cat -- '$remote_root/$path'" </dev/null > "$tmp" 2>/dev/null; then
    rm -f "$tmp"; return 1
  fi
  got=$(sha256sum "$tmp" | awk '{print $1}')
  if [[ "$got" != "$expected" ]]; then
    echo "PEER_HASH_MISMATCH site=$site path=$path expected=$expected got=$got" >&2
    rm -f "$tmp"; return 1
  fi
  chmod 0644 "$tmp"
  mv "$tmp" "$destination"
  return 0
}

write_receipt() {
  local dir=$1 stamp=$2 status=$3 verified=$4 missing=$5 mismatched=$6 files=$7 bytes=$8 sources=$9
  local receipt="$dir/${CORPUS_ID}-${stamp}.edn"
  mkdir -p "$dir"
  cat > "$receipt" <<EOF
{:corpus "$CORPUS_ID"
 :site "$LOCAL_SITE"
 :swept-at "$stamp"
 :status :$status
 :verified $verified
 :missing-before $missing
 :mismatched-before $mismatched
 :transferred-files $files
 :transferred-bytes $bytes
 :sources {$sources}}
EOF
  ln -sfn "$(basename "$receipt")" "$dir/${CORPUS_ID}-latest.edn"
  echo "receipt=$receipt"
}

sync_corpus() {
  local id=$1; shift
  local dest_root=$REPO_ROOT receipt_dir=$STATE_ROOT/receipts
  local -a selected_peers=()
  while (($#)); do
    case "$1" in
      --dest-root) dest_root=$2; shift 2 ;;
      --receipt-dir) receipt_dir=$2; shift 2 ;;
      --peer) selected_peers+=("$2"); shift 2 ;;
      *) die "unknown option: $1" ;;
    esac
  done
  load_corpus "$id"
  [[ -n "$LOCAL_SITE" ]] || die "FUTON3C_SITE is required"
  site_record "$LOCAL_SITE" >/dev/null || die "unknown FUTON3C_SITE: $LOCAL_SITE"
  if ((${#selected_peers[@]} == 0)); then
    while IFS=$'\t' read -r site _; do
      [[ "$site" = \#* || "$site" == "$LOCAL_SITE" ]] || selected_peers+=("$site")
    done < "$SITES"
  fi

  local stamp path bytes sha file got peer found
  local verified=0 missing=0 mismatched=0 transferred=0 transferred_bytes=0 failures=0
  local sources="" quarantine="$receipt_dir/quarantine"
  declare -A source_counts=()
  stamp=$(date -u +%Y%m%dT%H%M%S.%NZ)
  while IFS=$'\t' read -r path bytes sha; do
    file=$(resolve_local_path "$dest_root" "$path")
    if [[ -f "$file" ]]; then
      got=$(sha256sum "$file" | awk '{print $1}')
      if [[ "$got" == "$sha" ]]; then verified=$((verified + 1)); continue; fi
      echo "HASH_MISMATCH $path expected=$sha got=$got; quarantining before repair" >&2
      mkdir -p "$quarantine/$stamp/$(dirname "$path")"
      mv "$file" "$quarantine/$stamp/$path"
      mismatched=$((mismatched + 1))
    else
      missing=$((missing + 1))
    fi
    mkdir -p "$(dirname "$file")"
    found=""
    for peer in "${selected_peers[@]}"; do
      if fetch_one "$peer" "$path" "$sha" "$file"; then found=$peer; break; fi
    done
    if [[ -z "$found" ]]; then
      echo "NO_VERIFYING_PEER path=$path peers=${selected_peers[*]}" >&2
      failures=$((failures + 1)); continue
    fi
    echo "FETCHED site=$found path=$path bytes=$bytes"
    transferred=$((transferred + 1)); transferred_bytes=$((transferred_bytes + bytes))
    source_counts[$found]=$(( ${source_counts[$found]:-0} + 1 ))
    verified=$((verified + 1))
  done < <(manifest_rows)

  for peer in "${!source_counts[@]}"; do
    sources+="\"$peer\" ${source_counts[$peer]} "
  done
  local status=ok
  ((failures == 0)) || status=failed
  echo "status=$status verified=$verified missing-before=$missing mismatched-before=$mismatched transferred-files=$transferred transferred-bytes=$transferred_bytes"
  write_receipt "$receipt_dir" "$stamp" "$status" "$verified" "$missing" "$mismatched" "$transferred" "$transferred_bytes" "$sources"
  ((failures == 0))
}

status_corpus() {
  local id=$1 site factor=0 total=0 non_laptop_factor=0 non_laptop_total=0 state
  load_corpus "$id"
  [[ -n "$LOCAL_SITE" ]] || die "FUTON3C_SITE is required"
  echo "corpus=$CORPUS_ID schema=$CORPUS_SCHEMA"
  while IFS=$'\t' read -r site _; do
    [[ "$site" = \#* ]] && continue
    total=$((total + 1))
    if [[ "$site" != oxf ]]; then non_laptop_total=$((non_laptop_total + 1)); fi
    if remote_has_verified_tree "$site"; then
      state=VERIFIED; factor=$((factor + 1))
      if [[ "$site" != oxf ]]; then non_laptop_factor=$((non_laptop_factor + 1)); fi
    else
      state=UNVERIFIED_OR_UNREACHABLE
    fi
    site_has_receipt "$site" || state="$state,NO_RECEIPT"
    echo "site=$site state=$state"
  done < "$SITES"
  echo "replication-factor=$factor/$total required-non-laptop=2"
  echo "non-laptop-replication-factor=$non_laptop_factor/$non_laptop_total"
  ((non_laptop_factor >= 2))
}

command=${1:-}; shift || true
case "$command" in
  sync) (($# >= 1)) || die "sync requires a corpus id"; sync_corpus "$@" ;;
  verify)
    (($# >= 1)) || die "verify requires a corpus id"
    id=$1; shift; root=$REPO_ROOT
    [[ ${1:-} == --dest-root ]] && { root=$2; shift 2; }
    (($# == 0)) || die "unknown verify options"
    load_corpus "$id"; verify_tree "$root"
    ;;
  status) (($# == 1)) || die "status requires a corpus id"; status_corpus "$1" ;;
  sync-all)
    while IFS=$'\t' read -r id schema _; do
      [[ "$id" = \#* ]] && continue
      if [[ "$schema" == sha256-v1 ]]; then sync_corpus "$id"; else echo "UNSUPPORTED corpus=$id schema=$schema" >&2; fi
    done < "$CORPORA"
    ;;
  *) usage; exit 2 ;;
esac
