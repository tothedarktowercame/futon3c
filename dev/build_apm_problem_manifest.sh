#!/usr/bin/env bash
set -euo pipefail

futon_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workspace_root="$(cd "$futon_root/.." && pwd)"

tasks_tsv="$workspace_root/apm-lean/pipeline/manifest/tasks.tsv"
informal_dir="$futon_root/data/apm-informal-proofs"
cleanup_log="$futon_root/data/apm-codex-cleanup-log.edn"
dojo_dir="$futon_root/data/apm-lean-dojo-handoffs"
overrides_tsv="$futon_root/data/apm-problem-overrides.tsv"
legacy_dir="$workspace_root/apm-lean/lean-proofs"
pipeline_dir="$workspace_root/apm-lean/pipeline/lean-proofs"
canary_dir="$workspace_root/apm-lean/ApmCanaries/Frames"
out_tsv="$futon_root/data/apm-problem-manifest.tsv"

declare -A informal_paths=()
declare -A legacy_main_paths=()
declare -A legacy_statement_paths=()
declare -A pipeline_statement_paths=()
declare -A canary_main_counts=()
declare -A canary_latest_paths=()
declare -A cleanup_closed=()
declare -A cleanup_remaining=()
declare -A dojo_handoff_paths=()
declare -A dojo_states=()
declare -A machine_processing_states=()
declare -A machine_processing_notes=()

while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  id="$(basename "$path" .md)"
  id="${id#apm-}"
  informal_paths["$id"]="$path"
done < <(find "$informal_dir" -maxdepth 1 -type f -name 'apm-*.md' | sort)

while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  id="$(basename "$(dirname "$path")")"
  legacy_main_paths["$id"]="$path"
done < <(find "$legacy_dir" -maxdepth 2 -type f -name 'Main.lean' | sort)

while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  id="$(basename "$(dirname "$path")")"
  legacy_statement_paths["$id"]="$path"
done < <(find "$legacy_dir" -maxdepth 2 -type f -name 'Statement.lean' | sort)

while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  id="$(basename "$(dirname "$path")")"
  pipeline_statement_paths["$id"]="$path"
done < <(find "$pipeline_dir" -maxdepth 2 -type f -name 'Statement.lean' | sort)

while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  rel="${path#"$canary_dir"/}"
  id_upper="${rel%%/*}"
  id="${id_upper,}"
  current_count="${canary_main_counts[$id]:-0}"
  canary_main_counts["$id"]="$((current_count + 1))"

  direct_path="$canary_dir/$id_upper/Main.lean"
  if [[ -f "$direct_path" ]]; then
    canary_latest_paths["$id"]="$direct_path"
  else
    canary_latest_paths["$id"]="$path"
  fi
done < <(find "$canary_dir" -type f -name 'Main.lean' | sort)

if [[ -f "$cleanup_log" ]]; then
  while IFS= read -r line; do
    [[ "$line" =~ :problem-id\ \"([^\"]+)\" ]] || continue
    id="${BASH_REMATCH[1]}"
    if [[ "$line" =~ :closed\ ([0-9]+) ]]; then
      cleanup_closed["$id"]="${BASH_REMATCH[1]}"
    fi
    if [[ "$line" =~ :remaining\ ([0-9]+) ]]; then
      cleanup_remaining["$id"]="${BASH_REMATCH[1]}"
    fi
  done < "$cleanup_log"
fi

if [[ -d "$dojo_dir" ]]; then
  while IFS= read -r path; do
    [[ -n "$path" ]] || continue
    id="$(basename "$path" .md)"
    id="${id#apm-}"
    dojo_handoff_paths["$id"]="$path"
    if state_line="$(grep -m1 '^dojo_state:' "$path" 2>/dev/null)"; then
      dojo_states["$id"]="${state_line#dojo_state: }"
    fi
  done < <(find "$dojo_dir" -maxdepth 1 -type f -name 'apm-*.md' | sort)
fi

if [[ -f "$overrides_tsv" ]]; then
  while IFS=$'\t' read -r id machine_processing_state machine_processing_note; do
    [[ -n "$id" ]] || continue
    machine_processing_states["$id"]="$machine_processing_state"
    machine_processing_notes["$id"]="$machine_processing_note"
  done < <(tail -n +2 "$overrides_tsv")
fi

{
  printf 'id\tsubject\tyear\tsession\tnumber\ttex_path\tmachine_processing_state\tmachine_processing_note\tprocessed_level\thas_informal_outline\tinformal_proof_path\thas_legacy_main\tlegacy_main_path\thas_legacy_statement\tlegacy_statement_path\thas_pipeline_statement\tpipeline_statement_path\tcanary_main_count\tlatest_canary_main_path\tcleanup_closed\tcleanup_remaining\tcleanup_state\tdojo_handoff_path\tdojo_state\n'

  tail -n +2 "$tasks_tsv" | while IFS=$'\t' read -r id subject year session number tex_path; do
    machine_processing_state="${machine_processing_states[$id]:-}"
    machine_processing_note="${machine_processing_notes[$id]:-}"
    informal_path="${informal_paths[$id]:-}"
    legacy_main_path="${legacy_main_paths[$id]:-}"
    legacy_statement_path="${legacy_statement_paths[$id]:-}"
    pipeline_statement_path="${pipeline_statement_paths[$id]:-}"
    canary_count="${canary_main_counts[$id]:-0}"
    canary_latest_path="${canary_latest_paths[$id]:-}"
    closed="${cleanup_closed[$id]:-}"
    remaining="${cleanup_remaining[$id]:-}"
    dojo_handoff_path="${dojo_handoff_paths[$id]:-}"
    dojo_state="${dojo_states[$id]:-}"

    cleanup_state=""
    if [[ -n "$remaining" ]]; then
      if [[ "$remaining" == "0" ]]; then
        cleanup_state="complete"
      else
        cleanup_state="blocked"
      fi
    fi

    processed_level="source_only"
    if [[ -n "$informal_path" ]]; then
      processed_level="informal_outline_present"
    fi
    if [[ -n "$legacy_main_path" || -n "$legacy_statement_path" ]]; then
      processed_level="legacy_lean_present"
    fi
    if [[ -n "$pipeline_statement_path" ]]; then
      processed_level="pipeline_statement_present"
    fi
    if [[ "$canary_count" != "0" ]]; then
      processed_level="canary_frame_present"
    fi
    if [[ "$cleanup_state" == "blocked" ]]; then
      processed_level="codex_cleanup_blocked"
    fi
    if [[ "$dojo_state" == "dojo_ready" ]]; then
      processed_level="lean_dojo_handoff_ready"
    fi
    if [[ "$cleanup_state" == "complete" ]]; then
      processed_level="codex_cleanup_complete"
    fi

    has_informal_outline=false
    has_legacy_main=false
    has_legacy_statement=false
    has_pipeline_statement=false
    [[ -n "$informal_path" ]] && has_informal_outline=true
    [[ -n "$legacy_main_path" ]] && has_legacy_main=true
    [[ -n "$legacy_statement_path" ]] && has_legacy_statement=true
    [[ -n "$pipeline_statement_path" ]] && has_pipeline_statement=true

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$id" "$subject" "$year" "$session" "$number" "$tex_path" "$machine_processing_state" "$machine_processing_note" "$processed_level" \
      "$has_informal_outline" "$informal_path" "$has_legacy_main" "$legacy_main_path" \
      "$has_legacy_statement" "$legacy_statement_path" "$has_pipeline_statement" \
      "$pipeline_statement_path" "$canary_count" "$canary_latest_path" "$closed" "$remaining" \
      "$cleanup_state" "$dojo_handoff_path" "$dojo_state"
  done
} > "$out_tsv"

printf 'Wrote %s\n' "$out_tsv"
