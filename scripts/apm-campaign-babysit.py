#!/usr/bin/env python3
"""Overnight babysitter for the next multi-problem JIT problem queue.

Waits for a new :apm/jit-problem-queue coordinator (more than one problem,
to distinguish it from single-problem retry queues) to appear in
data/apm-coordinators/registry.edn, then attaches automatically. No campaign
name is hardcoded since it is not known until the runner registers it.

Once attached: follows the coordinator's active frame across queue advances
(f1 -> f2 -> ...), reuses scripts/apm-watch-projection.sh (codex-10's
contract-aware watchdog) for per-frame health, and adds queue-level checks
the frame watchdog can't see: regulator status, heartbeat staleness, and new
entries appended to :regulator/failures.

An alert must survive two consecutive watchdog observations before it fires an
Agency bell. Incidents are keyed by frame and reason, so retry counters do not
turn one unresolved condition into a bell storm. Queue-level state, rather
than a regulator tick result, is authoritative for pause/completion.

Runtime configuration is available through `APM_BABYSIT_*` environment
variables; in particular `APM_BABYSIT_FROM_ID`, `APM_BABYSIT_TO_ID`, and
`APM_BABYSIT_BELLS_PAUSED`.
"""
import queue
import re
import subprocess
import threading
import time
import calendar
import os
from pathlib import Path

REPO = os.environ.get(
    "FUTON3C_REPO", str(Path(__file__).resolve().parent.parent))
REGISTRY = f"{REPO}/data/apm-coordinators/registry.edn"
WATCH_SCRIPT = f"{REPO}/scripts/apm-watch-projection.sh"
BELL_SCRIPT = f"{REPO}/scripts/bell-file.sh"
FROM_ID = os.environ.get("APM_BABYSIT_FROM_ID", "claude-cli")
TO_ID = os.environ.get("APM_BABYSIT_TO_ID", "codex-10")
PARK_DECISION_TO_ID = os.environ.get(
    "APM_BABYSIT_PARK_DECISION_TO_ID", "claude-12")
PARK_DECISIONS = os.environ.get(
    "APM_BABYSIT_PARK_DECISIONS",
    f"{REPO}/holes/labs/M-apm-demonstration/frame-park-decisions.edn")
POLL_S = int(os.environ.get("APM_BABYSIT_POLL_S", "20"))
DISCOVERY_LOG_EVERY_S = int(
    os.environ.get("APM_BABYSIT_DISCOVERY_LOG_S", "300"))
COORD_STALE_S = int(os.environ.get("APM_BABYSIT_COORD_STALE_S", "180"))
BELL_COOLDOWN_S = int(
    os.environ.get("APM_BABYSIT_BELL_COOLDOWN_S", "1200"))
# current_frame values that are placeholders, not real frame-ids -- never a
# valid target for start_watch (no such campaign directory exists).
SENTINEL_FRAMES = ("__campaign_complete__", "__queue_paused__")

# Set once a qualifying queue is discovered.
CAMPAIGN_ID = None
CAMPAIGN_DIR = None
COORD = None
QUEUE_STATE = None


def out(msg):
    print(f"BABYSIT: {msg}", flush=True)


def read_text(path):
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return None


def parse_iso(s):
    m = re.match(r"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})", s)
    if not m:
        return None
    y, mo, dd, h, mi, se = map(int, m.groups())
    return calendar.timegm((y, mo, dd, h, mi, se, 0, 0, 0))


def fmt_duration(seconds):
    seconds = max(0, int(seconds))
    h, rem = divmod(seconds, 3600)
    m, s = divmod(rem, 60)
    if h:
        return f"{h}h{m:02d}m"
    if m:
        return f"{m}m{s:02d}s"
    return f"{s}s"


def split_registry_entries(text):
    """Yield brace-matched registry entries keyed by their map key.

    Coordinator ids recur inside config and enabled-history records, so
    splitting on ``:coordinator/id`` eventually mistakes a nested history
    record for the registration that owns it.
    """
    for match in re.finditer(r'"(jit-queue:[^"]+)"\s+\{', text):
        cid = match.group(1)
        start = text.find('{', match.start())
        depth, i, in_string, escaped = 0, start, False, False
        while i < len(text):
            ch = text[i]
            if in_string:
                if escaped:
                    escaped = False
                elif ch == '\\':
                    escaped = True
                elif ch == '"':
                    in_string = False
            elif ch == '"':
                in_string = True
            elif ch == '{':
                depth += 1
            elif ch == '}':
                depth -= 1
                if depth == 0:
                    yield cid, text[start:i + 1]
                    break
            i += 1


def discover_queue(known_campaign_dir):
    """Find an enabled :apm/jit-problem-queue coordinator with >1 problems
    whose campaign dir differs from the one we're already attached to (if
    any). Prefer a queue with an active frame over an older paused queue;
    registry map order is not an activity signal. Returns a dict or None."""
    text = read_text(REGISTRY)
    if text is None:
        return None
    candidates = []
    for cid, chunk in split_registry_entries(text):
        if ':coordinator/adapter :apm/jit-problem-queue' not in chunk:
            continue
        m = re.search(r':coordinator/enabled\? (\S+?)[,}]', chunk)
        if not m or m.group(1) != 'true':
            continue
        problem_count = len(re.findall(r':problem/id "', chunk))
        if problem_count <= 1:
            continue  # single-problem retry queue, not what we're waiting for
        qm = re.search(r':queue-name "([^"]+)"', chunk)
        sm = re.search(r':coordinator/state-path "([^"]+)"', chunk)
        if not (qm and sm):
            continue
        campaign_dir = sm.group(1).rsplit('/', 1)[0]
        if campaign_dir == known_campaign_dir:
            continue
        queue = parse_queue_state(read_text(f"{campaign_dir}/queue-state.edn")) or {}
        coordinator_path = f"{campaign_dir}/coordinator.edn"
        coordinator = parse_coordinator(read_text(coordinator_path)) or {}
        try:
            touched = os.path.getmtime(coordinator_path)
        except OSError:
            touched = 0
        queue_status = queue.get('queue_status')
        active = queue.get('active_frame') is not None
        nonterminal = queue_status not in ('paused', 'complete')
        running = coordinator.get('status') in ('running', 'failed', 'stopped')
        candidates.append({
            'coordinator_id': cid, 'queue_name': qm.group(1),
            'campaign_dir': campaign_dir, 'problem_count': problem_count,
            '_rank': (active, nonterminal, running, touched),
        })
    if not candidates:
        return None
    selected = max(candidates, key=lambda candidate: candidate['_rank'])
    selected.pop('_rank')
    return selected


def parse_coordinator(text):
    if text is None:
        return None
    d = {}
    m = re.search(r':regulator/status :(\S+?)[,}]', text)
    d['status'] = m.group(1) if m else None
    # coordinator.edn retains quiescence history containing many historical
    # :regulator/ticks values before the current top-level value.  Taking the
    # first regex match made a live epoch-83 coordinator look stuck at the
    # epoch-1 tick (3980).  Tick ordinals are monotone, so the maximum is the
    # authoritative current ordinal even with retained history present.
    ticks = [int(value) for value in
             re.findall(r':regulator/ticks (\d+)', text)]
    d['ticks'] = max(ticks) if ticks else None
    m = re.search(r':regulator/updated-at "([^"]+)"', text)
    d['updated_at'] = m.group(1) if m else None
    fails = re.findall(r':failed-at "([^"]+)".*?:repair/reason "([^"]*)"', text)
    d['failure_count'] = len(fails)
    d['last_failure'] = fails[-1] if fails else None
    return d


def parse_queue_state(text):
    if text is None:
        return None
    d = {}
    active = text.find(':active ')
    if active >= 0 and not text.startswith(':active nil', active):
        m = re.search(r':frame/id "([^"]+)"', text[active:])
    else:
        m = None
    d['active_frame'] = m.group(1) if m else None
    m = re.search(r':next-index (\d+)', text)
    d['next_index'] = int(m.group(1)) if m else None
    # queue-level :status (:paused / :complete / absent-while-active) is a
    # DIFFERENT field from coordinator.edn's :regulator/status -- the
    # regulator can report :complete just because it has nothing to do this
    # tick (e.g. the queue is merely :paused), which is not the same as the
    # whole 141-problem plan being done. Only THIS field (queue-state.edn's
    # own :status, set by problem-queue-supervisor/prepare-next only when
    # next-index == count(plan.problems)) is authoritative for "batch done."
    m = re.search(r', :status :(\S+?)[,}]', text)
    d['queue_status'] = m.group(1) if m else None
    pending_parks = []
    for decision in re.finditer(r':decision/status :awaiting-decision', text):
        prefix = text[max(0, decision.start() - 4000):decision.start()]
        frames = re.findall(r':frame/id "([^"]+)"', prefix)
        types = re.findall(r':state/type :(\S+?)[,}]', prefix)
        owners = re.findall(r':decision/owner :(\S+?)[,}]',
                            text[decision.start() - 500:decision.end() + 500])
        if frames and types:
            pending_parks.append(
                {'frame_id': frames[-1], 'state_type': types[-1],
                 'owner': owners[-1] if owners else None})
    d['pending_parks'] = list({p['frame_id']: p for p in pending_parks}.values())
    return d


def reconcile_park_decisions():
    """Persist recorded decisions before considering any decision bells."""
    if not QUEUE_STATE or not os.path.exists(PARK_DECISIONS):
        return
    result = subprocess.run(
        ["clojure", "-M", "-m", "futon3c.apm.frame-park-decisions",
         QUEUE_STATE, PARK_DECISIONS],
        cwd=REPO, capture_output=True, text=True, timeout=60)
    if result.returncode != 0:
        out(f"park-decision reconciliation failed: {result.stderr.strip()}")
    elif ":changed? true" in result.stdout:
        out(f"park decisions reconciled: {result.stdout.strip()}")


def send_bell(subject, body, to_id=TO_ID):
    tmp = f"/tmp/claude-babysit-bell-{int(time.time() * 1000)}.md"
    with open(tmp, "w") as f:
        f.write(body)
    try:
        r = subprocess.run(
            [BELL_SCRIPT, "--from", FROM_ID, "--to", to_id, "--kind", "bell", tmp],
            cwd=REPO, capture_output=True, text=True, timeout=90)
        out(f"BELL SENT [{subject}]: rc={r.returncode} "
            f"{r.stdout.strip()[:200]} {r.stderr.strip()[:200]}")
    except Exception as e:
        out(f"BELL SEND FAILED [{subject}]: {e}")


last_bell = {}
# Operators can suppress delivery while retaining detection/logging by setting
# APM_BABYSIT_BELLS_PAUSED=true before launch.
BELLS_PAUSED = os.environ.get(
    "APM_BABYSIT_BELLS_PAUSED", "false").lower() in ("1", "true", "yes")


def maybe_bell(key, subject, body, to_id=TO_ID):
    now = time.time()
    prev = last_bell.get(key)
    if prev is None or now - prev >= BELL_COOLDOWN_S:
        if BELLS_PAUSED:
            out(f"BELL SUPPRESSED (paused, not interrupting codex-10) [{subject}]")
            last_bell[key] = now
            return
        send_bell(subject, body, to_id=to_id)
        last_bell[key] = now


def clear_bell(key):
    if key in last_bell:
        del last_bell[key]


current_proc = None
current_frame = None
log_q = queue.Queue()


WATCHER_EOF = "__watcher_eof__"


def reader_thread(proc):
    for line in proc.stdout:
        log_q.put(line.rstrip("\n"))
    # Tagged with the emitting process so a sentinel from a watcher that has
    # already been stopped or replaced cannot be mistaken for a live crash.
    log_q.put((WATCHER_EOF, proc))


def stop_watch():
    global current_proc
    if current_proc and current_proc.poll() is None:
        current_proc.terminate()
        try:
            current_proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            current_proc.kill()
    current_proc = None


def start_watch(frame_id):
    global current_proc, current_frame
    if not frame_id or frame_id in SENTINEL_FRAMES:
        out(f"refusing to start a frame watcher for non-frame id {frame_id!r}")
        return
    stop_watch()
    try:
        while True:
            log_q.get_nowait()
    except queue.Empty:
        pass
    log_path = f"{CAMPAIGN_DIR}/{CAMPAIGN_ID}-{frame_id}/problem-transitions.edn"
    out(f"attaching frame watcher: frame={frame_id} log={log_path}")
    proc = subprocess.Popen(
        [WATCH_SCRIPT, log_path, COORD, "120"],
        cwd=REPO, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        text=True, bufsize=1)
    t = threading.Thread(target=reader_thread, args=(proc,), daemon=True)
    t.start()
    current_proc = proc
    current_frame = frame_id


out("campaign babysitter armed; waiting for a multi-problem JIT queue "
    "to register in data/apm-coordinators/registry.edn")
last_frame_alert = None
pending_frame_alert = None
pending_frame_alert_count = 0
last_phase = None
last_coord = None
last_discovery_log = 0.0
between_frames_logged = False
# Wall-clock timing, computed here rather than by the cycle machine itself.
# Uses each transition line's own :event/observed-at (authoritative,
# independent of this script's poll cadence), not time.time().
frame_start_ts = None
phase_start_ts = None
last_seen_transition_ts = None
frame_durations = []  # [(frame_id, seconds), ...] this run
suppressed_frame_alerts = set()

while True:
    if CAMPAIGN_DIR is None:
        found = discover_queue(None)
        if found:
            CAMPAIGN_ID = found['queue_name']
            CAMPAIGN_DIR = found['campaign_dir']
            COORD = f"{CAMPAIGN_DIR}/coordinator.edn"
            QUEUE_STATE = f"{CAMPAIGN_DIR}/queue-state.edn"
            out(f"DISCOVERED queue: {found['coordinator_id']} "
                f"({found['problem_count']} problems) -> campaign_dir="
                f"{CAMPAIGN_DIR}. Attaching.")
            last_bell = {}
            last_coord = None
            current_frame = None
            last_frame_alert = None
            pending_frame_alert = None
            pending_frame_alert_count = 0
            last_phase = None
            between_frames_logged = False
            frame_start_ts = None
            phase_start_ts = None
            last_seen_transition_ts = None
        else:
            now = time.time()
            if now - last_discovery_log > DISCOVERY_LOG_EVERY_S:
                out("still waiting for the multi-problem queue to come online")
                last_discovery_log = now
            time.sleep(POLL_S)
            continue

    text = read_text(COORD)
    c = parse_coordinator(text)
    initial_q = parse_queue_state(read_text(QUEUE_STATE)) or {}
    if initial_q.get('pending_parks'):
        reconcile_park_decisions()
    q = parse_queue_state(read_text(QUEUE_STATE)) or {}
    for park in q.get('pending_parks', []):
        if park.get('owner') == 'claude-supervisor':
            maybe_bell(
                f"park-decision-{park['frame_id']}-{park['state_type']}",
                "parked APM frame awaits Claude decision",
                f"Campaign {CAMPAIGN_ID}: frame {park['frame_id']} is parked "
                f"as :{park['state_type']} with "
                f":decision/owner :claude-supervisor and "
                f":decision/status :awaiting-decision. The authoritative "
                f"park record is in {QUEUE_STATE}. Please inspect the "
                f"preserved receipt and residual, then record the disposition "
                f"or apparatus repair; do not void or silently forget the "
                f"frame.",
                to_id=PARK_DECISION_TO_ID)
    if c is None:
        maybe_bell("coordinator-missing", "coordinator.edn unreadable",
                   f"coordinator.edn at {COORD} could not be read. Please check "
                   f"the JIT queue regulator for campaign {CAMPAIGN_ID}.")
    else:
        # queue-state.edn's OWN :status is authoritative for whether the
        # 141-problem plan is actually done (problem-queue-supervisor's
        # prepare-next only sets it to :complete when next-index == total
        # problem count). coordinator.edn's :regulator/status :complete is a
        # DIFFERENT, weaker signal -- it just means the regulator has
        # nothing to do this tick, which is also true while the queue is
        # merely :paused (e.g. after a :partial frame result). Conflating
        # the two caused a real bug: the babysitter detached from a queue
        # with 140/141 problems still unprocessed because it saw
        # :regulator/status :complete and assumed the whole batch was done.
        queue_status = q.get('queue_status')

        if queue_status == 'complete':
            clear_bell("regulator-not-running")
            clear_bell("coordinator-heartbeat-stale")
            if current_frame != "__campaign_complete__":
                out(f"CAMPAIGN COMPLETE: {CAMPAIGN_ID} finished all "
                    f"{q.get('next_index')} problems (queue-state.edn "
                    f":status :complete) at coordinator tick {c['ticks']}. "
                    f"Will resume waiting for a new queue to attach to.")
                stop_watch()
                current_frame = "__campaign_complete__"
                CAMPAIGN_DIR = None
        elif queue_status == 'paused':
            clear_bell("regulator-not-running")
            clear_bell("coordinator-heartbeat-stale")
            if current_frame != "__queue_paused__":
                out(f"QUEUE PAUSED: {CAMPAIGN_ID} is paused after "
                    f"next-index={q.get('next_index')} of its problem plan "
                    f"(queue-state.edn :status :paused). The regulator being "
                    f"idle/:complete while paused is expected, not a "
                    f"failure -- not belling {TO_ID} for this. Staying "
                    f"attached; watching for the queue to resume (a new "
                    f"active frame, or queue_status leaving :paused).")
                stop_watch()
                current_frame = "__queue_paused__"
        elif c['status'] == 'complete':
            maybe_bell(
                "regulator-complete-queue-not-done",
                "regulator complete but queue-state disagrees",
                f"Campaign {CAMPAIGN_ID}: coordinator.edn reports "
                f":regulator/status :complete at tick {c['ticks']}, but "
                f"queue-state.edn's own :status is {queue_status!r} "
                f"(next-index={q.get('next_index')}) -- neither :complete "
                f"nor :paused. That doesn't match either expected terminal "
                f"case; please check whether the regulator has genuinely "
                f"stopped driving this queue with problems still "
                f"outstanding.")
        elif c['status'] != 'running':
            maybe_bell(
                "regulator-not-running", "JIT regulator not running",
                f"Campaign {CAMPAIGN_ID}: coordinator.edn reports "
                f":regulator/status :{c['status']} (expected :running) at tick "
                f"{c['ticks']}. Please investigate and restart/repair the "
                f"regulator so the campaign can keep advancing through frames "
                f"overnight.")
        else:
            clear_bell("regulator-not-running")

        updated_at = parse_iso(c['updated_at']) if c['updated_at'] else None
        if updated_at is not None and queue_status not in ('complete', 'paused'):
            age = time.time() - updated_at
            if age > COORD_STALE_S and q.get('active_frame') is None:
                maybe_bell(
                    "coordinator-heartbeat-stale", "coordinator heartbeat stale",
                    f"Campaign {CAMPAIGN_ID}: coordinator.edn :regulator/updated-at "
                    f"is {int(age)}s old (limit {COORD_STALE_S}s), "
                    f"status={c['status']}, ticks={c['ticks']}, active "
                    f"frame={q.get('active_frame')}. The JIT regulator appears stuck. "
                    f"Please investigate (exceptions, deadlocks, resource "
                    f"throttling) and repair so overnight frame progression can "
                    f"continue. (Checked and ruled out on the prior campaign: "
                    f"cgroup memory throttling -- futon3c-zone.service "
                    f"MemoryHigh/MemoryMax were raised to 64G/80G at "
                    f"2026-08-23T22:03Z; if current usage is again near that "
                    f"ceiling, that's a plausible cause to check first.)")
            else:
                # While a frame is active, its contract-aware watcher owns
                # heartbeat judgement together with job reachability and the
                # declared role timeout. The queue-level age check lacks that
                # context and otherwise emits a duplicate false alarm every
                # time a synchronous tick waits on a healthy role job.
                clear_bell("coordinator-heartbeat-stale")

        if last_coord is not None and c['failure_count'] > last_coord['failure_count']:
            ts, reason = c['last_failure'] or ("?", "?")
            maybe_bell(
                f"regulator-failure-{q.get('active_frame')}-{reason}",
                "new JIT regulator failure logged",
                f"Campaign {CAMPAIGN_ID}: coordinator.edn just logged a new "
                f":regulator/failures entry (#{c['failure_count']}): "
                f"failed-at={ts} repair-reason=\"{reason}\". Active frame: "
                f"{q.get('active_frame')}. Please investigate this tick failure and "
                f"repair the regulator so it can keep advancing through frames "
                f"overnight without a human re-prompting it each time.")

        active_frame = q.get('active_frame')
        if (active_frame is None and current_frame is not None
                and current_frame not in ("__campaign_complete__", "__queue_paused__")
                and q.get('next_index') is not None):
            if not between_frames_logged:
                out(f"queue active frame is nil (between {current_frame} and next "
                    f"mint), next-index={q.get('next_index')} -- queue may be "
                    f"minting the successor frame now. Detaching the frame "
                    f"watcher: the frame it was watching has reached a terminal "
                    f"state (certified, or parked at "
                    f":solver-human-intervention-frame-park), and a terminal "
                    f"frame is not a fault. Leaving it attached makes the "
                    f"watchdog alert forever on a finished frame while the live "
                    f"one runs unobserved -- that happened four times on the "
                    f"night of 2026-08-25/26 (f36 parked, f37 certified, f38 "
                    f"parked) and each time cost a manual restart.")
                stop_watch()
                between_frames_logged = True
        if active_frame and active_frame != current_frame:
            between_frames_logged = False
            if current_frame is not None:
                out(f"frame advanced: {current_frame} -> {active_frame} "
                    f"(next-index={q.get('next_index')})")
                if (current_frame not in ("__campaign_complete__", "__queue_paused__")
                        and frame_start_ts is not None
                        and last_seen_transition_ts is not None):
                    dur = last_seen_transition_ts - frame_start_ts
                    frame_durations.append((current_frame, dur))
                    n = len(frame_durations)
                    avg = sum(d for _, d in frame_durations) / n
                    out(f"TIMING: frame {current_frame} took {fmt_duration(dur)} "
                        f"wall-clock, first transition to last (includes any "
                        f"stall/repair time, not pure compute -- treat early "
                        f"frames as noisy). Session average over {n} "
                        f"frame{'s' if n != 1 else ''}: {fmt_duration(avg)}.")
            start_watch(active_frame)
            last_frame_alert = None
            pending_frame_alert = None
            pending_frame_alert_count = 0
            last_phase = None
            frame_start_ts = None
            phase_start_ts = None
            last_seen_transition_ts = None
            suppressed_frame_alerts = set()

        last_coord = c

    try:
        while True:
            line = log_q.get_nowait()
            if isinstance(line, tuple) and line[0] == WATCHER_EOF:
                if line[1] is not current_proc:
                    # The watcher that emitted this EOF is no longer the
                    # installed one: stop_watch() cleared current_proc (a
                    # deliberate detach at a terminal frame, or the queue
                    # pausing), or start_watch() has since replaced it. Only a
                    # sentinel from the *current* watcher means a real crash.
                    # Restarting on a stale sentinel is how f40 was re-attached
                    # seconds after it certified.
                    continue
                if current_frame in SENTINEL_FRAMES or not current_frame:
                    continue
                out(f"frame watcher process for {current_frame} exited; restarting")
                start_watch(current_frame)
                continue
            if ':watch/status :alert' in line:
                # contract-aware watchdog: {:watch/findings [{:error/code :x ...} ...]}
                codes = re.findall(r':error/code :([A-Za-z0-9?*+!_-]+)', line)
                if codes:
                    reason = '+'.join(sorted(set(codes)))
                else:
                    # legacy shell-watcher shape: {:reason :x ...}
                    m = re.search(r':reason :(\S+)', line)
                    reason = m.group(1) if m else 'unknown'
                waiting_on_healthy_job = (
                    reason == 'coordinator-heartbeat-stale'
                    and ':status :waiting-for-terminal-result' in line
                    and ':agency-job-running' in line
                    and ':job-within-declared-timeout' in line)
                if waiting_on_healthy_job:
                    key = (current_frame, reason)
                    if key not in suppressed_frame_alerts:
                        out(f"KNOWN CONDITION [{current_frame}]: coordinator "
                            "heartbeat is stale while a reachable role job is "
                            "running within its declared timeout; not belling")
                        suppressed_frame_alerts.add(key)
                    pending_frame_alert = None
                    pending_frame_alert_count = 0
                    continue
                if reason == pending_frame_alert:
                    pending_frame_alert_count += 1
                else:
                    pending_frame_alert = reason
                    pending_frame_alert_count = 1
                if pending_frame_alert_count >= 2 and reason != last_frame_alert:
                    out(f"FRAME ALERT [{current_frame}]: {line}")
                    maybe_bell(
                        f"frame-{current_frame}-{reason}",
                        f"frame {current_frame} watchdog alert: {reason}",
                        f"Campaign {CAMPAIGN_ID}, frame {current_frame}: "
                        f"scripts/apm-watch-projection.sh reported an alert:\n\n"
                        f"{line}\n\nPlease investigate and repair so this frame "
                        f"(and the overnight campaign) can keep progressing. "
                        f"Verify the fix with:\n"
                        f"  scripts/apm-watch-projection.sh --once "
                        f"{CAMPAIGN_DIR}/{CAMPAIGN_ID}-{current_frame}/"
                        f"problem-transitions.edn {COORD} 120\n"
                        f"-- should report :watch/status :healthy.")
                    last_frame_alert = reason
            elif ':watch/status :healthy' in line:
                pending_frame_alert = None
                pending_frame_alert_count = 0
                if last_frame_alert is not None:
                    out(f"FRAME RECOVERED [{current_frame}]: healthy again "
                        f"after {last_frame_alert}")
                    clear_bell(f"frame-{current_frame}-{last_frame_alert}")
                    last_frame_alert = None
            elif ':problem-projection-transition' in line:
                pm = re.search(r':phase :([A-Za-z0-9?*+!_-]+)', line)
                sm = re.search(r':event/sequence (\d+)', line)
                tm = re.search(r':event/observed-at "([^"]+)"', line)
                phase = pm.group(1) if pm else '?'
                ts = parse_iso(tm.group(1)) if tm else None
                if ts is not None:
                    if frame_start_ts is None:
                        frame_start_ts = ts
                    last_seen_transition_ts = ts
                if phase != last_phase:
                    dur_note = ""
                    if (last_phase is not None and phase_start_ts is not None
                            and ts is not None):
                        dur_note = (f" (prior phase {last_phase} took "
                                    f"{fmt_duration(ts - phase_start_ts)})")
                    out(f"TRANSITION [{current_frame}]: "
                        f"phase={phase} seq={sm.group(1) if sm else '?'}"
                        f"{dur_note}")
                    last_phase = phase
                    phase_start_ts = ts
    except queue.Empty:
        pass

    time.sleep(POLL_S)
