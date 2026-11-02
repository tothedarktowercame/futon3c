#!/usr/bin/env python3
"""Probe whether the Place a backlog row depends on is alive.

Usage: place-probe.py OUT_JSON

Each entry names a row, the Place its evidence says is gone, and a mechanical
check: a listening port, a process, a crontab or timer entry, a file or
directory, a host. Writes {row-id: {place, check, alive: true/false/null,
evidence}}. A reading says "gone"; this says whether it is gone now. No
state is changed.
"""
import json, pathlib, subprocess, sys, datetime

CODE = pathlib.Path.home() / "code"

def sh(cmd, timeout=15):
    try:
        p = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)
        return p.returncode, p.stdout.strip()
    except Exception as e:  # noqa: BLE001
        return 99, str(e)

def port(n):
    rc, out = sh(f"ss -ltn | awk '{{print $4}}' | grep -c ':{n}$'")
    return (out.strip() not in ("", "0")), f"port :{n} listening: {out.strip() not in ('', '0')}"

def process(pat):
    # bracket the first character so the probe's own shell line does not match itself
    rc, out = sh(f"pgrep -f '[{pat[0]}]{pat[1:]}' | wc -l")
    n = int(out or 0); return n > 0, f"processes matching '{pat}': {n}"

def crontab(pat):
    rc, out = sh(f"crontab -l 2>/dev/null | grep -v '^#' | grep -ci '{pat}'")
    rc2, out2 = sh(f"systemctl --user list-timers --all 2>/dev/null | grep -ci '{pat}'")
    n = int(out or 0) + int(out2 or 0); return n > 0, f"crontab/timer entries matching '{pat}': {n} (crontab has {sh('crontab -l 2>/dev/null | grep -vc ^#')[1] or 0} lines)"

def path(p):
    q = CODE / p; return q.exists(), f"{p}: {'exists' if q.exists() else 'absent'}"

def host(h):
    rc, _ = sh(f"getent hosts {h} >/dev/null"); rc2, _ = sh(f"ping -c1 -W1 {h} >/dev/null 2>&1")
    return (rc == 0 and rc2 == 0), f"host {h}: resolves={rc == 0}, pings={rc2 == 0}"

def anyof(*checks):
    res = [c() for c in checks]; return any(r[0] for r in res), "; ".join(r[1] for r in res)

PROBES = {
    "E-clean-up-substrate-2":             ("futon1a census and registry on :7071 (futon1a superseded by futon1b, Joe 2026-11-02)", lambda: port(7071)),
    "M-populate-substrate-2":             ("futon1a census on :7071",              lambda: port(7071)),
    "M-stack-geometry":                   ("futon1a ingest target on :7071",       lambda: port(7071)),
    "M-stack-hud-refactor":               ("HUD daemon's default futon1a :7071",   lambda: port(7071)),
    "M-the-futon-stack-Q6-r12-design-choices": ("futon1a penholder (code present, service down?)", lambda: port(7071)),
    "M-legacy-sorry-cleanup":             ("futon1a erase gate (E-futon1a-archivist)", lambda: port(7071)),
    "E-live-loop-1":                      ("July arming lane and hourly WM cron",   lambda: crontab("wm\\|war\\|arm")),
    "E-live-loop-2":                      ("July microstep lane (cron)",            lambda: crontab("wm\\|war\\|microstep")),
    "M-war-machine-aif-last-mile":        ("R10 cron and futon2/data/sorrys.edn",   lambda: anyof(lambda: crontab("r10\\|wm"), lambda: path("futon2/data/sorrys.edn"))),
    "M-paper-reverse-morphogenesis":      ("APM forward-solver consumer",           lambda: (lambda rc_out: (rc_out[1].strip() not in ("", "0"), f"files mentioning forward-solver in futon3c/src, futon6: {rc_out[1].strip() or 0}"))(sh("grep -rl -i 'forward-solver\\|forward_solver' --include=*.clj --include=*.py futon3c/src futon6 2>/dev/null | wc -l"))),
    "E-evidence-flow":                    ("laptop/Dionysus mesh",                  lambda: host("dionysus")),
    "M-war-machine-frontend-upgrade1":    ("futon7 web/war-machine client",         lambda: path("futon7/web/war-machine")),
    "E-possible-world-regulator":         ("parent charter E-efe-education",        lambda: anyof(lambda: path("futon3c/holes/E-efe-education.md"), lambda: path("futon3c/holes/excursions/E-efe-education.md"), lambda: path("futon0/holes/excursions/E-efe-education.md"), lambda: path("futon2/holes/E-efe-education.md"))),
}

def main(out):
    res = {}
    for rid, (place, fn) in PROBES.items():
        try:
            alive, ev = fn()
        except Exception as e:  # noqa: BLE001
            alive, ev = None, f"probe error: {e}"
        res[rid] = {"place": place, "alive": alive, "evidence": ev}
    n_gone = sum(1 for r in res.values() if r["alive"] is False); n_alive = sum(1 for r in res.values() if r["alive"])
    pathlib.Path(out).write_text(json.dumps({"schema": "place-probe-v1", "probed-at": datetime.datetime.now(datetime.timezone.utc).replace(microsecond=0).isoformat(),
        "gone": n_gone, "alive": n_alive, "unknown": len(res) - n_gone - n_alive, "rows": res}, indent=1) + "\n")
    for rid, r in res.items(): print(f"{'GONE ' if r['alive'] is False else ('ALIVE' if r['alive'] else '?    ')} {rid:42s} {r['evidence'][:110]}")

if __name__ == "__main__":
    import os; _out = pathlib.Path(sys.argv[1]).resolve(); os.chdir(CODE); main(_out)
