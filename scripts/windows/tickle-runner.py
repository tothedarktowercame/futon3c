#!/usr/bin/env python
import argparse
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from string import Template
from urllib import error, request


def register_tickle(base_url: str) -> None:
    url = base_url.rstrip("/") + "/api/alpha/agents"
    payload = json.dumps({"agent-id": "tickle-1", "type": "tickle"}).encode("utf-8")
    req = request.Request(
        url,
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    print(f"[tickle] Registering tickle-1 on {base_url}...", flush=True)
    try:
        with request.urlopen(req, timeout=3) as resp:
            print(f"[tickle] Registration request returned HTTP {resp.status}.", flush=True)
    except Exception as exc:
        print(f"[tickle] Registration may have failed — continuing: {exc}", flush=True)


def build_clojure_expr(base_url: str, interval_ms: int, threshold_s: int) -> str:
    template = Template(
        r"""
(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[futon3c.agents.tickle :as tickle]
         '[futon3c.evidence.http-backend :as http-backend]
         '[org.httpkit.client :as http])

(def base-url $base_url)

(defn fetch-agents []
  (try
    (let [resp @(http/get (str base-url "/api/alpha/agents") {:timeout 5000})
          parsed (json/parse-string (:body resp) true)]
      (when (:ok parsed)
        {:agents (:agents parsed)}))
    (catch Exception _ nil)))

(defn page-via-dispatch! [agent-id]
  (try
    @(http/post (str base-url "/dispatch")
                {:headers {"Content-Type" "application/json"}
                 :body (json/generate-string
                        {"msg_id" (str "tickle-page-" (java.util.UUID/randomUUID))
                         "payload" "Tickle: you have been quiet for a while. Are you still working?"
                         "from" "tickle-1"
                         "to" agent-id})
                 :timeout 10000})
    {:paged? true :agent-id agent-id :method :dispatch}
    (catch Exception e
      {:paged? false :agent-id agent-id :method :dispatch :error (.getMessage e)})))

(println "[tickle] Watchdog running.")

(let [evidence-store (http-backend/make-http-backend base-url)
      {:keys [stop-fn started-at]}
      (tickle/start-watchdog!
       {:evidence-store evidence-store
        :interval-ms $interval_ms
        :threshold-seconds $threshold_s
        :page-config {:ring-test-bell! (fn [{:keys [agent-id]}]
                                         (println (str "[tickle] Paging " agent-id " via dispatch"))
                                         (let [r (page-via-dispatch! agent-id)]
                                           (println (str "[tickle]   result: " r))
                                           r))}
        :escalate-config {:notify-fn (fn [agent-id reason]
                                       (println (str "[tickle] ESCALATE: " agent-id " reason=" reason)))}
        :on-cycle (fn [{:keys [scanned stalled paged escalated]}]
                    (println (str "[tickle] cycle: scanned=" scanned
                                  " stalled=" (vec stalled)
                                  " paged=" (vec paged)
                                  " escalated=" (vec escalated)))
                    (flush))
        :registry-snapshot (or (fetch-agents) [])})]
  (println (str "[tickle] Started at " started-at))
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    ^Runnable
    (fn []
      (println "\n[tickle] Stopping...")
      (stop-fn)
      (println "[tickle] Stopped."))))
  @(promise))
"""
    )
    return template.substitute(
        base_url=json.dumps(base_url),
        interval_ms=interval_ms,
        threshold_s=threshold_s,
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Start the Tickle watchdog agent on the Windows futon3c lane."
    )
    parser.add_argument(
        "--interval",
        type=int,
        default=max(1, int(os.environ.get("TICKLE_INTERVAL_MS", "60000")) // 1000),
        help="watchdog scan interval in seconds (default: 60)",
    )
    parser.add_argument(
        "--threshold",
        type=int,
        default=int(os.environ.get("TICKLE_THRESHOLD_S", "300")),
        help="stale threshold in seconds (default: 300)",
    )
    parser.add_argument(
        "--base-url",
        default=os.environ.get("FUTON3C_EVIDENCE_BASE", "http://localhost:7070"),
        help="agency base URL (default: http://localhost:7070)",
    )
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    run_clojure = script_dir / "run-clojure-windows.bat"
    if not run_clojure.exists():
        print(f"[tickle-windows] ERROR: missing {run_clojure}", file=sys.stderr, flush=True)
        return 1

    register_tickle(args.base_url)
    interval_ms = args.interval * 1000
    print(
        f"[tickle] Starting watchdog (interval={interval_ms}ms, threshold={args.threshold}s)",
        flush=True,
    )
    print(f"[tickle] Agency: {args.base_url}", flush=True)
    print("[tickle] Press Ctrl-C to stop.", flush=True)
    print(flush=True)

    expr = build_clojure_expr(args.base_url, interval_ms, args.threshold)
    repo_root = script_dir.parent.parent
    temp_dir = repo_root / ".tmp" / "tickle"
    temp_dir.mkdir(parents=True, exist_ok=True)

    temp_path = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            suffix=".clj",
            prefix="tickle-runner-",
            dir=temp_dir,
            delete=False,
        ) as handle:
            handle.write(expr)
            temp_path = Path(handle.name)

        result = subprocess.run(
            [str(run_clojure), str(temp_path)],
            check=False,
        )
        return int(result.returncode)
    finally:
        if temp_path is not None:
            try:
                temp_path.unlink()
            except OSError:
                pass


if __name__ == "__main__":
    raise SystemExit(main())
