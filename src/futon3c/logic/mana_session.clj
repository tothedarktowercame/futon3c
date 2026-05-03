(ns futon3c.logic.mana-session
  "Read-side binding to nonstarter mana for the metabolic-balance
   check-fn (per ARGUE-3 amendment in V-6: drain measures per-repo;
   *balance aggregates per-session*).

   The nonstarter HTTP API at futon5/src/nonstarter/api.clj exposes:
     GET /api/mana?session-id=<id>   → {:session-id :earned :spent :balance}
     GET /api/pool                   → {:balance :total-donated :total-funded}

   Production precedent for sessions as the mana unit:
   `futon5/scripts/nonstarter_mana.clj`'s `sospeso` command takes a
   `--session` parameter and writes to `mana_events` via
   `nonstarter.db/record-mana!`.

   This namespace exposes:
     - `mana-summary` — GET /api/mana?session-id=...
     - `pool-stats`   — GET /api/pool
   Both gracefully return nil when nonstarter is unreachable so the
   metabolic-balance check-fn can keep running without a hard
   dependency on nonstarter being live.

   Write-side (recording block-closure mana awards) is deferred:
   nonstarter's API exposes no POST for record-mana yet; for the
   first INSTANTIATE the operator records via the CLI
   (`bb -m scripts.nonstarter-mana sospeso ...`) and the check-fn
   reads what's there.

   Mission: M-bounded-in-flight-state INSTANTIATE Block 5."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def default-nonstarter-base-url
  "Default nonstarter API base URL. Per the boot output of
   ./scripts/dev-laptop-env, nonstarter listens on :7072."
  (or (System/getenv "NONSTARTER_API_URL")
      "http://localhost:7072"))

(def default-request-timeout-ms
  "Hard cap on each HTTP request to nonstarter. The check-fn must
   not block boot if nonstarter is slow or down."
  1500)

(defn- get-json
  "GET URL with query params, parse JSON body. Returns nil on any
   network/parse failure — never throws. The metabolic-balance
   check-fn must remain robust to nonstarter being down."
  [url query-params]
  (try
    (let [resp (http/get url
                         {:query-params query-params
                          :throw false
                          :timeout default-request-timeout-ms})]
      (when (= 200 (:status resp))
        (let [body (:body resp)]
          (when (string? body)
            (try
              (json/parse-string body true)
              (catch Throwable _ nil))))))
    (catch Throwable _ nil)))

(defn mana-summary
  "Return {:session-id :earned :spent :balance} for SESSION-ID, or
   nil if nonstarter is unreachable / the session has no events.

   Optional opts:
     :base-url — default `default-nonstarter-base-url`
                 (overrideable for tests)."
  ([session-id] (mana-summary session-id {}))
  ([session-id {:keys [base-url] :or {base-url default-nonstarter-base-url}}]
   (when (and session-id (not (str/blank? (str session-id))))
     (get-json (str base-url "/api/mana")
               {"session-id" (str session-id)}))))

(defn pool-stats
  "Return {:balance :total-donated :total-funded} for the
   nonstarter pool, or nil if unreachable."
  ([] (pool-stats {}))
  ([{:keys [base-url] :or {base-url default-nonstarter-base-url}}]
   (get-json (str base-url "/api/pool") {})))

(defn ^:no-doc reachable?
  "Quick check: is nonstarter responding at all? Used by callers
   that want to disambiguate \"no session events\" from \"nonstarter
   is down\". The cost of this check is one short HTTP request;
   keep it off the hot path."
  ([] (reachable? {}))
  ([{:keys [base-url] :or {base-url default-nonstarter-base-url}}]
   (some? (pool-stats {:base-url base-url}))))

(defn balance-for-session
  "Convenience: return just the :balance number for SESSION-ID,
   or nil if unavailable. Useful for HUD widgets and
   metabolic-balance per-session aggregation."
  ([session-id] (balance-for-session session-id {}))
  ([session-id opts]
   (some-> (mana-summary session-id opts) :balance)))
