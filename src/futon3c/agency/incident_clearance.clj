(ns futon3c.agency.incident-clearance
  "P14 retrospective clearance: explanation and permission, never withdrawal or
   settlement. /rules retain their applied intervals. Validation uses P0's pinned
   notice-set query; no independent hand-count authority. No shared JVM reload."
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.rule-record :as store])
  (:import [java.time Instant]
           [java.net URLEncoder]))

(def recognition "this rule recognises this recorded sequence")
(defn- refuse! [field]
  (throw (ex-info "Invalid incident clearance" {:reason :invalid-incident-clearance :field field})))
(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- exact? [m ks] (and (map? m) (= (set (keys m)) ks)))
(defn- instant! [field x]
  (try (Instant/parse x) (catch Exception _ (refuse! field))))

(defn validate!
  "Context carries independently queried rule records, evidence and P0 notice IDs.
   Equal cardinality is insufficient: compensation membership must match exactly.
   This packet records debts only; settlement requires a separate sourced act."
  [r context]
  (when-not (exact? r #{:clearance/incident :clearance/resolved :clearance/measures-can-end
                        :clearance/compensation :clearance/recognition :clearance/provenance})
    (refuse! (if (:clearance/incident r) :record :clearance/incident)))
  (let [incident (:clearance/incident r)
        evidence (into {} (map (juxt :evidence/id identity) (:evidence context)))
        rules (into {} (map (juxt :hx/id identity) (:rules context)))
        resolved (:clearance/resolved r)
        source (get evidence (:source-id resolved))
        measures (:clearance/measures-can-end r)
        ids (:ids measures)
        comp (:clearance/compensation r)
        items (mapv :evidence-id comp)
        p (:clearance/provenance r)]
    (when-not (and (text? incident) (contains? evidence incident)) (refuse! :clearance/incident))
    (when-not (and (exact? resolved #{:meaning :at :source-id :commit})
                   (= :explained (:meaning resolved)) source
                   (= (:at resolved) (:evidence/at source))
                   (= (:commit resolved) (get-in context [:resolution-commit :sha]))
                   (string? (:commit resolved)) (re-matches #"[0-9a-f]{40}" (:commit resolved))
                   (str/includes? (str (get-in source [:evidence/body :text])) (subs (:commit resolved) 0 8)))
      (refuse! :clearance/resolved))
    (instant! :clearance/resolved (:at resolved))
    (when-not (and (exact? measures #{:meaning :ids}) (= :permission-only (:meaning measures))
                   (vector? ids) (seq ids) (= (count ids) (count (set ids))))
      (refuse! :clearance/measures-can-end))
    (doseq [id ids]
      (let [rule (get rules id)]
        (when-not (and (string? id) (str/starts-with? id "act:")
                       (contains? #{:rule/record "rule/record"} (:hx/type rule))
                       (= incident (get-in rule [:hx/props :rule/incident :ref/id])))
          (refuse! :clearance/measures-can-end))))
    (when-not (and (vector? comp) (= (count items) (count (set items)))
                   (= (set items) (set (:notice-ids context))))
      (refuse! :clearance/compensation))
    (doseq [c comp]
      (when-not (and (exact? c #{:evidence-id :status}) (text? (:evidence-id c))
                     (= :owed-unsettled (:status c))) (refuse! :clearance/compensation)))
    (when-not (= {:claim recognition :rule-ids ids} (:clearance/recognition r))
      (refuse! :clearance/recognition))
    (when-not (and (exact? p #{:author :recorded-at :basis :grant-status :sources})
                   (text? (:author p)) (= :historical-reconstruction (:basis p))
                   (= :unrecorded (:grant-status p))
                   (vector? (:sources p)) (seq (:sources p)) (every? text? (:sources p)))
      (refuse! :clearance/provenance))
    (instant! :clearance/provenance (:recorded-at p)))
  r)

(defn payload
  ([request context]
   (payload request context (act-harness/plain "cli:futon3c.agency.incident-clearance")))
  ([{:keys [record valid-from idempotency-key] :as request} context harness]
   (when-not (exact? request #{:record :valid-from :idempotency-key}) (refuse! :request))
   (validate! record context)
   (instant! :valid-from valid-from)
   (when-not (text? idempotency-key) (refuse! :idempotency-key))
   {:hx/type :incident/clearance :hx/mint-id true :hx/valid-time valid-from
    :hx/idempotency-key idempotency-key
    :hx/endpoints (into [(:clearance/incident record)] (get-in record [:clearance/measures-can-end :ids]))
    :hx/props (assoc record :clearance/schema 1 :clearance/valid-from valid-from
                     :act/harness (act-harness/validate! harness))}))

(defn live-context!
  "Read-only P0 capture. Shares exact origin/backfill rules and system-as-of pin.
   Subprocess is a CLI port, never invoked by the pure validator or unit tests."
  []
  (let [{:keys [exit out err]} (shell/sh "python3" "scripts/xiang2000_p0.py" "--clearance-context")]
    (when-not (zero? exit) (throw (ex-info "P0 context query failed" {:exit exit :stderr err})))
    (json/read-str out :key-fn keyword)))

(defn write!
  ([base request context]
   (write! base request context (act-harness/plain "cli:futon3c.agency.incident-clearance")))
  ([base request context harness]
   (let [p (payload request context harness)
         receipt (store/request! base "POST" "/api/alpha/hyperedge" p)
         id (:hx/id receipt)]
     (when-not (and (:ok receipt) (string? id) (str/starts-with? id "act:"))
       (throw (ex-info "Missing minted clearance receipt" receipt)))
     (let [page (store/request! base "GET"
                               (str "/api/alpha/hyperedges?type=incident%2Fclearance&limit=1000&end="
                                    (URLEncoder/encode (:clearance/incident (:record request)) "UTF-8")
                                    "&valid-as-of=" (URLEncoder/encode (:valid-from request) "UTF-8")) nil)
           matches (filter #(= id (:hx/id %)) (:hyperedges page))]
       (when-not (and (= 1 (count matches))
                      (= (select-keys p [:hx/type :hx/endpoints :hx/props])
                         (select-keys (first matches) [:hx/type :hx/endpoints :hx/props])))
         (throw (ex-info "Clearance readback mismatch" {:id id})))
       (assoc receipt :verified? true)))))

(defn -main [& args]
  (try
    (let [{:keys [write? file harness]}
          (act-harness/parse-cli args "cli:futon3c.agency.incident-clearance"
                                 "Usage: incident-clearance [--write] [--harness-kind KIND --harness-execution-id ID] FILE.edn")
          request (edn/read-string (slurp file))
          context (live-context!)]
      (prn (if write? (write! "http://127.0.0.1:7073" request context harness)
               {:ok true :dry-run? true :payload (payload request context harness)})))
    (shutdown-agents)
    (catch Exception e
      (binding [*out* *err*] (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents) (System/exit 1))))
