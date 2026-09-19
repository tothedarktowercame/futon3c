(ns futon3c.test-registry.validation-adapters
  "Read-only incident sweeps into the generic validation revalidation queue.

  No live hooks: callers supply sources, mappings, cursor and queue paths."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.job-port :as job-port]
            [futon3c.test-registry.validation :as validation])
  (:import [java.io PushbackReader]
           [java.nio.file Files StandardOpenOption]))

(defn- refuse! [reason details]
  (throw (ex-info (name reason)
                  {:record/type :test-registry.validation-adapters/refusal
                   :reason reason :details details})))

(defn- append-edn! [file value]
  (let [f (io/file file)]
    (when-let [parent (.getParentFile f)] (.mkdirs parent))
    (Files/writeString (.toPath f) (str (pr-str value) "\n")
                       (into-array StandardOpenOption
                                   [StandardOpenOption/CREATE StandardOpenOption/APPEND]))
    value))

(defn- ledger-forms [file]
  (let [f (io/file file)]
    (if (.isFile f)
      (mapv edn/read-string (remove str/blank? (str/split-lines (slurp f))))
      [])))

(defn- strict-edn-file [file]
  (with-open [reader (PushbackReader. (io/reader file))]
    (let [eof (Object.) value (edn/read {:eof eof} reader) tail (edn/read {:eof eof} reader)]
      (when (or (identical? eof value) (not (identical? eof tail)))
        (refuse! :trip-report-invalid {:path (.getCanonicalPath (io/file file))}))
      value)))

(defn- mapped-subject [mapping source default]
  (or (cond (fn? mapping) (mapping source)
            (map? mapping) (get mapping source))
      default))

(defn- job-failure? [job]
  (or (contains? #{"failed" "timed-out"} (some-> (:state job) name))
      (= "agent-not-found" (some-> (:terminal-code job) name))
      (= "agent-not-found" (some-> (:error job) name))))

(defn- job-at [job]
  (or (:finished-at job)
      (some->> (:events job) (keep :at) last)
      (:created-at job)
      (refuse! :agency-job-time-missing {:job-id (:job-id job)})))

(defn agency-incidents
  "Pure translation of the public list endpoint response."
  ([response] (agency-incidents response nil))
  ([response subject-mapping]
   (when-not (and (map? response) (true? (:ok response)) (vector? (:jobs response)))
     (refuse! :agency-response-invalid {:response-type (type response)}))
   (mapv (fn [job]
           (let [job-id (:job-id job) agent-id (:agent-id job)]
             (when-not (and (string? job-id) (string? agent-id))
               (refuse! :agency-job-invalid {:job job}))
             {:subject-id (mapped-subject subject-mapping job-id (str "agency/" agent-id))
              :incident {:kind :agency-job-failure :source job-id :at (job-at job)
                         :detail (or (:terminal-message job) (:error job)
                                     (:result-summary job) (:terminal-code job)
                                     (str "terminal state " (:state job)))}}))
         (filter job-failure? (:jobs response)))))

(defn wm-trip-incidents
  "Read strict one-form EDN reports in DIR, sorted by canonical path."
  ([dir] (wm-trip-incidents dir nil))
  ([dir subject-mapping] (wm-trip-incidents dir subject-mapping #{}))
  ([dir subject-mapping excluded-sources]
   (let [root (io/file dir)]
     (when-not (.isDirectory root) (refuse! :trip-directory-missing {:path (str dir)}))
     (mapv (fn [file]
             (let [trip (strict-edn-file file)
                   wire (:trip/wire-id trip) path (.getCanonicalPath file)]
               (when-not (and (keyword? wire) (string? (:trip/recorded-at trip))
                              (map? (:trip/witness trip)))
                 (refuse! :trip-report-invalid {:path path}))
               {:subject-id (mapped-subject subject-mapping path
                                            (str "wm/tripwire/" (name wire)))
                :incident {:kind :wm-tripwire :source path :at (:trip/recorded-at trip)
                           :detail {:wire-id wire :witness (:trip/witness trip)}}}))
           (sort-by #(.getCanonicalPath ^java.io.File %)
                    (remove #(contains? excluded-sources (.getCanonicalPath ^java.io.File %))
                            (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn"))
                                    (or (seq (.listFiles root)) []))))))))

(defn- already-seen [options adapter]
  (let [cursor (into #{} (comp (filter #(= adapter (:adapter %))) (map :source))
                     (ledger-forms (:cursor-file options)))
        ;; Queue inspection closes the crash window after enqueue but before
        ;; cursor append. It is read-only and recognizes slice-1's exact form.
        queued (into #{} (comp (filter #(= :revalidation-opened (:entry/type %)))
                              (map #(get-in % [:incident :source])))
                     (ledger-forms (:queue-file options)))]
    (into cursor queued)))

(defn sweep-incidents!
  [options adapter incidents]
  (let [seen (atom (already-seen options adapter)) counts (atom {:enqueued 0 :skipped 0})]
    (doseq [{:keys [incident] :as item} incidents]
      (let [source (:source incident)]
        (if (contains? @seen source)
          (swap! counts update :skipped inc)
          (do (validation/enqueue-revalidation! options item)
              (append-edn! (:cursor-file options)
                           {:entry/type :incident-seen :adapter adapter
                            :source source :at (:at incident)})
              (swap! seen conj source)
              (swap! counts update :enqueued inc)))))
    @counts))

(defn sweep-agency! [{:keys [request-fn agency-base agency-limit agency-subjects] :as options}]
  (let [response (job-port/list-jobs request-fn agency-base (or agency-limit 4000))]
    (sweep-incidents! options :agency (agency-incidents response agency-subjects))))

(defn sweep-wm-trips! [{:keys [trip-directory trip-subjects] :as options}]
  (let [seen (already-seen options :wm-trips)]
    (sweep-incidents! options :wm-trips
                      (wm-trip-incidents trip-directory trip-subjects seen))))

(defn sweep! [options selection]
  (case selection
    :agency (sweep-agency! options)
    :wm-trips (sweep-wm-trips! options)
    :all (merge-with + (sweep-agency! options) (sweep-wm-trips! options))
    (refuse! :selection-invalid {:selection selection})))

(defn -main [& [command selection]]
  (when-not (and (= "sweep" command) (contains? #{"agency" "wm-trips" "all"} selection))
    (binding [*out* *err*] (println "usage: ... validation-adapters sweep [agency|wm-trips|all]"))
    (System/exit 2))
  (let [result (sweep! {:request-fn (fn [method url body]
                                      ((requiring-resolve 'futon3c.apm.live-preflight-runtime/http-json)
                                       method url body))
                        :agency-base (or (System/getenv "FUTON3C_AGENCY_BASE") "http://127.0.0.1:7070")
                        :trip-directory (or (System/getenv "FUTON2_WM_TRIPS")
                                            "/home/joe/code/futon2/data/wm-tripwires/trips")
                        :queue-file (or (System/getenv "FUTON3C_REVALIDATION_QUEUE")
                                        "data/test-registry-validation/revalidation.ednlog")
                        :cursor-file (or (System/getenv "FUTON3C_VALIDATION_CURSOR")
                                         "data/test-registry-validation/incidents.ednlog")}
                       (keyword selection))]
    (println "enqueued" (:enqueued result) "skipped" (:skipped result))))
