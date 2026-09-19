(ns futon3c.test-registry.validation
  "Generic subject-to-warrant indexing and revalidation readout.

  This namespace reports evidence; it never gates a machine run. All durable
  locations and the evidence backend are explicit in the library API."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.evidence.store :as store]
            [futon3c.test-registry :as registry])
  (:import [java.nio.file Files StandardOpenOption]
           [java.time Instant]
           [java.util UUID]))

(defn- refuse! [reason details]
  (throw (ex-info (name reason)
                  {:record/type :test-registry.validation/refusal
                   :reason reason :details details})))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))

(defn- instant! [x field]
  (when-not (nonblank? x) (refuse! :invalid-instant {:field field :value x}))
  (try (Instant/parse x)
       (catch Exception _ (refuse! :invalid-instant {:field field :value x}))))

(defn- append-edn! [file value]
  (let [f (io/file file)]
    (when-let [parent (.getParentFile f)] (.mkdirs parent))
    (Files/writeString (.toPath f) (str (pr-str value) "\n")
                       (into-array StandardOpenOption
                                   [StandardOpenOption/CREATE StandardOpenOption/APPEND]))
    value))

(defn resolve-options
  "Standalone callers use the same HTTP evidence backend as the registry CLI.
  An explicitly supplied isolated backend remains available for library/tests;
  the process-local default store is never used for validation warrants."
  [options]
  (if (and (:backend options) (not (identical? store/!store (:backend options))))
    options
    (assoc options :backend
           (http-backend/make-http-backend
            (or (:agency-url options)
                (System/getenv "FUTON3C_AGENCY_URL")
                (System/getenv "FUTON3C_AGENCY_BASE")
                "http://localhost:7070")))))

(defn- record-refusals! [file operation attempted-args f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo original
      (let [{:keys [reason details]} (ex-data original)]
        (try
          (append-edn! file
                       {:entry/type :refusal :entry/id (str (UUID/randomUUID))
                        :operation operation :reason reason :details details
                        :at (str (Instant/now)) :attempted-args attempted-args})
          (catch Exception append-error
            ;; Even an unwritable/missing ledger must not replace the refusal.
            (try
              (binding [*out* *err*]
                (println "validation refusal trace append failed:"
                         (str file) (.getMessage append-error)
                         "original refusal:" reason))
              (catch Exception _ nil)))))
      (throw original))))

(defn- read-ledger [file]
  (let [f (io/file file)]
    (if-not (.isFile f)
      []
      (mapv (fn [line]
              (try (edn/read-string line)
                   (catch Exception e
                     (refuse! :ledger-invalid {:path (str f) :error (.getMessage e)}))))
            (remove str/blank? (str/split-lines (slurp f)))))))

(defn bind-subject!
  "Append a subject -> warrant binding. Rebinding preserves prior entries."
  [{:keys [index-file] :as options} subject-id warrant-id actor at]
  (record-refusals!
   index-file :bind-subject!
   {:options (dissoc options :backend) :subject-id subject-id
    :warrant-id warrant-id :actor actor :at at}
   (fn []
     (when-not (every? nonblank? [index-file subject-id warrant-id actor])
        (refuse! :binding-invalid {:subject-id subject-id :warrant-id warrant-id :actor actor}))
      (instant! at :at)
      (append-edn! index-file
                   {:entry/type :subject-binding :entry/id (str (UUID/randomUUID))
                    :subject-id subject-id :warrant-id warrant-id :actor actor :at at}))))

(defn subjects [{:keys [index-file]}]
  (->> (read-ledger index-file)
       (remove #(= :refusal (:entry/type %)))
       (filter #(= :subject-binding (:entry/type %)))
       (reduce (fn [m entry] (assoc m (:subject-id entry) entry)) (sorted-map))))

(defn subject-binding [options subject-id]
  (get (subjects options) subject-id))

(defn enqueue-revalidation!
  [{:keys [queue-file] :as options} {:keys [subject-id incident] :as request}]
  (record-refusals!
   queue-file :enqueue-revalidation!
   {:options (dissoc options :backend) :request request}
   (fn []
     (when-not (and (nonblank? queue-file) (nonblank? subject-id) (map? incident)
                     (keyword? (:kind incident)) (nonblank? (:source incident))
                     (contains? incident :detail))
        (refuse! :incident-invalid {:subject-id subject-id :incident incident}))
      (instant! (:at incident) :incident-at)
      (append-edn! queue-file
                   {:entry/type :revalidation-opened :entry/id (str (UUID/randomUUID))
                    :subject-id subject-id :incident incident}))))

(defn- queue-state [queue-file]
  (reduce (fn [state entry]
            (case (:entry/type entry)
              :revalidation-opened (assoc state (:entry/id entry) entry)
              :revalidation-closed (dissoc state (:queue-entry-id entry))
              :refusal state
              (refuse! :queue-ledger-invalid {:entry entry})))
          (sorted-map) (read-ledger queue-file)))

(defn revalidation-queue [{:keys [queue-file]}]
  (vec (vals (queue-state queue-file))))

(defn- warrant-payload! [backend warrant-id]
  (let [entry (store/get-entry* backend warrant-id)]
    (when-not entry (refuse! :warrant-not-found {:warrant-id warrant-id}))
    (let [row (last (registry/read-chain! backend warrant-id))
          payload (:payload row)]
      (when-not (= :run (:kind payload))
        (refuse! :warrant-not-run {:warrant-id warrant-id :kind (:kind payload)}))
      payload)))

(defn close-revalidation!
  "Close ENTRY-ID only with the subject's currently bound run warrant, minted
  strictly after its incident. The new binding must already be appended."
  [{:keys [queue-file] :as options} entry-id warrant-id actor at]
  (record-refusals!
   queue-file :close-revalidation!
   {:options (dissoc options :backend) :entry-id entry-id
    :warrant-id warrant-id :actor actor :at at}
   (fn []
     (let [{:keys [backend] :as options} (resolve-options options)
           open (get (queue-state queue-file) entry-id)]
       (when-not open (refuse! :revalidation-not-open {:entry-id entry-id}))
       (when-not (every? nonblank? [warrant-id actor])
         (refuse! :closure-invalid {:entry-id entry-id}))
       (instant! at :at)
       (let [binding (subject-binding options (:subject-id open))
             warrant (warrant-payload! backend warrant-id)
             incident-at (instant! (get-in open [:incident :at]) :incident-at)
             minted-at (instant! (:finished-at warrant) :warrant-finished-at)]
         (when-not (= warrant-id (:warrant-id binding))
           (refuse! :fresh-warrant-not-bound
                    {:subject-id (:subject-id open) :warrant-id warrant-id
                     :bound-warrant-id (:warrant-id binding)}))
         (when-not (.isAfter minted-at incident-at)
           (refuse! :warrant-not-fresh
                    {:incident-at (str incident-at) :warrant-finished-at (str minted-at)}))
         (append-edn! queue-file
                      {:entry/type :revalidation-closed :entry/id (str (UUID/randomUUID))
                       :queue-entry-id entry-id :subject-id (:subject-id open)
                       :warrant-id warrant-id :actor actor :at at}))))))

(defn- check-binding [backend binding]
  (try
    (let [payload (warrant-payload! backend (:warrant-id binding))
          result (registry/check-record!
                  backend {:entry-id (:warrant-id binding)
                           :repo-root (:repo/root payload) :changed-paths []})]
      (cond
        (:warrant? result) {:verdict :current :check result}
        (= :stale-sha (:reason result))
        {:verdict :stale
         :closure-diff (registry/closure-diff
                        (merge (:code-files payload) (:test-files payload))
                        (merge (get-in result [:details :current :code-files])
                               (get-in result [:details :current :test-files])))
         :check result}
        :else {:verdict :unverifiable :check result}))
    (catch Exception e
      (let [reason (or (:reason (ex-data e)) :exception)]
        {:verdict (if (= :warrant-not-found reason) :no-warrant :unverifiable)
         :reason reason :detail (.getMessage e)}))))

(defn conformance [options]
  (let [{:keys [backend] :as options} (resolve-options options)
        open-by-subject (group-by :subject-id (revalidation-queue options))]
    (mapv (fn [[subject-id binding]]
            (if-let [open (seq (get open-by-subject subject-id))]
              {:subject-id subject-id :warrant-id (:warrant-id binding)
               :verdict :revalidation-open :incidents (mapv :entry/id open)}
              (merge {:subject-id subject-id :warrant-id (:warrant-id binding)}
                     (check-binding backend binding))))
          (subjects options))))

(defn report! [options]
  (let [rows (conformance options) counts (frequencies (map :verdict rows))]
    (doseq [{:keys [subject-id warrant-id verdict]} rows]
      (println subject-id (name verdict) (or warrant-id "-")))
    (println "SUMMARY" (pr-str (into (sorted-map) counts)))
    {:rows rows :summary counts}))

(defn register-and-bind!
  "The end-to-end mint: register-run! (the registry runner executes the
  command once and the ledger holds intent -> run -> closure), then, when a
  warrant resulted and the spec names a :subject-id, bind it. This is the
  invocation that existed only in one agent's transcript until 2026-09-19
  (claude-4, r112: an unregistered verification run 'is a claim, not
  evidence' precisely because this entry point was missing).

  spec keys: register-run!'s options (:repo-root :command :author
  :artifact-dir :code-paths :test-paths ...) plus optional :subject-id.
  Returns {:record ... :binding ...}; no warrant => no binding, and the
  typed reason is in the record — this CLI reports, it does not gate."
  [options spec]
  (record-refusals!
   (:index-file options) :register-and-bind!
   {:options (dissoc options :backend) :spec spec}
   (fn []
     (let [{:keys [backend] :as options}
           (resolve-options (merge (select-keys spec [:agency-url]) options))
           rec (registry/register-run! backend (dissoc spec :subject-id))
           eid (:evidence/id rec)
           warrant? (boolean (or (get-in rec [:payload :warrant?]) (:warrant? rec)))
           binding (when (and warrant? (:subject-id spec))
                     (bind-subject! options (:subject-id spec) eid
                                    (:author spec) (str (Instant/now))))]
       {:record rec :evidence-id eid :warrant? warrant? :binding binding}))))

(def ^:private default-files
  {:index-file (or (System/getenv "FUTON3C_VALIDATION_INDEX")
                   "data/test-registry-validation/subjects.ednlog")
   :queue-file (or (System/getenv "FUTON3C_REVALIDATION_QUEUE")
                   "data/test-registry-validation/revalidation.ednlog")})

(defn -main [& [command arg]]
  (try
    (let [config (when (and arg (.isFile (io/file arg)))
                   (edn/read-string (slurp arg)))
          options (resolve-options
                   (merge default-files
                          (select-keys config [:index-file :queue-file :agency-url])))]
      (case command
        "report" (report! options)
        "register"
        (if-not (and arg (.isFile (io/file arg)))
          (do (binding [*out* *err*]
                (println "register needs a spec file: ... validation register <spec.edn>"))
              (System/exit 2))
          (let [spec config
                {:keys [evidence-id warrant? binding record]}
                (register-and-bind! options spec)]
            (println "evidence-id" evidence-id)
            (println "warrant?" warrant?)
            (println "results" (pr-str (or (get-in record [:payload :results])
                                           (:results record))))
            (when binding
              (println "bound" (:subject-id binding) "->" (:warrant-id binding)))
            (when-not warrant? (System/exit 1))))
        (do (binding [*out* *err*]
              (println "usage: ... validation report [config.edn] | register <spec.edn>"))
            (System/exit 2))))
    (finally (shutdown-agents))))
