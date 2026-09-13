(ns futon3c.agency.invoke-lifecycle-reconciliation
  "Pure, resolver-backed census of prospective invoke lifecycle snapshots.
  It never reads or compacts the serving ledger and never authorizes restart."
  (:require [clojure.edn :as edn]
            [clojure.set :as set])
  (:import (java.io PushbackReader StringReader)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)
           (java.security MessageDigest)))

(defn- refuse! [code data]
  (throw (ex-info (name code) (assoc data :refusal code))))

(defn- sha256 [^bytes bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))

(defn file-snapshot-resolver
  "Return a resolver that reads PATH once. EXPECTED-SHA is configured outside
  the candidate snapshot and authenticates the exact parsed byte buffer."
  [path expected-sha]
  (fn []
    (let [bs (try (Files/readAllBytes (Path/of (str path) (make-array String 0)))
                  (catch Throwable e
                    (throw (ex-info "snapshot read failed"
                                    {:refusal :reconcile/source-missing :path (str path)} e))))]
      {:bytes bs :expected-sha256 expected-sha :path (str path)})))

(defn- read-one [^bytes bs]
  (try
    (with-open [r (PushbackReader. (StringReader. (String. bs StandardCharsets/UTF_8)))]
      (let [x (edn/read {:eof ::eof} r) trailing (edn/read {:eof ::eof} r)]
        (when (or (= ::eof x) (not= ::eof trailing))
          (refuse! :reconcile/source-cardinality {}))
        x))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable e
      (throw (ex-info "snapshot parse failed" {:refusal :reconcile/source-invalid} e)))))

(defn- resolve! [kind resolver schema]
  (when-not (fn? resolver) (refuse! :reconcile/source-missing {:kind kind}))
  (let [{:keys [bytes expected-sha256 path]} (resolver)
        observed (when (bytes? bytes) (sha256 bytes))]
    (when-not (and observed (= observed expected-sha256))
      (refuse! :reconcile/source-pin-mismatch
               {:kind kind :path path :expected expected-sha256 :observed observed}))
    (let [record (read-one bytes)]
      (when-not (= schema (:schema record))
        (refuse! :reconcile/source-schema-mismatch {:kind kind :schema (:schema record)}))
      {:record record :pin {:kind kind :path path :sha256 observed}})))

(defn- ids [x] (set (map str x)))

(defn reconcile
  "Join independently pinned immutable snapshots. All sources must name the
  same closed controller generation. A positive result is drain evidence only;
  :restart-authorized? is always false."
  [{:keys [controller hot-ledger accepted-queue execution final-delivery deferred]}]
  (let [resolved [(resolve! :controller controller :agency/ingress-controller-snapshot-v1)
                  (resolve! :hot-ledger hot-ledger :agency/invoke-hot-ledger-snapshot-v1)
                  (resolve! :accepted-queue accepted-queue :agency/accepted-queue-snapshot-v1)
                  (resolve! :execution execution :agency/execution-snapshot-v1)
                  (resolve! :final-delivery final-delivery :agency/final-delivery-snapshot-v1)
                  (resolve! :deferred deferred :agency/deferred-resume-snapshot-v1)]
        [c h q x d r] (map :record resolved)
        generation (:generation c)
        generations (mapv :generation [h q x d])
        jobs (:jobs h)
        queue-ids (ids (:job-ids q)) execution-ids (ids (:job-ids x))
        delivery-records (:records d) delivery-ids (set (keys delivery-records))
        overlaps (set (concat (set/intersection queue-ids execution-ids)
                              (set/intersection queue-ids delivery-ids)
                              (set/intersection execution-ids delivery-ids)))
        durable-ids (set (keys jobs))
        referenced (into #{} (concat queue-ids execution-ids delivery-ids))]
    (when-not (and (= :closed (:mode c)) (integer? generation)
                   (zero? (:waiting-writer c -1)))
      (refuse! :reconcile/controller-not-closed-and-idle {:controller c}))
    (when-not (every? #{generation} generations)
      (refuse! :reconcile/stale-generation {:controller generation :sources generations}))
    (when-not (and (map? jobs) (every? (fn [[id j]] (= id (:job-id j))) jobs))
      (refuse! :reconcile/hot-ledger-identity-invalid {}))
    (when (seq overlaps) (refuse! :reconcile/duplicate-lifecycle {:job-ids overlaps}))
    (when-let [extra (seq (set/difference referenced durable-ids))]
      (refuse! :reconcile/unknown-job-reference {:job-ids (set extra)}))
    (doseq [[id job] jobs]
      (let [state (:state job) trace-id (:trace-id job) delivery (get delivery-records id)]
        (case state
          :queued (when-not (contains? queue-ids id)
                    (refuse! :reconcile/omitted-queued-job {:job-id id}))
          :executing (when-not (contains? execution-ids id)
                       (refuse! :reconcile/omitted-executing-job {:job-id id}))
          :final-delivery (when-not (and delivery (= :pending (:status delivery)))
                            (refuse! :reconcile/delivery-state-unknown {:job-id id}))
          :terminal (when-not (and (string? trace-id) (not-empty trace-id)
                                   delivery (= :complete (:status delivery))
                                   (= id (:job-id delivery))
                                   (= trace-id (:trace-id delivery)))
                      (refuse! :reconcile/terminal-join-invalid {:job-id id}))
          (refuse! :reconcile/job-state-unknown {:job-id id :state state}))))
    (let [nonterminal (set (for [[id j] jobs :when (not= :terminal (:state j))] id))]
      (when (seq nonterminal)
        (refuse! :reconcile/nonterminal-accepted-jobs {:job-ids nonterminal})))
    (when-not (= durable-ids delivery-ids)
      (refuse! :reconcile/delivery-census-incomplete
               {:missing (set/difference durable-ids delivery-ids)
                :extra (set/difference delivery-ids durable-ids)}))
    {:schema :agency/invoke-lifecycle-reconciliation-v1
     :status :complete-census
     :generation generation
     :job-count (count jobs)
     :jobs (into (sorted-map) (map (fn [[id j]] [id (select-keys j [:job-id :trace-id :state])]) jobs))
     :deferred-resumes {:count (count (:records r)) :ids (:order r)
                        :accounting :separate-from-accepted-jobs}
     :pins (mapv :pin resolved)
     :zero-in-flight? true
     :restart-authorized? false}))
