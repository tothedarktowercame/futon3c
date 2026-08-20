(ns futon3c.apm.campaign-observe
  "Read-only adapters from existing APM artifacts/API responses to reconciliation facts.

  These functions normalize observations; they do not decide whether facts
  agree. Files are hashed from bytes actually read. API response bodies are
  supplied by the caller so observation remains testable and transport-neutral."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.security MessageDigest]
           [java.time Instant]))

(defn- sha256 [bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn- path [x]
  (cond
    (instance? Path x) x
    (string? x) (Path/of x (make-array String 0))
    :else nil))

(defn- observation-time [now]
  (when (instance? Instant now) (str now)))

(defn- read-edn-file [source]
  (if-let [p (path source)]
    (try
      (if-not (Files/isRegularFile p (make-array LinkOption 0))
        {:ok false :error/code :observation-file-not-regular :path (str p)}
        (let [bytes (Files/readAllBytes p)
              value (edn/read-string (String. bytes StandardCharsets/UTF_8))]
          (if (map? value)
            {:ok true :path (str p) :bytes bytes :value value}
            {:ok false :error/code :observation-edn-not-map :path (str p)})))
      (catch Throwable t
        {:ok false :error/code :observation-file-unreadable :path (str p)
         :finding {:message (.getMessage t)}}))
    {:ok false :error/code :observation-path-invalid}))

(defn observe-registration [source now]
  (let [read (read-edn-file source)]
    (if-not (:ok read)
      read
      (let [registration (:value read)
            problem-id (or (get-in registration [:problem :problem-id])
                           (get-in registration [:reg/problem :problem/id]))]
        {:ok true
         :fact {:observed-at (observation-time now)
                :source/path (:path read)
                :frame-id (:reg/frame-id registration)
                :problem-id problem-id
                :registration-hash (sha256 (:bytes read))
                :harness-hash (:reg/harness-revision registration)
                :seats {:solver (:reg/solver-seat registration)
                        :guide (:reg/guide-seat registration)
                        :student (:reg/student-seat registration)
                        :proctor (:reg/proctor-seat registration)
                        :scribe (:reg/scribe-seat registration)}}}))))

(defn- compact-frame-id [x]
  (when-let [[_ n] (and (string? x) (re-find #"(?:^|/)frame-(\d+)(?:$|-)" x))]
    (str "f" n)))

(defn- common-value [values]
  (let [values (set values)]
    (when (= 1 (count values)) (first values))))

(defn observe-frame-records [sources expected-frame-id now]
  (let [reads (mapv read-edn-file sources)
        failed (filterv (complement :ok) reads)]
    (cond
      (empty? sources)
      {:ok false :error/code :frame-records-required}

      (seq failed)
      {:ok false :error/code :frame-record-read-failed :failures failed}

      :else
      (let [records (mapv :value reads)
            frame-ids (mapv #(compact-frame-id (:frame/id %)) records)
            problems (mapv :problem records)
            batches (mapv :batch records)
            statuses (mapv :frame/status records)
            actual-frame-id (common-value frame-ids)]
        {:ok true
         :fact {:observed-at (observation-time now)
                :frame-id actual-frame-id
                :problem-id (common-value problems)
                :block-id (common-value batches)
                :status (if (and (= expected-frame-id actual-frame-id)
                                 (= #{:open} (set statuses)))
                          :open :conflict)
                :records (mapv (fn [read record]
                                 {:path (:path read) :id (:frame/id record)
                                  :arm (:arm record) :seat (:seat record)
                                  :status (:frame/status record)
                                  :base-revision (:base-revision record)
                                  :digest (sha256 (:bytes read))})
                               reads records)}}))))

(defn- response-body [response]
  (if (contains? response :body) (:body response) response))

(defn- response-ok? [body]
  (or (true? (:ok body)) (true? (get body "ok"))))

(defn- value [m k]
  (or (get m k) (get m (name k))))

(defn- frame-from-agent [agent-id]
  (when-let [[_ frame] (and (string? agent-id)
                            (re-find #"^(f\d+)-" agent-id))]
    frame))

(defn normalize-binding [response now]
  (let [body (response-body response)]
    (if-not (and (map? body) (response-ok? body))
      {:ok false :error/code :binding-response-invalid :response body}
      (let [agent-id (value body :agent-id)]
        {:ok true
         :fact {:observed-at (observation-time now)
                :bound? (true? (value body :bound?))
                :agent-id agent-id
                :frame-id (frame-from-agent agent-id)
                :problem-id (value body :problem-id)
                :cycle-id (value body :cycle-id)
                :binding-version (value body :version)
                :phase (value body :phase)
                :ledger-digest (value body :ledger-digest)}}))))

(defn- normalize-state [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else nil))

(defn normalize-jobs [response now]
  (let [body (response-body response)
        jobs (value body :jobs)]
    (if-not (and (map? body) (response-ok? body) (sequential? jobs))
      {:ok false :error/code :jobs-response-invalid :response body}
      {:ok true
       :fact {:observed-at (observation-time now)
              :items (mapv (fn [job]
                             (let [agent-id (value job :agent-id)]
                               {:job-id (value job :job-id)
                                :agent-id agent-id
                                :frame-id (frame-from-agent agent-id)
                                :role (some-> agent-id
                                              (str/split #"-" 2) second keyword)
                                :state (normalize-state (value job :state))
                                :created-at (value job :created-at)
                                :started-at (value job :started-at)
                                :finished-at (value job :finished-at)}))
                           jobs)}})))

(defn observe-receipts [sources now]
  (let [reads (mapv read-edn-file sources)
        failed (filterv (complement :ok) reads)]
    (if (seq failed)
      {:ok false :error/code :receipt-read-failed :failures failed}
      {:ok true
       :fact {:observed-at (observation-time now)
              :items (mapv (fn [read]
                             (let [receipt (:value read)]
                               {:kind (:receipt/kind receipt)
                                :frame-id (:receipt/frame-id receipt)
                                :digest (sha256 (:bytes read))
                                :declared-digest (:receipt/digest receipt)
                                :path (:path read)}))
                           reads)}})))

(defn observe-facts
  "Gather all normalized routes. Returns no partial :facts when any adapter
  fails, preventing a read error from masquerading as an absent observation."
  [{:keys [registration-path frame-record-paths receipt-paths
           binding-response jobs-response expected-frame-id now]}]
  (let [routes (cond->
                {:binding (normalize-binding binding-response now)
                 :jobs (normalize-jobs jobs-response now)}
                 expected-frame-id
                 (assoc :registration (observe-registration registration-path now)
                        :frame-record (observe-frame-records frame-record-paths
                                                            expected-frame-id now)
                        :receipts (observe-receipts (or receipt-paths []) now)))
        failures (into {} (remove (comp :ok val)) routes)]
    (if (seq failures)
      {:ok false :error/code :campaign-observation-failed :failures failures}
      {:ok true :facts (into {} (map (fn [[route result]]
                                       [route (:fact result)])) routes)})))
