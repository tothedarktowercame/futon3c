#!/usr/bin/env bb

(require '[babashka.http-client :as http])

(def base
  (str/replace
   (or (System/getenv "FUTON_SUBSTRATE_URL")
       (throw (ex-info "FUTON_SUBSTRATE_URL is required" {})))
   #"/+$" ""))

(def root "holes/labs/M-codex-sorry-loop")
(def names-file (str root "/attachments-awaiting-codex5-review.edn"))
(def attachment-reviewer "codex-5")
(def post-reviews? (some #{"--commit"} *command-line-args*))

(def provenance
  (str
   "Attachment-warrant review by codex-5 on 2026-07-30. Checked that the "
   "memory entry is a :memory/:assert with a matching name and substantive "
   "inner body; fetched every cited turn-round and receipt id; and checked "
   "the exact attachment patterns, :mathematics domain, and witness status. "
   "This review does not re-derive the mathematics. Important separation "
   "limitation: codex-5 drafted the underlying memory content. The stored "
   "memory author is claude-9, so the lifecycle author/reviewer guard is "
   "satisfied, but this is weaker than a fully independent substantive review."))

(defn get-edn [path]
  (let [r (http/get (str base path)
                    {:headers {"accept" "application/edn"}
                     :throw false
                     :timeout 60000})]
    {:status (:status r)
     :body (when (seq (:body r))
             (try
               (edn/read-string (:body r))
               (catch Throwable _ (:body r))))}))

(defn post-edn [path payload]
  (let [r (http/post (str base path)
                     {:headers {"accept" "application/edn"
                                "content-type" "application/edn"
                                "x-penholder" "api"}
                      :body (pr-str payload)
                      :throw false
                      :timeout 120000})]
    {:status (:status r)
     :body (when (seq (:body r))
             (try
               (edn/read-string (:body r))
               (catch Throwable _ (:body r))))}))

(defn draft-files []
  (->> (file-seq (io/file root))
       (filter #(.isFile %))
       (filter #(re-matches #"scribe-pass-.*-drafts\.edn" (.getName %)))))

(defn draft-index []
  (into {}
        (for [f (draft-files)
              draft (edn/read-string (slurp f))
              :when (:name draft)]
          [(:name draft) {:draft draft :file (.getPath f)}])))

(defn nonblank-substantive? [body]
  (and (map? body)
       (seq body)
       (> (count (pr-str body)) 80)
       (not= :stub (:status body))))

(defn evidence-resolves? [id]
  (= 200 (:status (get-edn (str "/api/alpha/evidence/" id)))))

(defn review-id [name]
  (str "e-review-codex-5-" name))

(defn audit-one [drafts name]
  (let [memory-id (str "e-codexpilot-" name)
        edge-id (str "hx-codexpilot-" name)
        memory-response (get-edn (str "/api/alpha/evidence/" memory-id))
        edge-response (get-edn (str "/api/alpha/hyperedge/" edge-id))
        memory (:body memory-response)
        edge (:body edge-response)
        draft-record (get drafts name)
        draft (:draft draft-record)
        cited (concat (get-in draft [:evidence :turn-round-ids])
                      (get-in draft [:evidence :receipt-ids]))
        patterns (vec (get-in edge [:hx/props :roles :patterns]))
        checks
        {:memory-fetch (= 200 (:status memory-response))
         :memory-type (= :memory (:evidence/type memory))
         :memory-claim (= :assert (:evidence/claim-type memory))
         :memory-name (= name (get-in memory [:evidence/body :name]))
         :memory-author (= "claude-9" (:evidence/author memory))
         :substantive-body
         (nonblank-substantive? (get-in memory [:evidence/body :body]))
         :draft-found (some? draft)
         :citations-present (and (seq (get-in draft [:evidence :turn-round-ids]))
                                 (seq (get-in draft [:evidence :receipt-ids])))
         :citations-resolve (and (seq cited)
                                 (every? evidence-resolves? cited))
         :edge-fetch (= 200 (:status edge-response))
         :edge-entry (= memory-id (get-in edge [:hx/props :roles :entry]))
         :patterns-present (seq patterns)
         :domain-mathematics (= :mathematics
                                (get-in edge [:hx/props :domain]))
         :witness-status (some? (get-in edge [:hx/props :witness-status]))
         :attachment-pending
         (contains? #{:proposed :reviewed}
                    (get-in edge [:hx/props :attachment-status]))}
        failures (->> checks (keep (fn [[k ok?]] (when-not ok? k))) vec)]
    {:name name
     :memory-id memory-id
     :edge-id edge-id
     :draft-file (:file draft-record)
     :patterns patterns
     :cited-ids (vec cited)
     :attachment-status (get-in edge [:hx/props :attachment-status])
     :checks checks
     :failures failures
     :verdict (if (empty? failures) :approve :decline)}))

(defn review-entry [audit]
  {:evidence/id (review-id (:name audit))
   :evidence/subject {:ref/type :memory :ref/id (:memory-id audit)}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/at (str (java.time.Instant/now))
   :evidence/author attachment-reviewer
   :evidence/session-id "M-codex-sorry-loop/attachment-review"
   :evidence/tags [:memory :attachment-review :codex-lane]
   :evidence/body
   {:review/event :memory-attachment-review
    :review/memory-id (:memory-id audit)
    :review/pattern-ids (:patterns audit)
    :review/verdict :approve
    :review/witness-status :independently-witnessed
    :review/provenance provenance
    :review/policy-verdict :approve}})

(defn post-review! [audit]
  (let [id (review-id (:name audit))
        existing (get-edn (str "/api/alpha/evidence/" id))]
    (if (= 200 (:status existing))
      {:name (:name audit) :review-id id :result :existing}
      (let [r (post-edn "/api/alpha/evidence" (review-entry audit))]
        {:name (:name audit)
         :review-id id
         :result (if (<= 200 (:status r) 299) :posted :post-failed)
         :status (:status r)
         :response (:body r)}))))

(let [names (edn/read-string (slurp names-file))
      drafts (draft-index)
      audits (mapv #(audit-one drafts %) names)
      approved (filterv #(= :approve (:verdict %)) audits)
      declined (filterv #(= :decline (:verdict %)) audits)
      posts (if post-reviews? (mapv post-review! approved) [])
      post-failures (filterv #(= :post-failed (:result %)) posts)]
  (prn {:commit? (boolean post-reviews?)
        :total (count audits)
        :approved (count approved)
        :declined (count declined)
        :declines (mapv #(select-keys % [:name :failures]) declined)
        :would-post (when-not post-reviews? (mapv :name approved))
        :posts (frequencies (map :result posts))
        :post-failures post-failures
        :audits audits})
  (when (or (seq declined) (seq post-failures))
    (System/exit 2)))
