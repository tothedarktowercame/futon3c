(ns futon3c.apm.promotion-candidate-store
  "Controller-owned persistence for promotion candidates.

  A Scribe describes candidate content.  This boundary derives its evidence
  identity and digest, writes the evidence and proposed memory/assert edge,
  and reads both back before a review request can be constructed."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Instant]))

(defn- nonblank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- evidence-body [candidate]
  (select-keys candidate [:name :hook :kind :body :why :how-to-apply]))

(defn controller-source-attempts
  "Return the source receipts/jobs named by the controller-owned request."
  [request]
  (vec
   (distinct
    (concat
     (:source-attempt-ids request)
     (when-let [attempt-id (:input-attempt-id request)] [attempt-id])
     (mapcat (fn [{:keys [job-id repair-job-ids]}]
               (cond-> [] job-id (conj job-id) true (into repair-job-ids)))
             (:student-attempts request))))))

(defn canonical-candidate
  "Derive controller-owned identity and digest for one described candidate."
  [deposit-request depositor ordinal candidate]
  (let [reported-kind (:kind candidate)
        reported-source-attempts (:source-attempts candidate)
        kind (if (pipeline/proof-text?
                  candidate (:solver-certified-source deposit-request))
               :proof-text
               :memory)
        candidate (assoc candidate
                         :reported-kind reported-kind
                         :reported-source-attempts reported-source-attempts
                         :kind kind
                         :source-attempts
                         (controller-source-attempts deposit-request))
        body (evidence-body candidate)
        digest (machine/ledger-digest [body])
        identity-digest
        (machine/ledger-digest
         [{:dispatch/id (:dispatch/id deposit-request)
           :ordinal ordinal
           :depositor depositor
           :content-digest digest}])]
    (assoc candidate
           :reported-memory-id (:memory-id candidate)
           :reported-content-digest (:content-digest candidate)
           :memory-id (str "e-apm-promotion-" (subs identity-digest 0 32))
           :content-digest digest)))

(defn- candidate-errors [candidate]
  (cond-> []
    (not (nonblank? (:name candidate))) (conj :candidate-name-missing)
    (not (nonblank? (:hook candidate))) (conj :candidate-hook-missing)
    (not (nonblank? (:body candidate))) (conj :candidate-body-missing)
    (not (and (vector? (:pattern-ids candidate))
              (seq (:pattern-ids candidate))
              (every? nonblank? (:pattern-ids candidate))))
    (conj :candidate-patterns-missing)))

(defn- memory-entry [deposit-request depositor candidate]
  {:evidence/id (:memory-id candidate)
   :evidence/subject {:ref/type :problem
                      :ref/id (:problem-id deposit-request)}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author depositor
   :evidence/session-id (or (:job-id deposit-request)
                            (:dispatch/id deposit-request))
   :evidence/at (str (Instant/now))
   :evidence/body (evidence-body candidate)
   :evidence/tags [:memory :memory/assert :apm/promotion-candidate]})

(defn- memory-edge [deposit-request candidate]
  (let [memory-id (:memory-id candidate)
        problem-id (:problem-id deposit-request)
        patterns (:pattern-ids candidate)]
    {:hx/id (str "hx-mem-" (subs memory-id 2))
     :hx/type :memory/assert
     :hx/endpoints (vec (distinct (concat [memory-id problem-id] patterns)))
     :hx/props {:roles {:entry memory-id
                        :subjects (vec (cons problem-id patterns))
                        :distills []
                        :session (or (:job-id deposit-request)
                                     (:dispatch/id deposit-request))
                        :patterns patterns}
                :kind (:kind candidate)
                :name (:name candidate)
                :hook (:hook candidate)
                :volatile? false
                :state :current
                :domain :mathematics
                :attachment-status :proposed}}))

(defn- exact-entry? [expected observed]
  ;; :evidence/at is assigned on first append and retained on replay.
  (= (dissoc expected :evidence/at) (dissoc observed :evidence/at)))

(defn- exact-edge-visible? [expected fetch-hyperedges]
  (some #(and (= (:hx/id expected) (:hx/id %))
              (= (:hx/props expected) (:hx/props %)))
        (fetch-hyperedges (get-in expected [:hx/props :roles :entry]))))

(defn- materialization-witness [candidate observed]
  (let [digest (machine/ledger-digest [(:evidence/body observed)])]
    {:artifact-id (:memory-id candidate)
     :content-digest (:content-digest candidate)
     :persisted-content-digest digest
     :read-back-content-digest digest
     ;; boundary/append! returns the readable evidence id as its durable
     ;; delivery receipt.  On idempotent replay the same readable id is the
     ;; persistence authority.
     :persistence-receipt-id (:evidence/id observed)}))

(defn persist!
  "Persist and verify all candidates in DEPOSIT before independent review.

  The returned candidate vector contains only controller-derived ids and
  digests.  Partial, ambiguous, or mismatching writes fail closed."
  ([deposit deposit-request]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (persist! deposit deposit-request
               {:fetch-entry #(estore/get-entry* backend %)
                :append-entry #(boundary/append! backend %)
                :post-edge #(memory-write/post-hyperedge!
                             {:evidence-store backend} %)
                :fetch-hyperedges substrate/hyperedges-by-end})))
  ([{:keys [depositor candidates] :as deposit} deposit-request
    {:keys [fetch-entry append-entry post-edge fetch-hyperedges]}]
   (let [reported-depositor depositor
         depositor (:agent-id deposit-request)
         deposit (assoc deposit
                        :reported-depositor reported-depositor
                        :depositor depositor)
         source-attempts (controller-source-attempts deposit-request)
         shape-findings
         (mapv (fn [ordinal candidate]
                 {:ordinal ordinal :findings (candidate-errors candidate)})
               (range 1 (inc (count candidates))) candidates)
         invalid (filterv (comp seq :findings) shape-findings)]
     (if (or (not (nonblank? depositor)) (not (seq candidates))
             (empty? source-attempts) (seq invalid))
       {:ok false :error/code :promotion-candidate-content-invalid
        :findings (cond-> invalid
                    (not (nonblank? depositor))
                    (conj {:finding :controller-depositor-missing})
                    (not (seq candidates))
                    (conj {:finding :candidates-missing})
                    (empty? source-attempts)
                    (conj {:finding :controller-source-attempts-missing}))}
       (loop [remaining (map-indexed vector candidates)
              persisted []]
         (if-let [[index candidate] (first remaining)]
           (let [canonical (canonical-candidate deposit-request depositor
                                                (inc index) candidate)
                 entry (memory-entry deposit-request depositor canonical)
                 edge (memory-edge deposit-request canonical)
                 existing (fetch-entry (:memory-id canonical))
                 appended (when-not existing (append-entry entry))
                 observed (fetch-entry (:memory-id canonical))]
             (cond
               (and existing (not (exact-entry? entry existing)))
               {:ok false :error/code :promotion-candidate-id-conflict
                :memory-id (:memory-id canonical)}

               (and (nil? existing) (not (:ok appended)))
               {:ok false :error/code :promotion-candidate-evidence-write-failed
                :memory-id (:memory-id canonical) :finding appended}

               (not (exact-entry? entry observed))
               {:ok false :error/code :promotion-candidate-evidence-not-visible
                :memory-id (:memory-id canonical)}

               :else
               (let [edge-visible? (exact-edge-visible? edge fetch-hyperedges)
                     edge-result (when-not edge-visible? (post-edge edge))]
                 (cond
                   (and (not edge-visible?) (not (:ok edge-result)))
                   {:ok false :error/code :promotion-candidate-edge-write-failed
                    :memory-id (:memory-id canonical) :finding edge-result}

                   (not (exact-edge-visible? edge fetch-hyperedges))
                   {:ok false :error/code :promotion-candidate-edge-not-visible
                    :memory-id (:memory-id canonical)}

                   :else
                   (recur (next remaining)
                          (conj persisted
                                (assoc canonical :materialization
                                       (materialization-witness canonical
                                                                observed))))))))
           {:ok true
            :deposit (assoc deposit :candidates persisted)
            :candidates persisted}))))))

(defn visible?
  "Verify the persisted candidate entry, digest, and current attachment edge."
  ([candidate]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (visible? candidate #(estore/get-entry* backend %)
               substrate/hyperedges-by-end)))
  ([candidate fetch-entry fetch-hyperedges]
   (let [entry (fetch-entry (:memory-id candidate))
         edge (->> (fetch-hyperedges (:memory-id candidate))
                   (filter #(= :memory/assert (:hx/type %)))
                   (filter #(= :current (get-in % [:hx/props :state])))
                   first)
         status (get-in edge [:hx/props :attachment-status])
         edge-patterns (set (get-in edge [:hx/props :roles :patterns]))
         review (get-in edge [:hx/props :review])
         review-entry (when (and (= :reviewed status)
                                 (string? (:evidence-id review)))
                        (fetch-entry (:evidence-id review)))
         attachment-visible?
         (case status
           :proposed (= (set (:pattern-ids candidate)) edge-patterns)
           :reviewed
           (and review-entry
                (= (:memory-id candidate)
                   (get-in review-entry [:evidence/body :review/memory-id]))
                (= (:verdict review)
                   (get-in review-entry [:evidence/body :review/verdict]))
                (= edge-patterns (set (:pattern-ids review))))
           false)]
     (and entry edge
          (= (:content-digest candidate)
             (machine/ledger-digest [(:evidence/body entry)]))
          (= (:memory-id candidate) (get-in edge [:hx/props :roles :entry]))
          attachment-visible?))))

(defn review-inputs
  "Freshly read the full persisted evidence used to construct a review
  dispatch. A missing entry and an entry whose content body is absent are
  distinct apparatus failures; neither can be sent to a reviewer."
  ([candidates]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (review-inputs candidates #(estore/get-entry* backend %)
                    (substrate/configured-url))))
  ([candidates fetch-entry substrate-url]
   (loop [remaining candidates
          inputs []]
     (if-let [candidate (first remaining)]
       (let [memory-id (:memory-id candidate)
             entry (fetch-entry memory-id)
             body (:evidence/body entry)]
         (cond
           (nil? entry)
           {:ok false
            :error/code :promotion-review-candidate-evidence-unfetchable
            :memory-id memory-id}

           (not (nonblank? (:body body)))
           {:ok false
            :error/code :promotion-review-candidate-body-missing
            :memory-id memory-id}

           (not= (:content-digest candidate)
                 (machine/ledger-digest [body]))
           {:ok false
            :error/code :promotion-review-candidate-content-mismatch
            :memory-id memory-id}

           :else
           (recur (next remaining)
                  (conj inputs
                        {:memory-id memory-id
                         :content-digest (:content-digest candidate)
                         :read-ref (str (str/replace (str substrate-url) #"/$" "")
                                        "/api/alpha/evidence/" memory-id)
                         :entry entry}))))
       {:ok true :candidate-evidence inputs}))))
