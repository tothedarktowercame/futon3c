(ns futon3c.arxana.read-cache
  "Bounded process-local Arxana read cache. Any delete/reload or restore MUST
  call bump-epoch! in each process serving reads. Epoch and TTL invalidate BOTH
  sets and record-presence bounds. TTL bounds the effects of a missed bump;
  it does not make a missed bump strongly consistent. No negative caching.")

(defprotocol CacheIdentity (cache-identity [store]))
(defn identity-key [s]
  (if (satisfies? CacheIdentity s) (cache-identity s) s))
(def ^:dynamic *enabled* (not= "0" (System/getenv "FUTON3C_ARXANA_READ_CACHE")))
(def ^:dynamic *ttl-ms* 600000)
(def ^:dynamic *capacity* 2048)
(def ^:dynamic *clock* #(System/currentTimeMillis))
(def ^:dynamic *metrics* nil)
(defonce ^:private state (atom {:epoch 0 :tick 0 :entries {}}))
(defonce ^:private counts (atom {:hits 0 :misses 0 :revalidations 0}))
(defn counters [] @counts)
(defn measure [f]
  (binding [*metrics* (atom {:hits 0 :misses 0 :revalidations 0})]
    (let [value (f)] {:value value :metrics @*metrics*})))
(defn- count! [k]
  (swap! counts update k inc)
  (when *metrics* (swap! *metrics* update k inc)))
(defn bump-epoch! [] (:epoch (swap! state #(-> % (update :epoch inc) (assoc :entries {})))))
(defn- entry [key]
  (let [e (get-in @state [:entries key])]
    (when (and e (< (- (*clock*) (:at e)) *ttl-ms*)) e)))
(defn- drop! [key] (swap! state update :entries dissoc key))
(defn- save! [epoch key value]
  (swap! state
         (fn [s]
           (if (or (not= epoch (:epoch s)) (not (pos? *capacity*))) s
               (let [tick (inc (:tick s))
                     entries (assoc (:entries s) key (assoc value :at (*clock*) :used tick))
                     overflow (- (count entries) *capacity*)]
                 (assoc s :tick tick :entries
                        (apply dissoc entries (map first (take (max 0 overflow) (sort-by (comp :used val) entries))))))))))
(defn- touch! [key]
  (swap! state (fn [s] (if (contains? (:entries s) key)
                        (-> s (update :tick inc) (assoc-in [:entries key :used] (inc (:tick s)))) s))))
(defn- valid-time? [t]
  (and (string? t) (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z" t)
       (try (java.time.Instant/parse t) true (catch Exception _ false))))
(defn record-query!
  "FETCH returns the raw bounded-query envelope. Undated records use the time
  observed present as a lower bound; earlier reads always reach the store."
  [s q fetch]
  (if (or (not *enabled*) (not (valid-time? (:valid-time q)))) (fetch q)
      (let [key [:record (identity-key s) (:record-id q)] epoch (:epoch @state)
            cached (entry key) at (:valid-time q)]
        (if (and cached (not (neg? (compare at (:bound cached)))))
          (do (count! :hits) (touch! key) (:response cached))
          (do (count! :misses)
              (let [r (fetch q) body (:ok r) record (:record body)]
                (if (and (not (:error r)) (= :ok (:status body)) (= (:record-id q) (:id record)))
                  (let [event (:event-time record)
                        bound (if (and (valid-time? event) (not (pos? (compare event at)))) event at)]
                    (save! epoch key {:response r :bound bound}))
                  (drop! key))
                r))))))
(defn- complete-set? [r ids-key]
  (let [b (:ok r)]
    (and (not (:error r)) (= :ok (:status b)) (false? (:truncated? b))
         (integer? (:total b)) (vector? (get b ids-key))
         (= (:total b) (count (get b ids-key))))))
(defn set-query!
  "Reuse a complete set only after a successful total revalidation. A limit-1
  probe may be truncated; its ids are NEVER cached. TTL expiry forces full read."
  [s q fetch]
  (if-not *enabled* (fetch q)
    (let [key [:set (identity-key s) q] epoch (:epoch @state) cached (entry key)
          ids-key (case (:query/type q) :attestations :attestation-ids :role-reverse :edge-ids)
          full (fn []
                 (count! :misses)
                 (let [r (fetch q)]
                   (if (complete-set? r ids-key) (save! epoch key {:response r}) (drop! key)) r))]
      (if-not cached (full)
        (do (count! :revalidations)
            (let [r (fetch (assoc q :limit 1)) b (:ok r) old (:ok (:response cached))]
              (cond
                (or (not= epoch (:epoch @state)) (nil? (entry key))) (full)
                (or (:error r) (not= :ok (:status b))) (do (drop! key) r)
                (and (= (:total old) (:total b)) (integer? (:total b))
                     (or (nil? (:watermark old)) (nil? (:watermark b))
                         (>= (:watermark b) (:watermark old))))
                (do (count! :hits) (touch! key)
                    ;; Preserve the full response's limit/completeness; report
                    ;; the watermark of this successful count revalidation.
                    (assoc-in (:response cached) [:ok :watermark] (:watermark b)))
                :else (full))))))))
