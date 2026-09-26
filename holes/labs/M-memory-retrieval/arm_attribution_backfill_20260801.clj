(require '[clojure.edn :as edn]
         '[clojure.java.shell :as shell]
         '[clojure.string :as str])
(import '[java.net URI]
        '[java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
          HttpResponse$BodyHandlers]
        '[java.time Duration Instant])

(def receipts-path
  "holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn")
(def ledger-path "/tmp/futon3c-invoke-jobs.edn")
(def output-path
  "holes/labs/M-memory-retrieval/arm-attribution-backfill-20260801.edn")
(def store-base "http://127.0.0.1:7073")
(def content-arm-commit "50916c844337a2547a51bdac302cd1e7a997e619")
(def content-arm-job "invoke-1785440682974-385-e69dc511")

(defn offered-bodies [document]
  (->> (:entries document)
       (filter #(= :offered (get-in % [:evidence/body :phase])))
       (sort-by :evidence/at)
       (mapv #(assoc (:evidence/body %) ::at (:evidence/at %)))))

(defn surfaced-ids [body]
  (get-in body [:memory-use :memory-use/surfaced-ids]))

(defn surfacing-via [body]
  (get-in body [:memory-use :memory-use/surfacing-via]))

(defn non-empty-unattributed? [body]
  (and (:recall-query body)
       (= :ok (:recall-status body))
       (seq (surfaced-ids body))
       (empty? (surfacing-via body))))

(def http-client (HttpClient/newHttpClient))

(defn projection-request [memory-ids at]
  (let [body (pr-str {:endpoints (vec memory-ids)
                      :limit 100
                      :valid-as-of at
                      :system-as-of at})
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create (str store-base "/api/alpha/memory/projection")))
                    (.header "Accept" "application/edn")
                    (.header "Content-Type" "application/edn")
                    (.timeout (Duration/ofSeconds 30))
                    (.POST (HttpRequest$BodyPublishers/ofString body))
                    (.build))
        response (.send http-client request (HttpResponse$BodyHandlers/ofString))]
    {:body (.body response) :status (.statusCode response)}))

(def max-busy-retries 120)

(defn projection-as-of [memory-ids at]
  (loop [attempt 0]
    (let [{:keys [body status]} (projection-request memory-ids at)]
      (cond
        (<= 200 status 299)
        (let [projection (edn/read-string body)]
          ;; The read lease is released asynchronously after the response.
          (Thread/sleep 1500)
          projection)
        (and (= status 503)
             (= :expensive-read-busy (:error (edn/read-string body)))
             (< attempt max-busy-retries))
        (do (Thread/sleep 1500)
            (recur (inc attempt)))
        :else
        (throw (ex-info "historical projection failed"
                        {:attempt attempt :status status :body body}))))))

(defn attachment-evidence [projection memory-id]
  (let [group (some #(when (= memory-id (:endpoint %)) %) (:groups projection))]
    (->> (:components group)
         (keep (fn [component]
                 (let [edge (:edge component)
                       props (:hx/props edge)
                       patterns (get-in props [:roles :patterns])]
                   (when (and (= :reviewed (:attachment-status props))
                              (seq patterns))
                     (sorted-map
                      :attachment-status :reviewed
                      :hyperedge-id (:hyperedge-id component)
                      :patterns (vec (sort patterns))
                      :review-evidence-id (get-in props [:review :evidence-id])
                      :reviewed-at (get-in props [:review :reviewed-at])
                      :system-time (:system-time props))))))
         (sort-by (juxt :system-time :hyperedge-id))
         vec)))

(defn span-seconds [start end]
  (.getSeconds (Duration/between (Instant/parse start) (Instant/parse end))))

(defn main []
  (let [receipts (edn/read-string (slurp receipts-path))
        ledger (edn/read-string (slurp ledger-path))
        commission-job (get-in ledger [:jobs content-arm-job])
        commissioned-at (:created-at commission-job)
        source-path "src/futon3c/dispatch_with_recall.clj"
        old-result (shell/sh "git" "show"
                             (str content-arm-commit "^:" source-path))
        new-result (shell/sh "git" "show"
                             (str content-arm-commit ":" source-path))
        old-source (:out old-result)
        new-source (:out new-result)
        _ (assert (= commissioned-at "2026-07-30T19:44:42.974047737Z"))
        _ (assert (zero? (:exit old-result)))
        _ (assert (zero? (:exit new-result)))
        _ (assert (not (str/includes? old-source "(:content-matches proposals)")))
        _ (assert (str/includes? new-source "(:content-matches proposals)"))
        bodies (offered-bodies receipts)
        unattributed (filterv non-empty-unattributed? bodies)
        records
        (mapv
         (fn [body]
           (let [dispatch-at (::at body)
                 ids (surfaced-ids body)]
             (if (neg? (compare dispatch-at commissioned-at))
               (let [projection (projection-as-of ids dispatch-at)
                     attributions
                     (mapv (fn [memory-id]
                             (let [attachments
                                   (attachment-evidence projection memory-id)]
                               (if (seq attachments)
                                 (sorted-map
                                  :arm :pattern
                                  :derivation
                                  (sorted-map
                                   :content-arm-available? false
                                   :historical-reviewed-pattern-attachments attachments
                                   :rule :pre-content-arm-and-pattern-attached)
                                  :memory-id memory-id)
                                 (sorted-map
                                  :arm :unresolved
                                  :derivation
                                  (sorted-map
                                   :reason :no-predispatch-reviewed-pattern-attachment)
                                  :memory-id memory-id))))
                           ids)
                     recovered? (every? #(= :pattern (:arm %)) attributions)]
                 (sorted-map
                  :attributions attributions
                  :dispatch-at dispatch-at
                  :dispatch-id (:job-id body)
                  :problem (:problem body)
                  :status (if recovered? :recovered :partly-unresolved)))
               (sorted-map
                :attributions
                (mapv #(sorted-map
                        :arm :unresolved
                        :derivation
                        (sorted-map
                         :reason :dispatch-overlaps-content-arm-implementation-window)
                        :memory-id %)
                      ids)
                :dispatch-at dispatch-at
                :dispatch-id (:job-id body)
                :problem (:problem body)
                :status :unresolved))))
         unattributed)
        recovered-attributions
        (for [record records
              :when (= :recovered (:status record))
              attribution (:attributions record)]
          (assoc attribution
                 :dispatch-at (:dispatch-at record)
                 :dispatch-id (:dispatch-id record)))
        existing-attributions
        (for [body bodies
              via (surfacing-via body)]
          {:dispatch-at (::at body)
           :dispatch-id (:job-id body)
           :memory-id (:memory-id via)
           :arm (:via via)})
        widened (concat existing-attributions recovered-attributions)
        pattern-count (count (filter #(= :pattern (:arm %)) widened))
        content-count (count (filter #(= :content-match (:arm %)) widened))
        earliest-at (first (sort (map :dispatch-at widened)))
        corpus-start (::at (first bodies))
        corpus-end (::at (last bodies))
        corpus-span (span-seconds corpus-start corpus-end)
        attributed-span (span-seconds earliest-at corpus-end)
        unresolved (filterv #(not= :recovered (:status %)) records)
        result
        (sorted-map
         :analysis
         (sorted-map
          :attributed-span-seconds attributed-span
          :attribution-complete? (empty? unresolved)
          :classification
          (cond
            (< (+ pattern-count content-count) 20) :indeterminate
            (zero? pattern-count) :pattern-arm-silent
            (>= (* 4 pattern-count) (+ pattern-count content-count))
            :pattern-arm-substantial
            :else :pattern-arm-marginal)
          :content-match-surfacings content-count
          :corpus-span-seconds corpus-span
          :coverage-not-tail? (<= corpus-span (* 2 attributed-span))
          :pattern-share (/ (double pattern-count)
                            (+ pattern-count content-count))
          :pattern-surfacings pattern-count
          :surfacing-denominator (+ pattern-count content-count)
          :unresolved-dispatches (mapv :dispatch-id unresolved))
         :derivation-boundary
         (sorted-map
          :commission-job content-arm-job
          :commissioned-at commissioned-at
          :content-arm-commit content-arm-commit
          :old-code-returned-content-matches? false
          :rule "Dispatches strictly before commissioning are not content-arm results; classification additionally requires a reviewed pattern attachment visible at both valid-as-of and system-as-of dispatch time.")
         :input (sorted-map :receipts receipts-path
                            :store store-base
                            :store-read-only? true)
         :records records
         :version 1)]
    (spit output-path (str (pr-str result) "\n"))))

(main)
