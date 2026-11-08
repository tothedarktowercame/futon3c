(ns futon3c.arxana.formal-f2
  "F2a store-derived patch sequencing; futon1b CAS is the only sequencer."
  (:require [futon3c.arxana.envelope :as env]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.text-rebase :as rebase]
            [futon3c.arxana.write :as write]))

(def iso-millis #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z")

(defn- endpoint [role kind id]
  {:role role :kind kind :target (case kind
                                    :clusion {:clusion/id id}
                                    :artifact-revision {:artifact/revision-id id})})

(defn patch-record [artifact-id patch]
  (records/make-clusion
   {:type-revision [:text/patch 1]
    :endpoints [(endpoint :base :artifact-revision (:base patch))]
    :asserted-content {:artifact/id artifact-id :patch/digest (rebase/patch-digest patch)
                       :author (:author patch) :operation/id (:op-id patch)
                       :hunks (:hunks (rebase/->patch patch))}}))

(defn- assert! [graph-store envelope record suffix]
  (let [r (write/execute graph-store
                         (assoc envelope :message/id
                                (str (:message/id envelope) ":" suffix ":" (:id record)))
                         :clusion/assert record)]
    (when-let [error (:error r)]
      (throw (ex-info "record write refused" {:typed-error error})))
    r))

(defn- query! [graph-store query]
  (let [{:keys [ok error]} (store/bounded-query! graph-store query)]
    (if error (throw (ex-info "store query refused" {:typed-error error})) ok)))

(defn- head! [graph-store artifact-id]
  (:head/revision-id (query! graph-store {:query/type :artifact-head
                                          :artifact-id artifact-id})))

(defn- revision! [graph-store revision-id]
  (let [r (query! graph-store {:query/type :revision-by-id :revision-id revision-id})]
    (if (= :ok (:status r)) (:revision/canonical-bytes r)
        (throw (ex-info "revision missing"
                        {:typed-error (env/error :missing :arxana.text/revision-not-found
                                                 {:revision-id revision-id} nil)})))))

(defn- internal-patch [base-text patch]
  (assoc patch :base (rebase/revision-digest base-text)))

(defn- application-record [artifact-id change-id from to patch event-time]
  (assoc
   (records/make-clusion
    {:type-revision [:text/application 1]
     :endpoints [(endpoint :change :clusion change-id)
                 (endpoint :from :artifact-revision from)
                 (endpoint :to :artifact-revision to)]
     :asserted-content {:artifact/id artifact-id :event-time event-time
                        :patch (select-keys patch [:author :op-id :hunks])}})
   :event-time event-time))

(defn- target [record role key]
  (some->> (:endpoints record) (filter #(= role (:role %))) first :target key))

(defn- record! [graph-store id]
  (let [{:keys [ok error]} (store/get-entity graph-store id)]
    (if error (throw (ex-info "record missing" {:typed-error error})) ok)))

(defn- applications-from [graph-store artifact-id revision]
  (let [r (query! graph-store {:query/type :role-reverse
                               :edge-type [:text/application 1]
                               :endpoint-role :from :endpoint-id revision
                               :limit 200 :order :asc})]
    (when (= :stale-index (:status r))
      (throw (ex-info "application index behind"
                      {:typed-error (env/error :stale :arxana.text/application-index-behind
                                               r [:retry])})))
    (->> (:edge-ids r) (map #(record! graph-store %))
         (filter #(= artifact-id (get-in % [:asserted-content :artifact/id]))) vec)))

(defn- find-chain [graph-store artifact-id from to]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from []]) seen #{from}]
    (if-let [[revision path] (peek queue)]
      (if (= revision to) path
          (let [nexts (for [app (applications-from graph-store artifact-id revision)
                            :let [next (target app :to :artifact/revision-id)]
                            :when (not (seen next))]
                        [next (conj path app)])]
            (recur (into (pop queue) nexts) (into seen (map first nexts)))))
      nil)))

(defn- compose-chain [graph-store artifact-id patch-id chain]
  (try
   (let [base-revision (target (first chain) :from :artifact/revision-id)
         base-text (revision! graph-store base-revision)
         patches (mapv (fn [app]
                         (internal-patch
                          (revision! graph-store (target app :from :artifact/revision-id))
                          (get-in app [:asserted-content :patch])))
                       chain)]
     (rebase/compose-all base-text patches))
   (catch clojure.lang.ExceptionInfo e
     ;; text_rebase/compose refuses a later change that edits inside text an
     ;; earlier change inserted. Until F2b composes such chains, say so typed.
     (throw (ex-info "application chain not composable"
                     {:typed-error (env/error :conflict :arxana.text/chain-not-composable
                                              {:artifact/id artifact-id
                                               :patch/id patch-id
                                               :application/ids (mapv :id chain)
                                               :cause (ex-message e)}
                                              [:refresh :manual-rebase])})))))

(defn- conflict-record [artifact-id base head left right overlap change-ids]
  (records/make-clusion
   {:type-revision [:text/conflict 2]
    :endpoints [(endpoint :left :clusion (:id left))
                (endpoint :right :clusion (:id right))]
    :asserted-content {:artifact/id artifact-id :base/revision-id base
                       :computed-against/head-revision-id head
                       :overlap/class overlap
                       :conflicting/change-ids (vec (sort change-ids))}}))

(defn- alternative-record [conflict patch bytes]
  (records/make-clusion
   {:type-revision [:text/alternative 1]
    :endpoints [(endpoint :conflict :clusion (:id conflict))
                (endpoint :keeps :clusion (:id patch))]
    :asserted-content {:result/digest (rebase/revision-digest bytes)
                       :result/text (rebase/canonical-str bytes)}}))

(defn- relocate-after!
  "Run the post-application hook. The revision CAS and the application record
  have already landed, so a relocation failure must not turn the response into
  an error: it is reported beside the new head, and a fresh instance can rerun
  relocation from the stored application."
  [after-application app]
  (when after-application
    (try (after-application app)
         (catch Exception e
           {:result/status :failed
            :error (or (:typed-error (ex-data e))
                       (env/error :failed :arxana.text/relocation-failed
                                  {:application/id (:id app) :cause (ex-message e)}
                                  [:retry]))}))))

(defn submit!
  [{:keys [graph-store ingest clock before-cas after-application]} envelope {:keys [artifact/id patch]}]
  (try
    (let [patch (rebase/->patch patch) prec (patch-record id patch)
          _ (assert! graph-store envelope prec "patch")
          base (:base patch) head (head! graph-store id)
          base-text (revision! graph-store base)
          incoming (internal-patch base-text patch)
          chain (when (not= base head) (find-chain graph-store id base head))]
      (when (and (not= base head) (nil? chain))
        (throw (ex-info "application chain gap"
                        {:typed-error (env/error :conflict :arxana.text/application-chain-gap
                                                 {:artifact/id id :gap/from base :gap/to head
                                                  :patch/id (:id prec)}
                                                 [:refresh :manual-rebase])})))
      (let [landed (when (seq chain) (compose-chain graph-store id (:id prec) chain))
            outcome (if landed (rebase/rebase base-text landed incoming)
                        {:result (rebase/apply-patch base-text incoming)})]
        (if-let [bytes (:result outcome)]
          (let [applied-patch (if landed (rebase/transform incoming landed) incoming)
                _ (when before-cas (before-cas {:artifact/id id :expected head}))
                result (ingest bytes head)]
            (if-let [error (:error result)]
              {:error (env/error :conflict :arxana.text/rebase-raced
                                 {:artifact/id id :patch/id (:id prec)
                                  :expected/revision-id head :cause error}
                                 [:refresh :resubmit])}
              (let [rid (get-in result [:ok :artifact/revision-id])
                    app (application-record id (:id prec) head rid applied-patch (clock))]
                (assert! graph-store envelope app "application")
                (let [relocation (relocate-after! after-application app)]
                {:ok {:result/status (if landed :rebased :applied) :artifact/id id
                      :patch/id (:id prec) :application/id (:id app)
                      :head/revision-id rid :relocation relocation}}))))
          (let [last-app (last chain)
                landed-record (record! graph-store (target last-app :change :clusion/id))
                source-ops (into #{} (mapcat (comp :source-op-ids :a)
                                             (:conflicts outcome)))
                op->change (into {} (map (fn [app]
                                           [(get-in app [:asserted-content :patch :op-id])
                                            (target app :change :clusion/id)])
                                         chain))
                conflicting-change-ids (keep op->change source-ops)
                conflict (conflict-record id base head landed-record prec
                                          (-> outcome :conflicts first :class)
                                          conflicting-change-ids)
                alternatives (mapv (fn [{:keys [choice bytes]}]
                                     (alternative-record conflict
                                                         (if (= choice (:op-id landed))
                                                           landed-record prec) bytes))
                                   (:alternatives outcome))]
            (assert! graph-store envelope conflict "conflict")
            (doseq [a alternatives] (assert! graph-store envelope a "alternative"))
            {:ok {:result/status :conflict :artifact/id id :patch/id (:id prec)
                  :conflict/id (:id conflict) :head/revision-id head
                  :alternative/ids (mapv :id alternatives)}}))))
    (catch clojure.lang.ExceptionInfo e
      {:error (or (:typed-error (ex-data e))
                  (env/error :invalid :arxana.text/invalid-patch (ex-data e) nil))})))

(defn- minimal-replacement
  "One hunk covering only what differs between from-text and to-text, in
  codepoints. A whole-text hunk would make every older offline patch conflict
  with the choice when it is rebased across it."
  [^String from-text ^String to-text author op-id]
  (let [a (.toArray (.codePoints from-text)) b (.toArray (.codePoints to-text))
        na (alength a) nb (alength b)
        pre (loop [i 0] (if (and (< i na) (< i nb) (= (aget a i) (aget b i))) (recur (inc i)) i))
        suf (loop [j 0] (if (and (< j (- na pre)) (< j (- nb pre))
                                 (= (aget a (- na 1 j)) (aget b (- nb 1 j))))
                          (recur (inc j)) j))]
    {:author author :op-id op-id
     :hunks [{:start pre :end (- na suf)
              :replacement (String. b (int pre) (int (- nb suf pre)))}]}))

(defn choose!
  [{:keys [graph-store ingest clock before-cas after-application]} envelope body]
  (let [artifact-id (:artifact/id body) conflict-id (:conflict/id body)
        alternative-id (:alternative/id body) actor (:actor body)
        chosen-at (:chosen-at body) reason (:reason body)]
    (try
      (cond
        (not (and (string? actor) (seq actor) (re-matches iso-millis chosen-at)))
        {:error (env/error :invalid :arxana.text/choice-attribution
                           {:actor actor :chosen-at chosen-at
                            :required-time "yyyy-MM-ddTHH:mm:ss.SSSZ"} nil)}
        :else
        (let [conflict (record! graph-store conflict-id)
              alternative (record! graph-store alternative-id)
              expected (get-in conflict [:asserted-content :computed-against/head-revision-id])
              current (head! graph-store artifact-id)]
          (cond
            (not= conflict-id (target alternative :conflict :clusion/id))
            {:error (env/error :invalid :arxana.text/alternative-not-in-conflict
                               {:conflict/id conflict-id :alternative/id alternative-id} nil)}
            (not= expected current)
            {:error (env/error :stale :arxana.text/conflict-head-moved
                               {:expected/revision-id expected :current/revision-id current}
                               [:refresh :recompute-conflict])}
            :else
            (let [text (get-in alternative [:asserted-content :result/text])
                  _ (when before-cas (before-cas {:artifact/id artifact-id :expected expected}))
                  result (ingest text expected)]
              (if-let [error (:error result)] {:error error}
                  (let [choice (records/make-clusion
                                {:type-revision [:text/choice 1]
                                 :endpoints [(endpoint :conflict :clusion conflict-id)
                                             (endpoint :alternative :clusion alternative-id)]
                                 :asserted-content {:actor actor :chosen-at chosen-at
                                                    :reason (or reason "chosen")}})
                        rid (get-in result [:ok :artifact/revision-id])
                        from-text (revision! graph-store expected)
                        p (minimal-replacement from-text text actor (str "choice:" (:id choice)))
                        app (application-record artifact-id (:id choice) expected rid p (clock))]
                    (assert! graph-store envelope choice "choice")
                    (assert! graph-store envelope app "application")
                    (let [relocation (relocate-after! after-application app)]
                    {:ok {:result/status :chosen :artifact/id artifact-id
                          :choice/id (:id choice) :application/id (:id app)
                          :head/revision-id rid :relocation relocation}})))))))
      (catch clojure.lang.ExceptionInfo e
        {:error (or (:typed-error (ex-data e))
                    (env/error :invalid :arxana.text/invalid-choice (ex-data e) nil))}))))
