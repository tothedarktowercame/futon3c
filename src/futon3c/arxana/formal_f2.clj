(ns futon3c.arxana.formal-f2
  "F2a: codepoint patches, single-CAS sequencing, and durable conflicts."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.text-rebase :as rebase]
            [futon3c.arxana.write :as write]))

(def iso-millis #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z")

(defn state [] (atom {:applications {} :conflicts {} :alternatives {}}))

(defn- endpoint [role kind id]
  {:role role :kind kind :target (case kind
                                    :clusion {:clusion/id id}
                                    :artifact-revision {:artifact/revision-id id})})

(defn patch-record [artifact-id patch]
  (records/make-clusion
   {:type-revision [:text/patch 1]
    :endpoints [(endpoint :base :artifact-revision (:base patch))]
    :asserted-content {:artifact/id artifact-id
                       :patch/digest (rebase/patch-digest patch)
                       :author (:author patch) :operation/id (:op-id patch)
                       :hunks (:hunks (rebase/->patch patch))}}))

(defn- assert! [graph-store envelope record suffix]
  (write/execute graph-store (assoc envelope :message/id
                                    (str (:message/id envelope) ":" suffix ":" (:id record)))
                 :clusion/assert record))

(defn- revision-bytes! [provider rid]
  (let [{:keys [ok error]} (artifact/revision-bytes provider rid)]
    (if error (throw (ex-info "revision unavailable" {:typed-error error})) (:bytes ok))))

(defn- internal-patch [base-bytes patch]
  (assoc patch :base (rebase/revision-digest base-bytes)))

(defn- conflict-record [artifact-id base left right overlap]
  (records/make-clusion
   {:type-revision [:text/conflict 1]
    :endpoints [(endpoint :left :clusion (:id left))
                (endpoint :right :clusion (:id right))]
    :asserted-content {:artifact/id artifact-id :base/revision-id base
                       :overlap/class overlap}}))

(defn- alternative-record [conflict patch bytes]
  (records/make-clusion
   {:type-revision [:text/alternative 1]
    :endpoints [(endpoint :conflict :clusion (:id conflict))
                (endpoint :keeps :clusion (:id patch))]
    :asserted-content {:result/digest (rebase/revision-digest bytes)
                       :result/text (rebase/canonical-str bytes)}}))

(defn submit!
  "Submit PATCH. INGEST is a function of bytes and expected revision returning
  the revision/ingest result. It is called once: a further race is surfaced."
  [{:keys [graph-store artifact-provider text-state ingest current-head]} envelope
   {:keys [artifact/id patch]}]
  (try
    (let [patch (rebase/->patch patch)
          prec (patch-record id patch)
          _ (assert! graph-store envelope prec "patch")
          base (:base patch)
          current (or (get-in @text-state [:heads id]) (current-head id))
          base-bytes (revision-bytes! artifact-provider base)
          ip (internal-patch base-bytes patch)]
      (cond
        (= base current)
        (let [bytes (rebase/apply-patch base-bytes ip)
              result (ingest bytes current)]
          (if-let [err (:error result)]
            {:error err}
            (let [rid (get-in result [:ok :artifact/revision-id])]
              (swap! text-state (fn [s] (-> s
                                            (assoc-in [:heads id] rid)
                                            (assoc-in [:applications [id base rid]]
                                                      {:patch patch :record prec}))))
              {:ok {:result/status :applied :artifact/id id :patch/id (:id prec)
                    :head/revision-id rid}})))

        (nil? current)
        {:error (env/error :stale :arxana.text/head-unknown
                           {:artifact/id id :base/revision-id base} [:refresh])}

        :else
        (if-let [{landed :patch landed-record :record}
                 (get-in @text-state [:applications [id base current]])]
          (let [out (rebase/rebase base-bytes (internal-patch base-bytes landed) ip)]
            (if-let [bytes (:result out)]
              (let [result (ingest bytes current)]
                (if-let [err (:error result)]
                  {:error (env/error :conflict :arxana.text/rebase-raced
                                     {:artifact/id id :patch/id (:id prec)
                                      :current/revision-id current
                                      :cause err} [:refresh :resubmit])}
                  (let [rid (get-in result [:ok :artifact/revision-id])]
                    (swap! text-state (fn [s] (-> s
                                                  (assoc-in [:heads id] rid)
                                                  (assoc-in [:applications [id current rid]]
                                                            {:patch patch :record prec}))))
                    {:ok {:result/status :rebased :artifact/id id :patch/id (:id prec)
                          :head/revision-id rid}})))
              (let [overlap (-> out :conflicts first :class)
                    conflict (conflict-record id base landed-record prec overlap)
                    alternatives (mapv (fn [{:keys [choice bytes]}]
                                         (alternative-record conflict
                                                             (if (= choice (:op-id landed))
                                                               landed-record prec)
                                                             bytes))
                                       (:alternatives out))]
                (assert! graph-store envelope conflict "conflict")
                (doseq [a alternatives] (assert! graph-store envelope a "alternative"))
                (swap! text-state (fn [s]
                                    (-> s
                                        (assoc-in [:conflicts (:id conflict)]
                                                  {:record conflict :expected current})
                                        (update :alternatives merge
                                                (into {} (map (juxt :id identity) alternatives))))))
                {:ok {:result/status :conflict :artifact/id id :patch/id (:id prec)
                      :conflict/id (:id conflict)
                      :head/revision-id current
                      :alternative/ids (mapv :id alternatives)}})))
          {:error (env/error :conflict :arxana.text/rebase-base-not-parent
                             {:artifact/id id :base/revision-id base
                              :current/revision-id current :patch/id (:id prec)}
                             [:refresh :manual-rebase])})))
    (catch clojure.lang.ExceptionInfo e
      {:error (or (:typed-error (ex-data e))
                  (env/error :invalid :arxana.text/invalid-patch (ex-data e) nil))})))

(defn choose!
  [{:keys [graph-store text-state ingest]} envelope
   body]
  (let [artifact-id (:artifact/id body)
        conflict-id (:conflict/id body)
        alternative-id (:alternative/id body)
        actor (:actor body)
        chosen-at (:chosen-at body)
        reason (:reason body)]
  (cond
    (not (and (string? actor) (seq actor) (re-matches iso-millis chosen-at)))
    {:error (env/error :invalid :arxana.text/choice-attribution
                       {:actor actor :chosen-at chosen-at
                        :required-time "yyyy-MM-ddTHH:mm:ss.SSSZ"} nil)}
    :else
    (let [conflict (get-in @text-state [:conflicts conflict-id])
          alternative (get-in @text-state [:alternatives alternative-id])]
      (cond
        (or (nil? conflict) (nil? alternative))
        {:error (env/error :missing :arxana.text/choice-target
                           {:conflict/id conflict-id :alternative/id alternative-id} nil)}
        (not= conflict-id (get-in alternative [:endpoints 0 :target :clusion/id]))
        {:error (env/error :invalid :arxana.text/alternative-not-in-conflict
                           {:conflict/id conflict-id :alternative/id alternative-id} nil)}
        :else
        (let [expected (:expected conflict)
              result (ingest (:result/text (:asserted-content alternative)) expected)]
          (if-let [err (:error result)]
            {:error err}
            (let [choice (records/make-clusion
                          {:type-revision [:text/choice 1]
                           :endpoints [(endpoint :conflict :clusion conflict-id)
                                       (endpoint :alternative :clusion alternative-id)]
                           :asserted-content {:actor actor :chosen-at chosen-at
                                              :reason (or reason "chosen")}})
                  rid (get-in result [:ok :artifact/revision-id])]
              (assert! graph-store envelope choice "choice")
              (swap! text-state assoc-in [:heads artifact-id] rid)
              {:ok {:result/status :chosen :artifact/id artifact-id :choice/id (:id choice)
                    :head/revision-id rid}}))))))))
