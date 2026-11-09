(ns futon3c.arxana.sessionista-introductions
  "Materialise Joe's [[entity | referent]] introductions through public ports."
  (:require [clojure.string :as str]
            [futon3c.arxana.records :as records]))

(defn slugify [s]
  (-> s str/trim str/lower-case (str/replace #"[^\p{L}\p{N}]+" "-")
      (str/replace #"(^-|-$)" "")))

(defn parse-introductions [^String text]
  (let [m (re-matcher #"\[\[([^\]|]+?)(?:\|([^\]]+))?\]\]" text)]
    (loop [out []]
      (if (.find m)
        (recur (conj out {:as-written (str/trim (.group m 1))
                          :referents (if-let [r (.group m 2)]
                                       (mapv str/trim (str/split r #",")) [])
                          :codepoint-range [(.codePointCount text 0 (.start m))
                                            (.codePointCount text 0 (.end m))]}))
        out))))

(defn- entity [slug]
  (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                         :asserted-content {:entity/slug slug}}))
(defn- catalog []
  (records/make-clusion {:type-revision [:workplan/catalog 1] :endpoints []
                         :asserted-content {:catalog/id "entity-match-rules"}}))
(defn- rule [e]
  (records/make-clusion {:type-revision [:workplan/entity-match-rule 1]
                         :endpoints [{:role :entity :kind :clusion
                                     :target {:clusion/id (:id e)}}]
                         :asserted-content {:rule/id (str "entity-forms/" (get-in e [:asserted-content :entity/slug]))}}))
(defn- member [r]
  (records/make-clusion {:type-revision [:workplan/entity-rule-member 1]
                         :endpoints [{:role :catalog :kind :clusion :target {:clusion/id (:id (catalog))}}
                                     {:role :rule :kind :clusion :target {:clusion/id (:id r)}}]
                         :asserted-content {}}))
(defn- write! [dispatch! run-id op record]
  (dispatch! {:protocol/version "arxana.v1" :message/id (str run-id ":" (name op) ":" (:id record))
              :operation op :principal/id "sessionista/introduction" :body {:record record}}))
(defn- exists? [read! e]
  (boolean (:ok (read! :entity/get {:entity/id (:id e)}))))

(defn materialise!
  [dispatch! read! {:keys [evidence/id text actor time candidate/slugs]}]
  (let [run-id (str "sessionista:" id)
        parsed (parse-introductions text)
        results
        (mapv
         (fn [{:keys [as-written referents codepoint-range]}]
           (let [slug (slugify as-written) e (entity slug) r (rule e)
                 source {:ref/store :evidence :ref/id id}
                 intro (records/make-clusion
                        {:type-revision [:workplan/introduction 1]
                         :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id e)}}
                                     {:role :source :kind :external-ref :target source}]
                         :asserted-content {}})
                 intro-att (records/make-attestation
                            {:subject-clusion-id (:id intro) :producer actor :basis :human
                             :method {:id "sessionista/bracket-introduction" :version 1}
                             :standing :verified :coverage :complete
                             :content {:evidence/id id :actor actor :time time :as-written as-written
                                       :codepoint-range codepoint-range}})
                 explicit (mapv (fn [x] [x (entity (slugify x))]) referents)
                 resolved (filterv #(exists? read! (second %)) explicit)
                 candidates (->> slugs (remove #{slug}) distinct (mapv (fn [s] [s (entity s)])))
                 refs (concat (map (fn [[x target]] [:explicit x target]) resolved)
                              (map (fn [[x target]] [:candidate x target]) candidates))
                 ref-records (mapv (fn [[role _ target]]
                                     (records/make-clusion
                                      {:type-revision [:workplan/referent 1]
                                       :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id e)}}
                                                   {:role :target :kind :clusion :target {:clusion/id (:id target)}}]
                                       :asserted-content {:referent/role role}})) refs)
                 rule-att (records/make-attestation
                           {:subject-clusion-id (:id r) :producer actor :basis :human
                            :method {:id "sessionista/introduction-form" :version 1}
                            :standing :verified :coverage :complete
                            :content {:entity/slug slug :forms [{:form/text as-written
                                                                :enabled? (boolean (seq resolved))}]
                                      :rule/revision 1}})]
             (doseq [x [(catalog) e r (member r) intro]] (write! dispatch! run-id :clusion/assert x))
             (write! dispatch! run-id :evidence/append intro-att)
             (write! dispatch! run-id :evidence/append rule-att)
             (doseq [[rr [role x _]] (map vector ref-records refs)]
               (write! dispatch! run-id :clusion/assert rr)
               (write! dispatch! run-id :evidence/append
                       (records/make-attestation
                        {:subject-clusion-id (:id rr) :producer actor
                         :basis (if (= role :explicit) :human :parser)
                         :method {:id "sessionista/same-turn-referent" :version 1}
                         :standing (if (= role :explicit) :verified :unverified) :coverage :complete
                         :content {:evidence/id id :as-written x :time time}})))
             {:entity/slug slug :entity/id (:id e) :introduction/id (:id intro)
              :rule/id (:id r) :enabled? (boolean (seq resolved))
              :unresolved (mapv first (remove #(exists? read! (second %)) explicit))
              :referent/ids (mapv :id ref-records)})) parsed)]
    {:introductions results :unresolved (vec (mapcat :unresolved results))
     :record/ids (vec (mapcat (juxt :entity/id :introduction/id :rule/id) results))}))
