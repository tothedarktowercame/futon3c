(ns futon3c.diagramprover.sentence-records
  "Source-role sentences and bounded, temporal reverse-index readers. No writes."
  (:require [cheshire.core :as json] [clojure.edn :as edn]
            [futon3c.arxana.records :as records] [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.diagramprover.arxana-records :as ar]))
(def publication-log "/home/joe/storage/arxana/diagramprover/first-write-2026-11-17.edn")
(def q83-id "arxana:clusion:sha256:777d1f59d5a939011126a8bebe9e4422cc9bc5b6d4799fa8451d75eca1c60bec")
(defn- endpoint [role id] {:role role :kind :clusion :target {:clusion/id id}})
(defn- target [r role] (some #(when (= role (:role %)) (get-in % [:target :clusion/id])) (:endpoints r)))
(defn- role-record [l]
  (records/make-clusion {:type-revision [:sentence/role 1] :endpoints []
                        :asserted-content {:module (:module l) :declaration (:declaration l)
                                           :corpus-sha256 (:corpus_sha256 l) :split-expression-key (:split_expr_key l)
                                           :role (keyword (:role l))}}))
(defn- manifestation [role commit sha words]
  (records/make-clusion {:type-revision [:sentence/manifestation 1] :endpoints [(endpoint :role (:id role))]
                        :asserted-content {:renderer-commit commit :corpus-sha256 sha :wordings (vec words) :count (count words)}}))
(defn records-from-switch-off
  "The optional publication value permits hermetic tests; default reads the saved
  publication log, never the live store. Other-change keys are expression keys,
  not established source occurrence locators, so those sentences are omitted."
  ([path] (records-from-switch-off path (edn/read-string (slurp publication-log))))
  ([path publication]
   (let [x (json/parse-string (slurp path) true)
         rows (filter #(and (= "rendered" (:status %)) (#{"a" "b" "c"} (:form %))) (:rows x))
         groups (sort-by key (group-by :qualname rows))
         old (some #(when (= q83-id (get-in % [:record :id])) (:record %)) (get-in publication [:plan :rows]))
         _ (when-not (= q83-id (when old (records/clusion-id old)))
             (throw (ex-info "Published Q83 identity required" {:error :sentence/q83-mismatch})))
         rules (into {} (for [[name rs] groups]
                          [name (if (= "_merge_right_or_splits" (:function (first rs))) old
                                    (ar/rule-record {:name (keyword name)
                                                     :pattern (if (or (some #(= "a" (:form %)) rs)
                                                                      (#{"apply" "_layout_steps" "_apply_case_ownership" "_collapse_finite_checks"}
                                                                       (:function (first rs))))
                                                                [:whole-sequence] [:opaque-function])
                                                     :guards [] :priority 0
                                                     :rewrite-doc {:python/qualname name :git/commit (:head x)}} :apm-printer))]))
         tested (vec (sort (distinct (map :id (vals rules)))))
         locs (mapv :locator (:roles (first rows)))
         roles (mapv role-record locs)
         manifestations (mapv (fn [role r] (manifestation role (:head x) (:corpus_sha256 x) (mapv :wording (:head r))))
                              roles (:roles (first rows)))
         rank {"Q55" 0 "Q56" 1 "Q56-review" 2 "Q61" 3 "Q66" 4 "Q78" 5 "Q83" 6}
         apps (vec (for [[name rs] groups [idx role] (map-indexed vector roles)
                         :when (some #(get-in % [:roles idx :changed]) rs)
                         :let [interventions (mapv (fn [r]
                                                     {:unit (if (= "50444f21" (:commit r)) "Q56-review" (:unit r))
                                                      :form ({"a" :pass "b" :added "c" :parent-body} (:form r))
                                                      :head-wordings (mapv :wording (get-in r [:roles idx :head]))
                                                      :switched-wordings (mapv :wording (get-in r [:roles idx :switched]))}) rs)]]
                     (records/make-clusion
                      {:type-revision [:rule/application 1]
                       :endpoints [(endpoint :sentence (:id role)) (endpoint :rule (:id (rules name)))]
                       :asserted-content {:method :switch-off :renderer-commit (:head x) :corpus-sha256 (:corpus_sha256 x)
                                          :tested-rules tested :interventions (vec (sort-by #(rank (:unit %)) interventions))}})))]
     {:roles roles :manifestations manifestations :rules rules :applications apps
      :records (vec (concat (vals rules) roles manifestations apps))
      :omitted-other-reason :no-established-source-role-locator})))
(defn scoped-manifestation
  "Build revision 2. Registry declarations select identity fields but do not
  validate content enums, so scope is checked here. Commit time is UTC millis."
  [role commit commit-time sha words scope]
  (when-not (#{:role :declaration} scope)
    (throw (ex-info "Invalid manifestation scope" {:error :sentence/invalid-scope :scope scope})))
  (let [time (.format (.toFormatter (.appendInstant (java.time.format.DateTimeFormatterBuilder.) 3))
                     (.truncatedTo (java.time.Instant/parse commit-time) java.time.temporal.ChronoUnit/MILLIS))]
    (records/make-clusion
     {:type-revision [:sentence/manifestation 2] :endpoints [(endpoint :role (:id role))]
      :asserted-content {:renderer-commit commit :renderer-commit-time time
                         :corpus-sha256 sha :wordings (vec words) :count (count words) :scope scope}})))
(defn history-records
  "Untyped historical counts are explicitly declaration-scoped revision 2.
  Explicit outer wordings retain the exact revision-1 record and event-time."
  [path]
  (mapv (fn [r]
          (let [typed (:outer_typed_closer_wordings r)
                words (if (seq typed) typed
                          (mapcat (fn [[word n]] (repeat n (name word))) (sort-by key (:case_closer_wordings r))))
                role (role-record (:outer_locator r)) sha (:corpus_sha256 (:outer_locator r))]
            (assoc (if (seq typed) (manifestation role (:commit r) sha words)
                       (scoped-manifestation role (:commit r) (:time r) sha words :declaration))
                   :event-time (:time r))))
        (:records (json/parse-string (slurp path) true))))
(defn- missing? [r] (= :arxana.store/record-not-found (get-in r [:error :error/code])))
(defn- visible [s id at] (http/get-entity-as-of s id at))
(defn- reverse-records [s type role id at]
  (let [q {:query/type :role-reverse :edge-type type :endpoint-role role :endpoint-id id}
        {:keys [ok error] :as response} (store/bounded-query! s q)]
    (cond error response
          (= :stale-index (:status ok)) {:status :inconclusive :reason :stale-index}
          (:truncated? ok) {:status :inconclusive :reason :truncated}
          (not= :ok (:status ok)) {:error {:error/code :sentence/query-response :response response}}
          :else (loop [ids (:edge-ids ok) rs []]
                  (if-let [id (first ids)]
                    (let [r (visible s id at)]
                      (cond (missing? r) (recur (rest ids) rs)
                            (:error r) r
                            :else (recur (rest ids) (conj rs (:ok r)))))
                    {:status :ok :records rs})))))
(defn sentence-view [s role-id at]
  (let [role (visible s role-id at)]
    (cond (missing? role) {:status :missing :id role-id}
          (:error role) role
          :else
          (let [apps (reverse-records s [:rule/application 1] :sentence role-id at)]
            (if (not= :ok (:status apps)) apps
                (loop [rules (distinct (map #(target % :rule) (:records apps))) because []]
                  (if-let [r (first rules)]
                    (let [sup (reverse-records s [:supersedes 1] :predecessor r at)]
                      (if (not= :ok (:status sup)) sup
                          (recur (rest rules) (into because (map :id (:records sup))))))
                    {:status :ok :stale? (boolean (seq because)) :stale-because (vec (sort (distinct because)))})))))))
(defn rule-fibre [s rule-id at]
  (let [apps (reverse-records s [:rule/application 1] :rule rule-id at)]
    (if (not= :ok (:status apps)) apps
        (loop [ids (sort (distinct (map #(target % :sentence) (:records apps)))) out []]
          (if-let [id (first ids)]
            (let [r (visible s id at)]
              (cond (missing? r) (recur (rest ids) out) (:error r) r
                    :else (recur (rest ids) (conj out id))))
            {:status :ok :roles out})))))

(defn role-manifestations
  "Read both revision indexes completely before returning scoped records. The
  stored revision-1 bytes are preserved; scope is carried alongside each record."
  [s role-id at]
  (loop [revisions [1 2] out []]
    (if-let [rev (first revisions)]
      (let [r (reverse-records s [:sentence/manifestation rev] :role role-id at)]
        (if (not= :ok (:status r)) r
            (recur (rest revisions)
                   (into out (map #(hash-map :record % :scope (if (= rev 1) :role (get-in % [:asserted-content :scope])))
                                  (:records r))))))
      {:status :ok :manifestations (vec (sort-by #(get-in % [:record :id]) out))})))
