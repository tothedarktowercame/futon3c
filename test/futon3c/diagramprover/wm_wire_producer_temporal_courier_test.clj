(ns futon3c.diagramprover.wm-wire-producer-temporal-courier-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-envelope-products-13a :as envelope]
            [futon3c.diagramprover.wm-wire-initialization-hash-products :as initialization]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]
            [futon3c.diagramprover.wm-wire-temporal-previous-products :as previous]
            [futon3c.diagramprover.wm-wire-temporal-run-products-14b :as run-products]
            [futon3c.diagramprover.wm-wire-temporal-storage-products :as storage])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-temporal-courier-test)
(def operation 'futon2.aif.flight/run!)
(def wire-ids
  [[:flight-judge-opts :temporal-inspect [:temporal-previous {:record :flight}]]
   [:r0-enact-step :flight-run :record-path]
   [:temporal-finalize :temporal-envelope [:temporal-cursor {:record :enactment}]]
   [:temporal-finalize :temporal-envelope [:temporal-posterior {:record :enactment}]]
   [:temporal-inspect :r1-token-input [:temporal-previous {:record :temporal-inspection}]]
   [:temporal-inspect :r1-token-temporal [:temporal-previous {:record :temporal-inspection}]]
   [:temporal-write-once :temporal-read-receipt [:digest {:record :temporal-publication}]]
   [:temporal-write-once :temporal-read-receipt [:record-path {:record :temporal-publication}]]
   [:token-belief-stage :r1-token-initialization [:initialization {:record :token-belief-stage}]]
   [:token-belief-stage :token-initialization-observations [:initialization {:record :token-belief-stage}]]])

(defn- stable [value]
  (cond
    (map? value) (into (empty value) (map (fn [[k v]] [k (stable v)])) value)
    (vector? value) (mapv stable value)
    (set? value) (set (map stable value))
    (seq? value) (mapv stable value)
    (and (string? value) (.contains value "/temporal-courier-wire-")) :temporary-path
    :else value))

(defn- primary [wire-id]
  (let [none (courier/observe wire-id :none)]
    {:writer (stable (:writer none))
     :reader (stable (:reader none))
     :product-present? (some? (:product none))
     :received? (w/received? (update-vals (select-keys none [:writer :reader]) stable))
     :interventions
     (into {}
           (for [mode [:absent :different]
                 :let [result (courier/observe wire-id mode)]]
             [mode {:writer-present? (some? (:writer result))
                    :received? (w/received? (update-vals (select-keys result [:writer :reader]) stable))}]))}))

(defn- previous-relations [kind]
  (let [{:keys [carriers products initial replayed]} (previous/products kind)
        [a b bad] products]
    (if (= kind :inspect)
      {:carriers-equal-products? (= carriers (mapv :temporal-previous products))
       :values-differ? (not= (:temporal-previous a) (:temporal-previous b))
       :other-fields-equal? (= (dissoc a :temporal-previous) (dissoc b :temporal-previous)
                                (dissoc bad :temporal-previous))}
      {:replayed-present? (every? some? replayed)
       :replayed-equal-beliefs? (= replayed (mapv :continuation-belief [a b]))
       :beliefs-differ? (not= (:continuation-belief a) (:continuation-belief b))
       :temporal-status? (= :temporal-posterior (:conditioning-status a) (:conditioning-status b))
       :posterior-basis? (= :posterior (:basis a) (:basis b))
       :bad-domain-changed? (= :domain-changed (:conditioning-status bad))
       :bad-initial-basis? (= :declared-initialization (:basis bad))
       :bad-uses-initial? (= initial (:continuation-belief bad))})))

(defn- envelope-relations [field]
  (let [{:keys [original changed a b]} (envelope/products field)]
    (if (= field :temporal-cursor)
      {:other-record-fields-equal? (= (dissoc original field) (dissoc changed field))
       :cursor-a-carried? (= (get original field) (select-keys (:envelope a) [:initial-event-id :consumed-event-ids]))
       :cursor-b-carried? (= (get changed field) (select-keys (:envelope b) [:initial-event-id :consumed-event-ids]))
       :envelopes-differ? (not= (:envelope a) (:envelope b))
       :other-envelope-fields-equal? (= (dissoc (:envelope a) :initial-event-id :consumed-event-ids)
                                         (dissoc (:envelope b) :initial-event-id :consumed-event-ids))
       :verified? (= :verified-temporal-posterior (get-in b [:consumed :reason]))
       :belief-stable? (= (get-in a [:consumed :continuation-belief])
                           (get-in b [:consumed :continuation-belief]))}
      {:other-record-fields-equal? (= (dissoc original field) (dissoc changed field))
       :changed-status-ok? (= :ok (get-in changed [field :status]))
       :a-record-carried? (= (get original field) (get-in a [:envelope :record]))
       :b-record-carried? (= (get changed field) (get-in b [:envelope :record]))
       :a-verified? (= :verified-temporal-posterior (get-in a [:consumed :reason]))
       :b-verified? (= :verified-temporal-posterior (get-in b [:consumed :reason]))
       :a-belief-carried? (= (get-in original [field :posterior]) (get-in a [:consumed :continuation-belief]))
       :b-belief-carried? (= (get-in changed [field :posterior]) (get-in b [:consumed :continuation-belief]))
       :a-envelope-read? (= (:envelope a) (dissoc (:read a) :publication))
       :b-envelope-read? (= (:envelope b) (dissoc (:read b) :publication))
       :digests-differ? (not= (get-in a [:publication :digest]) (get-in b [:publication :digest]))
       :beliefs-differ? (not= (get-in a [:consumed :continuation-belief]) (get-in b [:consumed :continuation-belief]))
       :envelope-shape-stable? (= (dissoc (:envelope a) :record) (dissoc (:envelope b) :record))})))

(defn- storage-relations [kind]
  (let [p (storage/products)]
    (if (= kind :digest)
      (let [{:keys [receipt bad-digest good digest-result]} p]
        {:only-digest-changed? (= (dissoc receipt :digest) (dissoc bad-digest :digest))
         :good-posterior? (= :posterior (:basis good))
         :good-status? (= :ok (get-in good [:record :status]))
         :refused? (= :absent (:status digest-result))
         :reason? (= :temporal-record-digest-mismatch (:reason digest-result))
         :no-record? (nil? (:record digest-result)) :no-basis? (nil? (:basis digest-result))})
      (let [{:keys [receipt missing-path wrong-path good missing-result wrong-result other-result]} p
            refused [missing-result wrong-result other-result]]
        {:only-path-changed? (= (dissoc receipt :record-path) (dissoc missing-path :record-path)
                                (dissoc wrong-path :record-path))
         :good-posterior? (= :posterior (:basis good))
         :missing-reason? (= :temporal-record-unreadable (:reason missing-result))
         :wrong-reason? (= :temporal-record-digest-mismatch (:reason wrong-result))
         :other-reason? (= :no-previous-posterior (:reason other-result))
         :all-refused? (every? #(= :absent (:status %)) refused)
         :no-records? (every? #(nil? (:record %)) refused)
         :no-bases? (every? #(nil? (:basis %)) refused)}))))

(defn- initialization-relations [kind]
  (let [{:keys [stages initial products no-updates token]}
        (initialization/initialization-products kind)
        [a b] products beliefs (mapv :continuation-belief products)]
    {:only-initialization-changed? (= (dissoc (first stages) :initialization)
                                      (dissoc (second stages) :initialization))
     :initial-differs? (not= (first initial) (second initial))
     :has-update? (some #(= :updated (:status %)) (:observation-updates a))
     :updates-stable? (= (:observation-updates a) (:observation-updates b))
     :has-not-updated-token? (some #(and (= token (:token %)) (= :not-updated (:status %)))
                                   (:observation-updates a))
     :beliefs-differ? (not= (first beliefs) (second beliefs))
     :no-update-beliefs-initial? (= initial (mapv :continuation-belief no-updates))
     :no-updates-reported? (every? #(not-any? (fn [u] (= :updated (:status u)))
                                              (:observation-updates %)) no-updates)}))

(defn build-record []
  {:producer producer :operation operation
   :inputs {:wire-ids wire-ids :modes [:none :absent :different]}
   :fields {:live-absent? (courier/live-absent?)
            :wires (into {} (map (juxt identity primary) wire-ids))
            :second-layer
            {(nth wire-ids 0) (previous-relations :inspect)
             (nth wire-ids 1) (let [{:keys [values stored other-products products]}
                                    (run-products/run-products :record-path)]
                                {:values-present? (every? some? values)
                                 :values-differ? (not= (first values) (second values))
                                 :stored-values? (= values stored)
                                 :other-products-stable? (= (first other-products) (second other-products))
                                 :no-progress? (= [:no-progress :no-progress] (mapv :status products))})
             (nth wire-ids 2) (envelope-relations :temporal-cursor)
             (nth wire-ids 3) (envelope-relations :temporal-posterior)
             (nth wire-ids 4) (previous-relations :receipt)
             (nth wire-ids 5) (previous-relations :consume)
             (nth wire-ids 6) (storage-relations :digest)
             (nth wire-ids 7) (storage-relations :path)
             (nth wire-ids 8) (initialization-relations :receipt)
             (nth wire-ids 9) (initialization-relations :observations)}}
   :left-out {:temporary-paths "replaced by :temporary-path; readers assert the recorded writer/reader relation"
              :products "readers check product presence, so the record stores :product-present?"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "temporal-courier@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(deftest temporal-courier-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "temporal-courier@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [[field value] (:fields expected)]
          (testing (name field) (is (= value (get-in actual [:fields field])))))))))
