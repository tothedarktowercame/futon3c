(ns futon3c.agency.blocker-escalation-record
  "Pure schema and hyperedge mapping for pattern-first blocker escalations.

   The record says that an assignee tried one recorded pattern and remained
   blocked.  It contains no requested work; routing is a later transport act."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp])
  (:import [java.time Instant]))

(def record-type :escalation/blocker)
(def schema-version 1)
(def prose-limit 4096)

(def ^:private record-keys
  #{:id :kind :schema :source-job :author :orchestrator :blocker :at
    :psr-id :pur-id :pattern-id :act/stamp :act/harness})

(defn- refuse! [reason field]
  (throw (ex-info "Invalid blocker escalation" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- parse-at! [value]
  (try (Instant/parse value)
       (catch Exception _ (refuse! :invalid-at :at))))

(defn validate!
  "Return RECORD or throw typed ex-info. The key set is closed, which also
   refuses any field that asks the orchestrator to perform new work."
  [record]
  (when-not (map? record) (refuse! :invalid-record :record))
  (when-let [key (first (remove record-keys (keys record)))]
    (refuse! :unexpected-key key))
  (when-let [field (first (remove #(contains? record %) record-keys))]
    (refuse! :missing-field field))
  (when-not (and (text? (:id record)) (str/starts-with? (:id record) "act:"))
    (refuse! :invalid-act-id :id))
  (when-not (= record-type (:kind record)) (refuse! :wrong-record-kind :kind))
  (when-not (= schema-version (:schema record))
    (refuse! :unsupported-schema :schema))
  (doseq [field [:source-job :author :orchestrator :psr-id :pur-id :pattern-id]]
    (when-not (text? (get record field)) (refuse! :missing-field field)))
  (when-not (str/starts-with? (:source-job record) "invoke-")
    (refuse! :invalid-source-job :source-job))
  (when-not (and (text? (:blocker record))
                 (<= (count (:blocker record)) prose-limit))
    (refuse! :invalid-blocker :blocker))
  (parse-at! (:at record))
  (act-stamp/validate! (:act/stamp record) #{:dispatch-edge})
  (when-not (= (:author record) (get-in record [:act/stamp :signer]))
    (refuse! :not-the-assignee :act/stamp))
  (act-harness/validate! (:act/harness record))
  record)

(defn ->hyperedge [record]
  (let [record (validate! record)]
    {:hx/id (:id record)
     :hx/type record-type
     :hx/valid-time (:at record)
     :hx/endpoints [(str "job:" (:source-job record))
                    (str "agent:" (:author record))
                    (str "agent:" (:orchestrator record))
                    (str "pattern:" (:pattern-id record))
                    (:psr-id record) (:pur-id record)]
     :hx/props (dissoc record :id :kind)}))

(defn hyperedge->record [edge]
  (when-not (= record-type (:hx/type edge))
    (refuse! :wrong-record-kind :hx/type))
  (validate! (assoc (:hx/props edge)
                    :id (:hx/id edge) :kind record-type
                    :at (or (:hx/valid-time edge) (get-in edge [:hx/props :at])))))
