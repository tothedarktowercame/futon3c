(ns futon3c.agency.pattern-card-record
  "Validation and lossless hyperedge mapping for pattern-card acts."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp])
  (:import [java.time Instant]))

(def selection-type :pattern-card/selection)
(def withdrawal-type :act/withdrawal)

(defn- refuse! [reason field]
  (throw (ex-info "Invalid pattern-card act" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- act-id? [value]
  (and (text? value) (str/starts-with? value "act:")))

(defn- validate-common
  ([record expected-kind] (validate-common record expected-kind #{:grant :operator}))
  ([record expected-kind allowed-authority-kinds]
  (when-not (map? record) (refuse! :invalid-record :record))
  (when-not (= expected-kind (:kind record))
    (refuse! :wrong-record-kind :kind))
  (when-not (act-id? (:id record)) (refuse! :invalid-act-id :id))
  (when (nil? (:at record)) (refuse! :missing-at :at))
  (try
    (Instant/parse (:at record))
    (catch Exception _ (refuse! :invalid-at :at)))
  (when-not (text? (:author record)) (refuse! :missing-author :author))
  (when (contains? record :act/harness)
    (act-harness/validate! (:act/harness record)))
  ;; Historical records predate P4's act stamp and remain readable. New write
  ;; paths require the stamp before minting.
  (when (contains? record :act/stamp)
    (act-stamp/validate! (:act/stamp record) allowed-authority-kinds))
  record))

(defn validate-selection
  "Return a valid plain selection record or throw a typed ex-info refusal."
  [record]
  (validate-common record selection-type)
  (doseq [field [:agent :session :pattern-id]]
    (when-not (text? (get record field))
      (refuse! :missing-selection-field field)))
  record)

(defn validate-withdrawal
  "Return a valid plain withdrawal record. When TARGET-RECORD is supplied,
   a self withdrawal must be authored by that target's stored author."
  ([record] (validate-withdrawal record nil))
  ([record target-record]
   (when (= :interpretation (:kind record))
     (refuse! :interpretation-not-effect :kind))
   (validate-common record withdrawal-type #{:grant :operator :dispatch-edge})
   (when (and (:reverses record) (not (act-id? (:target record))))
     (refuse! :reversal-missing-target :target))
   (when-not (act-id? (:target record)) (refuse! :missing-target :target))
   (when (and (contains? record :reverses) (some? (:reverses record))
              (not (act-id? (:reverses record))))
     (refuse! :invalid-reverses :reverses))
   (when-not (contains? #{:effective :provisional} (:status record))
     (refuse! :invalid-status :status))
   (when-not (contains? #{:self :grant :provisional-interpretation :dispatch-edge}
                        (get-in record [:basis :kind]))
     (refuse! :invalid-basis :basis))
   (when (and target-record
              (= :self (get-in record [:basis :kind]))
              (not= (:author record) (:author target-record)))
     (refuse! :not-author :author))
   (when (and (contains? record :reason)
              (not (and (string? (:reason record))
                        (not (str/blank? (:reason record)))
                        (<= (count (:reason record)) 4096))))
     (refuse! :invalid-reason :reason))
   record))

(defn record->hyperedge
  "Map a validated selection or withdrawal to its storage hyperedge. The act
   harness remains in :hx/props and :at becomes :hx/valid-time."
  [record]
  (let [validated (case (:kind record)
                    :pattern-card/selection (validate-selection record)
                    :act/withdrawal (validate-withdrawal record)
                    (refuse! :unsupported-record-type :kind))
        ;; futon1b uses :hx/valid-time as transaction metadata and deliberately
        ;; does not return it in LIST documents. Keep :at in props as the
        ;; domain record's queryable valid-time witness as well.
        props (dissoc validated :id :kind)]
    {:hx/id (:id validated)
     :hx/type (:kind validated)
     :hx/valid-time (:at validated)
     :hx/endpoints (case (:kind validated)
                     :pattern-card/selection
                     [(str "agent:" (:agent validated))
                      (str "session:" (:session validated))
                      (str "pattern:" (:pattern-id validated))]
                     :act/withdrawal
                     [(:target validated) (str "agent:" (:author validated))])
     :hx/props props}))

(defn hyperedge->record
  "Map one pattern-card act hyperedge back to its validated plain record."
  [hyperedge]
  (let [props (:hx/props hyperedge)
        record (assoc (dissoc props :at)
                      :id (:hx/id hyperedge)
                      :kind (:hx/type hyperedge)
                      :at (or (:hx/valid-time hyperedge) (:at props)))]
    (case (:kind record)
      :pattern-card/selection (validate-selection record)
      :act/withdrawal (validate-withdrawal record)
      (refuse! :unsupported-record-type :hx/type))))
