(ns futon3c.agency.agreement-record-cli
  "Minted, verified write path for operator agreements."
  (:require [clojure.string :as str]
            [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder] [java.time Instant]))

(def mint-id "act:pending-mint")
(defn- refuse! [reason field] (throw (ex-info "Invalid agreement write" {:reason reason :field field})))
(defn- text? [v] (and (string? v) (not (str/blank? v))))

(defn payload [{:keys [record idempotency-key]} offer]
  (when-not (text? idempotency-key) (refuse! :invalid-idempotency-key :idempotency-key))
  (when (or (:id record) (:act/stamp record) (:act/harness record))
    (refuse! :caller-assigned-storage-field :record))
  (let [draft (assoc record :id mint-id :act/stamp agreement/operator-stamp
                     :act/harness {:kind :none :basis :producer-context
                                   :source-ref "route:futon3c.agreement"})]
    (agreement/validate-against-offer! draft offer)
    (-> (agreement/record->hyperedge draft)
        (dissoc :hx/id)
        (assoc :hx/mint-id true :hx/idempotency-key idempotency-key))))

(defn- list-path [offer-id system-as-of valid-as-of]
  (str "/api/alpha/hyperedges?type=agreement%2Frecord&end="
       (URLEncoder/encode offer-id "UTF-8") "&limit=1000&include-total=false&system-as-of="
       (URLEncoder/encode system-as-of "UTF-8") "&valid-as-of="
       (URLEncoder/encode valid-as-of "UTF-8")))

(defn write! [base request offer]
  (let [p (payload request offer)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" p)
        id (:hx/id receipt)]
    (when-not (and (:ok receipt) (text? id) (str/starts-with? id "act:"))
      (refuse! :missing-minted-receipt :receipt))
    (let [system-as-of (str (Instant/now))
          edges (:hyperedges (store/request! base "GET"
                                             (list-path (:id offer) system-as-of
                                                        (get-in request [:record :agreement/at])) nil))
          stored (some #(when (= id (:hx/id %)) (agreement/hyperedge->record %)) edges)]
      (when-not stored (refuse! :readback-missing :receipt))
      (when-not (= (dissoc p :hx/mint-id :hx/idempotency-key)
                   (-> (agreement/record->hyperedge stored) (dissoc :hx/id)))
        (refuse! :readback-mismatch :receipt))
      {:receipt (assoc receipt :verified? true :system-as-of system-as-of)
       :record stored})))
