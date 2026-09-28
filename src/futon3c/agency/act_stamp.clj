(ns futon3c.agency.act-stamp
  "Closed identity and authority stamps for minted acts."
  (:require [clojure.string :as str]))

(def allowed-keys #{:executor :signer :authority :executor-basis})
(def executor-bases #{:declared :session-bound})

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- fail! [reason field]
  (throw (ex-info "Invalid act stamp" {:reason reason :field field})))

(defn- authority-kind [authority]
  (cond
    (and (map? authority) (contains? authority :interpretation))
    :interpretation

    (and (map? authority)
         (= #{:grant} (set (keys authority))))
    :grant

    (and (map? authority)
         (= #{:dispatch-edge} (set (keys authority))))
    :dispatch-edge

    (and (map? authority)
         (= {:operator true} authority))
    :operator

    :else :invalid))

(defn validate!
  "Return STAMP, or throw ex-info with a typed :reason.

  The closed shape is {:executor ID :signer ID :authority AUTH
  :executor-basis (:declared | :session-bound)}. AUTH is exactly a grant act
  reference, a dispatch-edge evidence reference, or {:operator true}."
  [stamp]
  (when-not (map? stamp)
    (fail! :invalid-stamp-map :act/stamp))
  (when-let [key (first (remove allowed-keys (keys stamp)))]
    (fail! :unexpected-stamp-key key))
  (when-let [field (first (remove #(contains? stamp %) allowed-keys))]
    (fail! :missing-stamp-field field))
  (when-let [field (first (filter #(and (contains? stamp %)
                                        (nil? (get stamp %)))
                                  allowed-keys))]
    (fail! :missing-stamp-field field))
  (when-let [field (first (filter #(and (contains? #{:executor :signer} %)
                                        (not (text? (get stamp %))))
                                  allowed-keys))]
    (fail! :blank-stamp-field field))
  (when-not (contains? executor-bases (:executor-basis stamp))
    (fail! :unknown-executor-basis :executor-basis))
  (let [authority (:authority stamp)
        kind (authority-kind authority)]
    (case kind
      :interpretation (fail! :interpretation-not-authority :authority)
      :invalid (fail! :invalid-authority :authority)
      :operator (when-not (= "joe" (:signer stamp))
                  (fail! :operator-authority-requires-joe :signer))
      :grant (when-not (and (text? (:grant authority))
                            (re-matches #"act:.+" (:grant authority)))
               (fail! :invalid-grant-id :authority))
      :dispatch-edge
      (when-not (text? (:dispatch-edge authority))
        (fail! :invalid-dispatch-edge-id :authority)))
    (when (and (not= (:executor stamp) (:signer stamp))
               (not= :grant kind))
      (fail! :overreach-without-grant :authority)))
  stamp)

(defn stamp
  "Build and validate an act stamp."
  [executor signer authority basis]
  (validate! {:executor executor
              :signer signer
              :authority authority
              :executor-basis basis}))
