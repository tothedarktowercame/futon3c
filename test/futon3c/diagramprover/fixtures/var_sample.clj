(ns futon3c.diagramprover.fixtures.var-sample)

(defn ^:private reads-g-terms [m] (get m :g-terms))

(defn writes-measurement [m] (assoc m :measurement 1))

(def a-map-literal {:g 1 :order [2]})

(defn comment-only [m]
  ;; :comment-field is mentioned here and never used
  m)

(defn string-with-parens [] "a ( string with :g and an escaped \" quote )")

(defn after-the-string [m] (:selected (get-in m [:decision :selection-law])))

(defn destructures [{:keys [universe] :as m}] [(update-in m [:field :path] inc) universe])

(def uses-the-private-fn reads-g-terms)
