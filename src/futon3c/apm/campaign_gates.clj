(ns futon3c.apm.campaign-gates
  "Data-driven qualification gates for steppable and autonomous campaigns.")

(defn- check-result [facts {:keys [fact/path check expected] :as requirement}]
  (let [actual (get-in facts path)
        pass? (case check
                :equals (= expected actual)
                :at-least (and (number? actual) (number? expected)
                               (<= expected actual))
                :present (some? actual)
                :nonempty (and (coll? actual) (seq actual))
                :all-true (and (map? actual) (seq actual)
                               (every? true? (vals actual)))
                false)]
    {:requirement/id (:requirement/id requirement)
     :fact/path path :check check :expected expected
     :actual actual :pass? (boolean pass?)}))

(defn evaluate
  "Evaluate declarative gate SPECS against FACTS.

  Every gate returns evidence for every requirement; missing facts fail closed."
  [specs facts]
  (mapv (fn [{:gate/keys [id requirements]}]
          (let [checks (mapv #(check-result facts %) requirements)]
            {:gate/id id
             :gate/status (if (every? :pass? checks) :pass :fail)
             :gate/evidence {:requirements checks}}))
        specs))
