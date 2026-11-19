(ns futon3c.diagramprover.answer-diagram
  "Turn an answer-holes/v1 answer into an open RM hypergraph."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.rmgraph :as rm]))

(defn- add-v [state payload key]
  (let [[graph id] (rm/add-vertex (:graph state) payload)]
    (-> state (assoc :graph graph) (assoc-in [:vertices key] id))))

(defn- add-e [state sources targets payload]
  (let [[graph id] (rm/add-edge (:graph state) sources targets payload)]
    (-> state (assoc :graph graph) (update :edges assoc (:id payload) id))))

(defn answer->diagram
  "Build an open graph. Free and offered holes are boundary inputs; question
  parts are boundary outputs. The returned indexes keep contract ids visible."
  [answer]
  (let [frames (mapcat :frames (:sentences answer))
        holes (mapcat :holes frames)
        initial {:graph (rm/make-graph) :vertices {} :edges {}}
        with-holes
        (reduce (fn [s h]
                  (let [hid (:id h)
                        s (add-v s {:vtype :hole :value (select-keys h [:id :role :question :status])}
                                 [:hole hid])
                        hv (get-in s [:vertices [:hole hid]])]
                    (case (:status h)
                      :own (let [s (add-v s {:vtype :binding :value (:binding h)} [:binding hid])]
                             (add-e s [(get-in s [:vertices [:binding hid]])] [hv]
                                    {:id [:bind hid] :kind :binding}))
                      :unknown (let [s (add-v s {:vtype :unknown :value hid} [:unknown hid])]
                                 (add-e s [(get-in s [:vertices [:unknown hid]])] [hv]
                                        {:id [:unknown hid] :kind :unknown}))
                      s))) initial holes)
        with-frames
        (reduce (fn [s frame]
                  (let [fid (:id frame)
                        s (add-v s {:vtype :claim :value fid} [:claim fid])]
                    (add-e s
                           (mapv #(get-in s [:vertices [:hole (:id %)]])
                                 (remove #(= :dissolved (:status %)) (:holes frame)))
                           [(get-in s [:vertices [:claim fid]])]
                           {:id [:frame fid] :kind :frame :frame fid})))
                with-holes frames)
        with-parts
        (reduce (fn [s part]
                  (add-v s {:vtype :question-part :value (select-keys part [:id :label])}
                         [:part (:id part)]))
                with-frames (:question-parts answer))
        with-fills
        (reduce (fn [s fill]
                  (let [pid (:part fill)
                        claims (keep #(get-in s [:vertices [:claim %]]) (:frames fill))]
                    (if (seq claims)
                      (add-e s (vec claims) [(get-in s [:vertices [:part pid]])]
                             {:id [:fill pid] :kind :fill :part pid})
                      s))) with-parts (:fills answer))
        inputs (mapv #(get-in with-fills [:vertices [:hole (:id %)]])
                     (filter #(#{:free :offered} (:status %)) holes))
        outputs (mapv #(get-in with-fills [:vertices [:part (:id %)]]) (:question-parts answer))]
    (update with-fills :graph #(-> % (rm/set-inputs inputs) (rm/set-outputs outputs)))))

(defn findings
  "Report open required holes, unreached question parts, and frame boxes that
  contribute to no part. Maps follow DiagramProver's `:finding` convention."
  [answer]
  (let [frames (mapcat :frames (:sentences answer))
        used (set (mapcat :frames (:fills answer)))
        filled (set (keep #(when (seq (:frames %)) (:part %)) (:fills answer)))]
    (vec
     (concat
      (for [h (mapcat :holes frames) :when (= :free (:status h))]
        {:finding :read-never-written :hole (:id h) :question (:question h)})
      (for [p (:question-parts answer) :when (not (contains? filled (:id p)))]
        {:finding :part-never-reached :part (:id p)})
      (for [f frames :when (not (contains? used (:id f)))]
        {:finding :box-reaches-no-part :frame (:id f)})))))

(defn render
  "Terminal-readable rendering of an answer diagram."
  [answer]
  (let [frames (mapcat :frames (:sentences answer))]
    (str/join
     "\n"
     (concat
      [(str "answer " (:slot answer))]
      (for [f frames]
        (str "[" (:id f) "] " (get-in f [:predicate :lemma])
             " (" (str/join ", "
                              (for [h (:holes f)]
                                (str (:id h) ":"
                                     (case (:status h)
                                       :free "OPEN"
                                       :offered "OFFERED"
                                       :own "wired"
                                       :unknown "unknown"
                                       :dissolved "dissolved")))) ")"))
      (for [fill (:fills answer)]
        (str (str/join "+" (:frames fill)) " -> " (:part fill)))
      (for [p (:question-parts answer)
            :when (not-any? #(and (= (:id p) (:part %)) (seq (:frames %))) (:fills answer))]
        (str "OPEN OUTPUT " (:id p)))))))
