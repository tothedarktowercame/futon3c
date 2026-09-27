(ns futon3c.diagramprover.wm-forward-passes-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring]))

(def inbound
  {:id :inbound :value :mu :from {:literal-arg-key :cascade-belief}
   :to {:call "forward" :arg 1 :callee-box :forward}})
(def onward
  {:value :mu :from {:param 'state :via :source}
   :to {:call "kernel" :arg 1 :callee-box :kernel}})

(defn fixture
  [forward source kernel edit]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "forward-pass-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (doseq [[file text] {"source.clj" source "forward.clj" forward "kernel.clj" kernel}]
        (spit (io/file root file) text))
      (let [boxes (edit [{:box/id :source :site {:file "source.clj" :var "source"}
                         :passes [inbound]}
                        {:box/id :forward :site {:file "forward.clj" :var "forward"}
                         :passes [onward]}
                        {:box/id :kernel :site {:file "kernel.clj" :var "kernel"}}])]
        (wiring/pass-attribution (str root) boxes :forward
                                 (first (:passes (second boxes)))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))

(def source "(defn source [] (forward {:cascade-belief {#{} 1}} 2))")
(def forward "(defn forward [state opts] (kernel state opts))")
(def kernel "(defn kernel [state opts] [(:cascade-belief state) opts])")

(deftest unchanged-parameter-composes-only-the-named-inbound
  (is (:ok? (fixture forward source kernel identity)))
  (is (:ok? (fixture forward source kernel
                    #(assoc-in % [1 :passes 0 :from :via] :inbound))))
  (testing "other callers are not claimed by this continuation"
    (is (:ok? (fixture forward
                      (str source "\n(defn unrelated [] (forward :other 2))")
                      kernel identity))))
  (is (:ok? (fixture "(defn forward [state opts] (let [x 3] (kernel state opts)))"
                    source kernel identity))))

(deftest refused-forwarding-cases
  (doseq [[label f s k edit expected]
          [["rebound" "(defn forward [state opts] (let [state {}] (kernel state opts)))"
            source kernel identity :forward-binding-context]
           ["shadowed" "(defn forward [state opts] ((fn [state] (kernel state opts)) {}))"
            source kernel identity :forward-binding-context]
           ["transformed" "(defn forward [state opts] (kernel (assoc state :x 1) opts))"
            source kernel identity :forward-value-transformed]
           ["no inbound" forward source kernel #(assoc-in % [0 :passes] [])
            :forward-inbound-not-unique]
           ["refused inbound" forward "(defn source [] (forward :unknown 2))" kernel
            identity :forward-inbound-refused]
           ["wrong parameter" forward "(defn source [] (forward {} {:cascade-belief 1}))" kernel
            #(assoc-in % [0 :passes 0 :to :arg] 2) :forward-inbound-parameter-mismatch]
           ["wrong outbound arity" "(defn forward [state opts] (kernel state))"
            source kernel identity :passes-arity-mismatch]
           ["later outbound arity mismatch"
            "(defn forward [state opts] (kernel state opts) (kernel state))"
            source kernel identity :passes-arity-mismatch]
           ["shadowed callee"
            "(defn forward [state opts] (let [kernel vector] (kernel state opts)))"
            source kernel identity :forward-binding-context]
           ["wrong inbound arity"
            "(defn forward ([state] (count state)) ([state opts] (kernel state opts)))"
            "(defn source [] (forward {:cascade-belief 1}))" kernel identity
            :forward-inbound-parameter-mismatch]
           ["destructured and rebuilt"
            "(defn forward [{:keys [cascade-belief]} opts] (kernel {:cascade-belief cascade-belief} opts))"
            source kernel identity :forward-value-transformed]
           ["ambiguous inbound" forward source kernel
            #(update-in % [0 :passes] conj inbound) :forward-inbound-not-unique]
           ["different wire" forward source kernel
            #(assoc-in % [0 :passes 0 :value] :other) :forward-inbound-not-unique]
           ["one valid and one shadowed site"
            "(defn forward [state opts] (kernel state opts) (let [state {}] (kernel state opts)))"
            source kernel identity :forward-binding-context]]]
    (testing label
      (is (= expected (:why (fixture f s k edit)))))))

(deftest circular-inbound-cannot-certify-itself
  (let [p {:id :cycle :value :mu :from {:param 'state :via :cycle}
           :to {:call "forward" :arg 1 :callee-box :forward}}
        r (fixture "(defn forward [state opts] (forward state opts) (kernel state opts))"
                   source kernel #(assoc-in % [1 :passes] [(assoc onward :from {:param 'state :via :cycle}) p]))]
    (is (false? (:ok? r)))
    (is (= :forward-inbound-refused (:why r)))))

(deftest real-decision-through-rank-actions-to-cascade-ranker
  ;; Resource lookup makes these read-only source dependencies visible to the
  ;; registry's recording loader; they are not evaluated or live-loaded.
  (doseq [[resource path] [["futon2/aif/efe.clj" "../futon2/src/futon2/aif/efe.clj"]
                           ["futon2/report/war_machine.clj" "../futon2/scripts/futon2/report/war_machine.clj"]]]
    (is (= (.getCanonicalPath (io/file path))
           (.getCanonicalPath (io/file (io/resource resource)))) resource))
  (let [p (assoc inbound :from {:literal-arg-key :cascade-belief}
                :to {:call "efe/rank-actions" :arg 1 :callee-box :forward})
        q (assoc onward :to {:call "rank-cascade-actions" :arg 1 :callee-box :kernel})
        boxes [{:box/id :source
                :site {:file "../futon2/scripts/futon2/report/war_machine.clj"
                       :var "cascade-decision-admitted"}
                :passes [p]}
               {:box/id :forward
                :site {:file "../futon2/src/futon2/aif/efe.clj" :var "rank-actions"}
                :passes [q]}
               {:box/id :kernel
                :site {:file "../futon2/src/futon2/aif/efe.clj" :var "rank-cascade-actions"}}]]
    (is (:ok? (wiring/pass-attribution "." boxes :source p)))
    (is (:ok? (wiring/pass-attribution "." boxes :forward q)))
    (is (false? (:ok? (wiring/pass-attribution
                       "." boxes :forward (assoc-in q [:to :arg] 2)))))
    (is (= :callee-var-mismatch
           (:why (wiring/pass-attribution "." boxes :source
                    (assoc-in p [:to :callee-box] :kernel)))))))
