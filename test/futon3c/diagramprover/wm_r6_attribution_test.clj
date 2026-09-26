(ns futon3c.diagramprover.wm-r6-attribution-test
  "Production source attribution with mutations in isolated source trees."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wiring :as w]))

(def root "/home/joe/code")
(def carry-file "futon2/src/futon2/aif/policy_precision_carry.clj")
(def policy-file "futon2/src/futon2/aif/policy.clj")
(def posterior-file "futon2/src/futon2/aif/cascade_selection.clj")
(def wm-file "futon2/scripts/futon2/report/war_machine.clj")
(defn source [file] (slurp (io/file root file)))
(defn boxes [caller-file caller-var callee-file callee-var]
  [{:box/id :caller :site {:file caller-file :var caller-var}}
   {:box/id :callee :site {:file callee-file :var callee-var}}])
(def policy-boxes (boxes policy-file "select-action-cascades" posterior-file "selection-posterior"))
(def carry-boxes (boxes wm-file "cascade-decision-admitted" policy-file "select-action-cascades"))
(def pass {:value :beta :from {:literal-arg-key :beta}
           :to {:call "cascade-selection/selection-posterior" :arg 1 :callee-box :callee}})
(def carry-pass (assoc pass :to {:call "policy/select-action-cascades" :arg 2 :callee-box :callee}))
(defn report [r bs p] (w/pass-attribution r bs :caller p))
(defn mutated [files f]
  (let [r (.toFile (java.nio.file.Files/createTempDirectory "r6-source" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[file text] files]
      (io/make-parents (io/file r file)) (spit (io/file r file) text))
    (f (str r))))

(deftest production-destructured-handoffs
  (doseq [[bs p] [[policy-boxes pass] [carry-boxes carry-pass]]]
    (let [r (report root bs p)]
      (is (:ok? r) (pr-str r))
      (is (= [:beta] (:field-path r)))
      (is (= "beta" (:local r)))
      (is (true? (:used? r))))))

(deftest production-handoff-negative-controls
  (is (= :parameter-field-not-bound
         (:why (report root policy-boxes (assoc pass :value :not-bound)))))
  (is (= :call-has-too-few-arguments
         (:why (report root policy-boxes (assoc-in pass [:to :arg] 2)))))
  (doseq [[body reason] [["'(beta candidates)" :param-unused]
                         [":unused" :param-unused]
                         ["(let [beta 7] beta)" :param-unused]
                         ["(fn [beta] beta)" :param-unused]
                         ["{:keys '[beta candidates]}" :param-unused]]]
    (mutated {policy-file (source policy-file)
              posterior-file (str "(defn selection-posterior [{:keys [beta candidates]}] " body ")")}
             #(is (= reason (:why (report % policy-boxes pass))))))
  ;; This is a data map in the actual caller's callee, not a parameter binding.
  (mutated {policy-file (source policy-file)
            posterior-file "(defn selection-posterior [opts] {:keys '[beta candidates]})"}
           #(is (= :param-unused (:why (report % policy-boxes pass))))))

(deftest renamed-namespaced-and-nested-parameters
  (doseq [[pattern body field path local]
          [["{b :beta}" "b" :beta [:beta] "b"]
           ["{:rate/keys [beta]}" "beta" :rate/beta [:rate/beta] "beta"]
           ["{:keys [rate/beta]}" "beta" :rate/beta [:rate/beta] "beta"]
           ["{{b :beta} :outer}" "b" :outer [:outer :beta] "b"]]]
    (mutated {"caller.clj" (str "(defn caller [] (callee {" field (if (= field :outer) " {:beta 1}" " 1") "}))")
              "callee.clj" (str "(defn callee [" pattern "] " body ")")}
             (fn [r]
               (let [p {:value field :from {:literal-arg-key field}
                        :to {:call "callee" :arg 1 :callee-box :callee :field-path path}}
                     result (report r (boxes "caller.clj" "caller" "callee.clj" "callee") p)]
                 (is (:ok? result) (pr-str result))
                 (is (= local (:local result)))
                 (is (= path (:field-path result))))))))

(deftest production-seal-and-merge-winners
  (let [s (source carry-file)]
    (doseq [field [:beta :gamma :tau]]
      (let [rs (filter :ok? (w/sealed-return-attributions s "advance" field))]
        (is (= 1 (count rs)))
        (is (= 1 (:merge-side (first rs))))
        (is (:preserved? (first rs)))
        (is (= #{:sha256} (get-in (first rs) [:wrapper-effects :overwrites])))))
    (let [r (first (filter :ok? (w/sealed-return-attributions s "advance" :schema)))]
      (is (= 0 (:merge-side r))) (is (= "base" (:binding r))))
    (is (some #(= :merge-source-overwritten (:why %))
              (w/sealed-return-attributions s "advance" :beta {:binding "base"})))
    (is (some #(= :wrapper-overwrites-field (:why %))
              (w/sealed-return-attributions s "advance" :sha256)))))

(deftest production-wrapper-negative-controls
  (let [s (source carry-file)
        seal "(defn seal [value] (assoc value :sha256 (evidence/value-digest value)))"]
    (doseq [replacement ["(defn seal [value] (dissoc value :beta))"
                         "(defn seal [value] (assoc (dissoc value :beta) :temperature (:beta value)))"]]
      (is (some #(= :wrapper-drops-field (:why %))
                (w/sealed-return-attributions (str/replace s seal replacement) "advance" :beta))))
    (is (some #(and (= :unknown-return-wrapper (:why %)) (= "opaque" (:wrapper %)))
              (w/sealed-return-attributions (str/replace s "(seal (merge" "(opaque (merge") "advance" :beta)))))

(deftest sealed-fields-reach-conformance-at-the-production-site
  (let [bs [{:box/id :carry :site {:file carry-file :var "advance"}
             :returns-record :precision :writes [[:beta {:record :precision}]]}]
        usages (w/usage root {:boxes bs})]
    (is (pos? (get-in (first usages) [:usage :writes])))))

(deftest missing-nested-source-does-not-prove-a-binding
  (mutated {"caller.clj" "(defn caller [] (callee {:outer 1}))"
            "callee.clj" "(defn callee [{{b :beta} :outer}] b)"}
           #(is (= :supplied-field-not-proved
                   (:why (report % (boxes "caller.clj" "caller" "callee.clj" "callee")
                                 {:value :beta :from {:literal-arg-key :outer}
                                  :to {:call "callee" :arg 1 :callee-box :callee
                                       :field-path [:outer :beta]}}))))))
