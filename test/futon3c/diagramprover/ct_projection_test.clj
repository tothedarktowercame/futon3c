(ns futon3c.diagramprover.ct-projection-test
  "The machine's map (holes/labs/M-wm-wiring/wm-flight-wiring.edn, read at
  futon3c HEAD) projected into futon5's ct/mission form and validated by
  futon5's own validators. futon5 is not on the :test classpath, so
  src/futon5/ct/mission.clj is read from futon5's git at a pinned sha and
  loaded into this process (read-only; it requires only clojure.set)."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.ct-projection :as proj]))

(def fixture
  (edn/read-string (slurp "test/futon3c/diagramprover/fixtures/wm-flight-ct-projection.edn")))

(defn- git-show [repo rev path]
  (let [{:keys [exit out err]} (sh/sh "git" "-C" repo "show" (str rev ":" path))]
    (when-not (zero? exit) (throw (ex-info "git show failed" {:repo repo :rev rev :path path :err err})))
    out))

(def ^:private ct
  (delay
    (load-string (git-show "/home/joe/code/futon5" (:futon5-sha fixture) "src/futon5/ct/mission.clj"))
    {:diagram (resolve 'futon5.ct.mission/mission-diagram)
     :validate (resolve 'futon5.ct.mission/validate)}))

(defn- head-map []
  (edn/read-string (git-show "." "HEAD" (:map-path fixture))))

(defn- validated [m]
  (let [{:keys [diagram validate]} @ct
        d (diagram (proj/project m))]
    {:d d :v (validate d)}))

(defn- check [v k] (first (filter #(= k (:check %)) (:checks v))))

(deftest projection-is-deterministic
  (let [m (head-map)]
    (is (= (proj/project m) (proj/project m)))
    (is (= (proj/i4-report m {}) (proj/i4-report m {})))))

(deftest the-validators-give-the-pinned-results
  ;; a map change that changes any validator's result fails here
  (let [{:keys [d v]} (validated (head-map))]
    (is (= (:counts fixture)
           {:inputs (count (get-in d [:ports :input])) :outputs (count (get-in d [:ports :output]))
            :components (count (:components d)) :edges (count (:edges d))}))
    (is (= (:validate fixture) v))
    (testing "the four asked for"
      (is (:valid (check v :type-safety)) "vacuous: no edge carries a :type")
      (is (not-any? :type (:edges d)) "why: the map's fields carry no types")
      (is (= #{:r2-served-by-reading}
             (set (map (comp :from :edge) (:violations (check v :timescale-ordering))))))
      (is (:valid (check v :exogeneity)))
      (is (:valid (check v :compositional-closure))))))

(deftest i4-answer-is-pinned
  (is (= (:i4 fixture) (proj/i4-report (head-map) {}))))

(deftest i4-no-bypass-into-preferences
  (let [r (proj/i4-report (head-map) {})]
    (is (= [] (get-in r [:bypass :to-preferences])))
    (is (= [] (get-in r [:bypass :to-preferences-positional])))
    (is (= [[:r2-served-by-reading] [:r2-served-by-reading]] (map :observed-by (:preference-fields r))))))

(deftest i4-habit-path-bypasses-the-checker
  ;; the enactment reaches the outer cascade's :enactment-records through
  ;; :attempts -> increment -> fold, not through :wc-checker
  (is (some #(= [:r0-enact-step :r7-increment :r7-fold] (:path %))
            (get-in (proj/i4-report (head-map) {}) [:bypass :to-scored-facts]))))

(deftest i4-self-caused-lineage-shows-once-declared
  ;; Bad case the map cannot show today: clock-in declares no field, and the
  ;; outer cascade reads no lineage. Declare both and the path appears, with
  ;; the positional dispatch hop flight-entry -> clock-in from :traces.
  (let [m (head-map)
        declared (update m :boxes
                         (fn [bs] (mapv #(case (:box/id %)
                                           :clock-in (assoc % :writes [:lineage])
                                           :r1-outer-cascade (update % :reads conj :lineage)
                                           %)
                                        bs)))
        paths (get-in (proj/i4-report declared {}) [:bypass :to-scored-facts-positional])]
    (is (not-any? #(= :lineage (:to %))
                  (get-in (proj/i4-report m {}) [:bypass :to-scored-facts-positional]))
        "absent from the map as committed")
    (is (some #(= {:to :lineage :path [:flight-entry :clock-in] :via [:positional]}
                  (select-keys % [:to :path :via]))
              paths))
    (is (some #(= [:r1-outer-cascade :flight-entry :clock-in] (:path %)) paths)
        "the loop: the outer cascade's choice dispatches, dispatch writes lineage")
    (testing "the validator's I4 does not see it: ct/mission outputs are sinks"
      (is (:valid (check (:v (validated declared)) :exogeneity))))))

(deftest a-preference-written-by-a-non-observation-box-is-a-bypass
  (let [m (update (head-map) :boxes conj
                  {:box/id :planted-writer :box/kind :component
                   :reads [:chosen-target] :writes [:want-span]})
        r (proj/i4-report m {})]
    (is (some #(= [:r1-outer-cascade :planted-writer] (:path %))
              (get-in r [:bypass :to-preferences])))))
