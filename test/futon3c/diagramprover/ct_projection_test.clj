(ns futon3c.diagramprover.ct-projection-test
  "The machine's map (holes/labs/M-wm-wiring/wm-flight-wiring.edn, read at
  futon3c HEAD) projected into futon5's ct/mission form and validated by
  futon5's own validators. futon5 is not on the :test classpath, so
  src/futon5/ct/mission.clj is read from futon5's git at a pinned sha and
  loaded into this process (read-only; it requires only clojure.set)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
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
      (is (:valid (check v :timescale-ordering))
          "the constraint is :owner-text (the map's :field-roles), which no box writes")
      (is (:valid (check v :exogeneity)))
      (is (:valid (check v :compositional-closure))))))

(deftest i4-answer-is-pinned
  (is (= (:i4 fixture) (proj/i4-report (head-map) {}))))

(deftest i4-no-bypass-into-preferences
  (let [r (proj/i4-report (head-map) {})]
    (is (= [] (get-in r [:bypass :to-preferences])))
    (is (= [] (get-in r [:bypass :to-preferences-positional])))
    (is (= [{:field :owner-text :writers [] :observed-by []}] (:preference-fields r))
        "the owner's text is exogenous by declaration")))

(deftest i4-habit-path-bypasses-the-checker
  ;; the enactment reaches the outer cascade's :enactment-records through
  ;; :attempts -> increment -> fold, not through :wc-checker
  (is (some #(= [:r0-enact-step :r7-increment :r7-fold] (:path %))
            (get-in (proj/i4-report (head-map) {}) [:bypass :to-scored-facts]))))

(deftest i4-self-caused-lineage-is-on-the-map
  ;; Declared since the map's 6305395c: :dispatch writes :mission-id, clock-in
  ;; reads it and writes :clock-lineage, the outer cascade reads that. No
  ;; observation box interrupts it: the self-caused path I4 is asked about.
  (let [m (head-map)
        r (proj/i4-report m {})]
    (is (some #(= {:to :clock-lineage :path [:dispatch :clock-in] :via [:mission-id]}
                  (select-keys % [:to :path :via]))
              (get-in r [:bypass :to-scored-facts]))
        "field edges only")
    (is (some #(= [:r1-outer-cascade :flight-entry :dispatch :clock-in] (:path %))
              (get-in r [:bypass :to-scored-facts-positional]))
        "with the trace's hops: the outer cascade's choice dispatches, dispatch writes lineage")
    (testing "falsifier: the outer cascade not reading lineage removes the path"
      (let [unread (update m :boxes (fn [bs] (mapv #(if (= :r1-outer-cascade (:box/id %))
                                                        (update % :reads (fn [rs] (vec (remove #{:clock-lineage} rs))))
                                                        %)
                                                     bs)))]
        (is (not-any? #(= :clock-lineage (:to %))
                      (get-in (proj/i4-report unread {}) [:bypass :to-scored-facts-positional])))))
    (testing "the validator's I4 does not see it: ct/mission outputs are sinks"
      (is (:valid (check (:v (validated m)) :exogeneity))))))

(deftest a-preference-written-by-a-non-observation-box-is-a-bypass
  (let [m (update (head-map) :boxes conj
                  {:box/id :planted-writer :box/kind :component
                   :reads [:chosen-target] :writes [:owner-text]})
        r (proj/i4-report m {})]
    (is (some #(= [:r1-outer-cascade :planted-writer] (:path %))
              (get-in r [:bypass :to-preferences])))))

;; ---------------------------------------------------------------------------
;; :field-roles (claude-10's request, 2026-09-25): the spans locate and pin the
;; owner's text; the constraint is the text itself, exogenous by declaration.

(defn- with-owner-text [m]
  (-> m
      (assoc :field-roles {:owner-text :constraint
                           :want-span :observation
                           :text-sha256 :observation})
      (update :boxes (fn [bs] (mapv #(if (= :r2-served-by-reading (:box/id %))
                                       (update % :reads (fn [rs] (if (some #{:owner-text} rs) (vec rs) (conj (vec rs) :owner-text))))
                                       %)
                                    bs)))))

(deftest field-roles-move-the-constraint-to-the-owner-text
  (let [m (with-owner-text (head-map))
        {:keys [d v]} (validated m)
        inputs (get-in d [:ports :input])]
    (is (= [:pref/owner-text] (map :id (filter :constraint inputs))))
    (is (not-any? #(#{:pref/want-span :pref/text-sha256} (:id %)) inputs))
    (is (not-any? #(= :world/r2-served-by-reading (:id %)) inputs)
        "the reading box now reads a declared field")
    (is (:valid (check v :timescale-ordering)) "no box writes the constraint")
    (is (= [{:field :owner-text :writers [] :observed-by []}]
           (:preference-fields (proj/i4-report m {}))))
    (testing "a box that writes the owner's text is an I3 finding and an I4 bypass"
      (let [planted (update m :boxes conj {:box/id :planted-editor :box/kind :component
                                           :site {:file "futon2/src/futon2/aif/flight_runner.clj"}
                                           :reads [:chosen-target] :writes [:owner-text]})]
        (is (= [:planted-editor]
               (map (comp :from :edge) (:violations (check (:v (validated planted)) :timescale-ordering)))))
        (is (some #(= [:r1-outer-cascade :planted-editor] (:path %))
                  (get-in (proj/i4-report planted {}) [:bypass :to-preferences])))))))

(deftest the-committed-roles-are-the-tested-form
  ;; not a tautology: strip the committed map's roles and its :owner-text
  ;; read, apply the form tested above, and the projection is the committed
  ;; map's projection
  (let [m (head-map)
        stripped (-> m
                     (dissoc :field-roles)
                     (update :boxes (fn [bs] (mapv #(if (= :r2-served-by-reading (:box/id %))
                                                      (update % :reads (fn [rs] (vec (remove #{:owner-text} rs))))
                                                      %)
                                                   bs))))]
    (is (not= (proj/project m) (proj/project stripped)) "the roles change the projection")
    (is (= (proj/project m) (proj/project (with-owner-text stripped))))))

(deftest opts-override-field-roles
  (is (= #{:x} (proj/preference-fields-of {:field-roles {:owner-text :constraint}}
                                          {:preference-fields [:x]})))
  (is (= proj/default-preference-fields (proj/preference-fields-of {} {}))))

;; ---------------------------------------------------------------------------
;; Re-pin keeps the fixture's header (WM-REPIN-I). The map test above reads
;; the fixture with edn/read-string, which skips `;;` lines: it compares the
;; value only, so the header can change without touching any result.

(deftest repin-keeps-the-header
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "ct-repin" (make-array java.nio.file.attribute.FileAttribute 0)))
        f (str (io/file dir "fixture.edn"))
        comment-lines (fn [] (vec (take-while #(str/starts-with? % ";;")
                                              (str/split-lines (slurp f)))))
        v1 {:map-sha "aaaa" :x 1}
        v2 {:map-sha "bbbb" :x 2}]
    (testing "a missing file gets a header naming the same fields"
      (proj/write-fixture! f v1 {:map-sha "aaaa" :projector-sha "p1" :date "2026-09-26"})
      (is (= (conj proj/fixture-header ";; re-pinned at aaaa from p1 on 2026-09-26")
             (comment-lines)))
      (is (= v1 (edn/read-string (slurp f)))))
    (testing "a two-line header re-pinned twice keeps both lines and gains two"
      (spit f ";; line one\n;; line two\n{:old true}\n")
      (proj/write-fixture! f v1 {:map-sha "aaaa" :projector-sha "p1" :date "2026-09-26"})
      (proj/write-fixture! f v2 {:map-sha "bbbb" :projector-sha "p2" :date "2026-09-27"})
      (is (= [";; line one" ";; line two"
              ";; re-pinned at aaaa from p1 on 2026-09-26"
              ";; re-pinned at bbbb from p2 on 2026-09-27"]
             (comment-lines))
          "the dropped-header bug fails here")
      (is (= v2 (edn/read-string (slurp f))) "the value is the last one written"))))
