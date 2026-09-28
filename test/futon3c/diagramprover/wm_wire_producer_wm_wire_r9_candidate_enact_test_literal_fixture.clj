(ns futon3c.diagramprover.wm-wire-producer-wm-wire-r9-candidate-enact-test-literal-fixture
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.policy :as policy]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire-r9-support :as r9-support]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-r9-candidate-enact-test-literal-fixture-test)
(def operation
  ['futon2.aif.policy/select-action-cascades
   'futon2.aif.full-loop-runner/run-opportunity!
   'futon2.aif.flight/record-click
   'futon2.aif.flight-runner/enact-fn])
(def wire-id [:r9-selection-law :r0-enact-step :candidate])
(def stem "wm-wire-r9-candidate-enact-test-literal-fixture")

(defn- step [id]
  {:id id :target "M-t" :guard {:clauses [{:present #{} :absent #{}}]} :produces #{}})
(defn- ranked [cascade-id action-id pattern g rank]
  {:action {:kind :cascade-candidate :id action-id :cascade-id cascade-id :target "M-t"
            :precedence [(step pattern)]
            :construction-receipt {:kind :fixture}
            :interpretation-receipts {cascade-id {:kind :fixture}}}
   :cascade true :cascade-id cascade-id :controller-score g :rank rank})
(def roster [(ranked :cas/b :cas/b :p/b 1.0 1)
             (ranked :cas/a :cas/a :p/a 3.0 2)])

(defn- runner-opts [decision]
  (merge (hermetic/runner-repair-options)
         r9-support/hermetic-runner-defaults
         {:cohort? false :author "zai-5" :reviewer "codex-7" :repair-reviewer "codex-1"
          :phase-log-fn (fn [_])
          :roster-fn (fn [_] {:zai-5 {:status "idle" :invoke-ready? true}
                              :codex-7 {:status "idle" :invoke-ready? true}
                              :codex-1 {:status "idle" :invoke-ready? true}})
          :refresh-fn (fn [])
          :substrate-preflight-fn (fn [_] {:route :test})
          :code-state-fn (fn [] {:repo "/futon2" :git-sha "head" :git-dirty? false :repo-heads {}})
          :mode-flags-fn (fn [])
          :version-stamp-fn identity
          :mission-fn (fn [target] {:id target})
          :repair-open-fn (constantly [])
          :repair-system-record-fn (fn [m] {:repair/id "repair-wire-1" :repair/class (:repair-class m)})
          :r16-park-fn (fn [_ _] {:ok true :id "park-wire" :status :parked})
          :delivery-qa-fn (fn [_ _] {:morning-brief/addendum-id "qa-wire"})
          :queue-fn identity
          :judge-fn (fn [_] {:judgement {:decision decision :belief {} :belief-pre {}
                                         :observation {} :free-energy {}
                                         :prediction-errors {} :precision-state {}
                                         :micro-step-trace [] :mode :maintain}})
          :construct-fn (fn [& _]
                          (throw (ex-info "stop after selection" {:outcome :incomplete})))}))

(defn- run-record [decision]
  (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                   #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                   #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
    #(binding [runner/*wm-status-reporting?* false]
       (edn/read-string
        (slurp (:run-record (runner/run-opportunity! (runner-opts decision))))))))

(defn- enact [record click]
  (let [f (flight/record-click
           (flight/start {:target "M-t" :chosen-because {:kind :requested}}
                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                         {:id "flight-wire"})
           (merge click {:wants [:t/b] :before {} :after {}}))
        out ((fr/enact-fn
              {:dispatch-step! (fn [s] {:commit "c1"
                                        :produced (first (get-in s [:interpretation :produces]))
                                        :check {:class :fixture}})
               :check-fn (fn [_] {:observed true})
               :interpretations (constantly {:p/b {:produces #{:t/b}}
                                             :p/a {:produces #{:t/a}}})
               :fetch-run-record (constantly record)
               :record-dir (w/tmp-dir "wire-enactments")})
             f (last (:clicks f)))]
    (if (:enactment out) (:enactment out) out)))

(defn- observe
  ([candidate-roster] (observe candidate-roster identity))
  ([candidate-roster click-fn]
   (let [decision (policy/select-action-cascades candidate-roster
                                                  {:beta 1 :novelty-inputs {}})
         record (run-record decision)
         enactment (enact record
                          (click-fn (fr/record-summary "M-t" "click-1" record)))]
     {:writer (get-in decision [:selection-law :candidate])
      :run-record-chosen (get-in record [:decision :chosen :candidate])
      :reader (if (w/typed-absence? enactment)
                enactment
                (:decision-candidate enactment))})))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs [{:case :primary :roster :literal-roster :click-transform :identity}
            {:case :absent :roster :literal-roster :click-transform :dissoc-chosen}
            {:case :different :roster :different-action-id :click-transform :identity}]
   :wires {wire-id
           {:primary (observe roster)
            :interventions
            {:absent (observe roster #(dissoc % :chosen))
             :different (observe [(ranked :cas/b :act/other :p/b 1.0 1)
                                  (ranked :cas/a :cas/a :p/a 3.0 2)])}}}
   :left-out {}})

(defn- record-text [value] (str (pr-str value) "\n"))
(defn- sha256 [text]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes text "UTF-8")))))
(defn- record-files []
  (filter #(.startsWith (.getName %) (str stem "@"))
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str stem "@" (subs (sha256 text) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "record exists" {:file (str file)})))
    (spit file text)
    (println file)))
(defn- leaf-paths [value]
  (letfn [(walk [path node]
            (if (map? node)
              (mapcat (fn [[key child]] (walk (conj path key) child)) node)
              [path]))]
    (walk [] value)))

(deftest producer-test
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (record-files)]
        (is (= 1 (count files)))
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
