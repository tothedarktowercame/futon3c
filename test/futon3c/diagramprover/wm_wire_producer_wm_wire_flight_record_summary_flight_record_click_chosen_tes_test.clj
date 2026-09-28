(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-chosen-tes-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-click-close-test-chosen-test :as fixture]
            [futon3c.diagramprover.wm-wire-summary-products :as products])
  (:import [java.security MessageDigest]))
(defn build-record []
  (let [p (fixture/observe fixture/roster) a (fixture/observe fixture/roster "M-some-other-target") d (fixture/observe fixture/roster-other)
        [x y] (products/products :record-click :chosen) rx (:record x) ry (:record y)]
    {:producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-chosen-tes-test
     :operation ['futon2.aif.full-loop-runner/run-opportunity! 'futon2.aif.flight-runner/record-summary 'futon2.aif.flight/record-click 'futon3c.diagramprover.wm-wire-summary-products/products]
     :inputs {:rosters [:cas-b-first :cas-a-first] :targets ["M-t" "M-some-other-target"] :summary-product [:record-click :chosen]}
     :fields {:positive p :absent a :different d
              :summary {:carriers-without-field-equal? (= (dissoc (:carrier x) :chosen) (dissoc (:carrier y) :chosen))
                        :a-copied? (= (get-in x [:carrier :chosen]) (get-in rx [:clicks 0 :chosen]))
                        :b-copied? (= (get-in y [:carrier :chosen]) (get-in ry [:clicks 0 :chosen]))
                        :values-differ? (not= (get-in rx [:clicks 0 :chosen]) (get-in ry [:clicks 0 :chosen]))
                        :records-without-field-equal? (= (update rx :clicks #(mapv (fn [c] (dissoc c :chosen)) %)) (update ry :clicks #(mapv (fn [c] (dissoc c :chosen)) %)))
                        :statuses [(:status rx) (:status ry)] :click-counts [(count (:clicks rx)) (count (:clicks ry))]}}
     :left-out {:temporary-stores "the shared hermetic fixture removes them" :full-summary-records "the reader checks the recorded relations"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- leaves [x] (letfn [(f [p v] (if (map? v) (mapcat (fn [[k x]] (f (conj p k) x)) v) [p]))] (f [] x)))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "wm-wire-flight-record-summary-flight-record-click-chosen-tes@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(deftest producer-test (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [f (first (filter #(.startsWith (.getName %) "wm-wire-flight-record-summary-flight-record-click-chosen-tes@") (.listFiles (io/file "test/fixtures/wire-producers")))) e (edn/read-string (slurp f))] (doseq [p (leaves (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
