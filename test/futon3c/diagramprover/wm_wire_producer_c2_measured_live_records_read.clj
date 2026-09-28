(ns futon3c.diagramprover.wm-wire-producer-c2-measured-live-records-read-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support])
  (:import [java.security MessageDigest]))
(def producer 'futon3c.diagramprover.wm-wire-producer-c2-measured-live-records-read-test)
(defn build-record []
  (let [o (support/measured :status :none) a (support/measured :status :absent)
        d (support/measured :status :different)]
    (support/assert-live-records support/measured-live-records-read :measured-a)
    {:producer producer
     :operation ['futon2.aif.observation-rates/sourced-rates
                 'futon2.aif.wm.cascade-decision/measured-a-version]
     :inputs {:field :status :mutations [:none :absent :different]}
     :fields {:writer (:writer o) :reader (:reader o)
              :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o))
              :received? (w/received? o) :schema? (= :wm/measured-a-v1 (get-in o [:result :schema]))
              :rates-correct? (= {:t {:false-neg 1/10 :false-pos 1/5}} (:rates (:producer o)))
              :absent {:status-reason? (= {:status :absent :reason :sourcing-refused} (select-keys (:result a) [:status :reason]))
                       :target-refusal? (contains? (:refusals (:result a)) support/measured-target)
                       :reader-absent? (= :absent (:reader a)) :typed-absence? (w/typed-absence? (:result a))
                       :received? (w/received? (select-keys a [:writer :reader]))}
              :different {:status-reason? (= {:status :absent :reason :sourcing-refused} (select-keys (:result d) [:status :reason]))
                          :actual-status? (= :sourcing-refused-differently (get-in d [:result :refusals support/measured-target :status]))
                          :received? (w/received? (select-keys d [:writer :reader]))}
              :live-records-lack-measured-a? true}
     :left-out {:full-measured-record "reader checks its schema, rate relation, refusal fields and carrier equality"}}))
(defn- text [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (text x) f (io/file "test/fixtures/wire-producers" (str "c2-measured-live-records-read@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "producer record already exists" {:file (str f)}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(walk [p v] (if (map? v) (mapcat (fn [[k x]] (walk (conj p k) x)) v) [p]))] (walk [] x)))
(deftest c2-measured-producer
  (let [a (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a)
        (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "c2-measured-live-records-read@") (.listFiles (io/file "test/fixtures/wire-producers"))))))]
          (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
