(ns futon3c.diagramprover.wm-wire-producer-wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix
  "Producer for the [:loop-entry :r1-outer-cascade :trigger] wire. Runs the
  real futon2.wm-trigger/trigger-from-env 1-arity (the writer's end, as
  wm-scheduled-run/-main calls it) with FUTON_WM_TRIGGER stubbed to
  \"wallclock-cron\", then the real futon2.aif.outer-cascade/select (the
  reader's end) over the reader's FIELD with seed 1 and the writer's value
  as :trigger — and the same select over the two intervention inputs
  (no trigger, a different trigger) — and asserts the observation equals
  the committed record."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.outer-cascade :as oc]
            [futon2.wm-trigger]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix-test)
(def operation '[futon2.wm-trigger/trigger-from-env futon2.aif.outer-cascade/select])
(def stem "wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix")
(def wire-id [:loop-entry :r1-outer-cascade :trigger])
(def mutations [:absent :different])

(def field
  "The reader's FIELD: a minimal field with one eligible target (select's
  input shape)."
  {:considered [{:target "M-a" :kind :mission}]
   :feasible [{:target "M-a" :kind :mission :next-step :ready :eligible true}]
   :exclusions []})

(defn- trigger-from-env [getenv]
  (@(ns-resolve 'futon2.wm-trigger 'trigger-from-env) getenv))

(defn- observe [trigger-opt]
  (let [writer (trigger-from-env (constantly "wallclock-cron"))
        handed (if (= ::written trigger-opt) writer trigger-opt)
        r (oc/select {:field field :seed 1 :trigger handed})]
    {:writer writer
     :reader (get-in r [:target-selection :trigger])}))

(defn- observation [mutation]
  (let [o (observe (case mutation
                     :none ::written
                     :absent nil
                     :different :duree-click-on-demand))]
    (assoc o :received? (w/received? o))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:writer "futon2.wm-trigger/trigger-from-env 1-arity over (constantly \"wallclock-cron\"), as wm-scheduled-run/-main calls it"
            :field 'futon3c.diagramprover.wm-wire-loop-entry-r1-outer-cascade-trigger-test/field
            :seed 1
            :wire-id wire-id
            :mutations (into [:none] mutations)}
   :wires {wire-id {:primary (observation :none)
                    :interventions (into {} (map (juxt identity observation) mutations))}}
   :left-out {:-main-scheduled-tick "wm-scheduled-run/-main itself is not drivable hermetically (trace writes, evidence emit); the writer's value is observed from a real trigger-from-env call, as the reader's support already did"
              :live-record-verification "the reader re-verifies the live pins itself (sha256 and the absence of a scheduled-run :trigger) through w/sha256-file and w/read-record; the entries are carried for the wire map's :live-records-read"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) (str stem "@"))
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str stem "@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable record for this stem")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path)))))))))))
