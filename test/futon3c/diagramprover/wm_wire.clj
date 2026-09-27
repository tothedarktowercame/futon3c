(ns futon3c.diagramprover.wm-wire
  "Support for the War Machine wire tests: the first-layer check and the
  live-record reads. What a wire test is, and the three statuses, are
  defined once, in futon3c.diagramprover.wm-wire-ledger-test's docstring."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.security MessageDigest]))

(def spike-dir "holes/labs/M-wm-wiring/spike")

(defn typed-absence?
  "An absence typed on the record: {:absent ...}, or the older
  {:status :absent ...} shape some records still carry. Inspect entries so
  string-keyed sorted maps never compare their keys with a keyword probe."
  [v]
  (and (map? v)
       (boolean (some (fn [[k value]]
                        (or (= :absent k)
                            (and (= :status k) (= :absent value))))
                      v))))

(defn received?
  "The first layer: the reader's value is present, is not a typed absence,
  and is the writer's value."
  [{:keys [writer reader]}]
  (and (some? reader) (not (typed-absence? reader)) (= writer reader)))

(defn sha256-file [path]
  (let [bytes (java.nio.file.Files/readAllBytes (.toPath (io/file path)))]
    (apply str (map #(format "%02x" (bit-and 0xff %))
                    (.digest (MessageDigest/getInstance "SHA-256") bytes)))))

(defn read-record [path]
  (edn/read-string {:default tagged-literal} (slurp path)))

(defn tmp-dir [prefix]
  (str (java.nio.file.Files/createTempDirectory
        prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(def ^:dynamic *warrant-store-path*
  "Local warrant database override. Nil selects REGISTRY_DB, then
  the test registry's canonical local path."
  nil)

(defn warrant-store-path []
  (or *warrant-store-path*
      (System/getenv "REGISTRY_DB")
      sqlite/default-path))

(defn latest-local-run
  "Return the newest local run for namespace after verifying its registry
  chain. A namespace absent from the local store is a normal typed absence."
  [namespace]
  (let [backend (sqlite/sqlite-backend (warrant-store-path))]
    (if-let [entry (sqlite/latest-run-for-namespace backend namespace)]
      (last (registry/read-chain! backend (:evidence/id entry)))
      {:record/type :absent :reason :no-local-record :namespace namespace})))

(defn second-layer
  "Resolve a declaration independently of its execution evidence. Product paths
  address the test's reader observation (or the returned product itself).
  Callers supply registry/Git reads so admission fixtures need neither service."
  [{:keys [wire] :as r} {:keys [allowed-nses latest last-commit ancestor? record-only?]}]
  (if-let [{:keys [test kind product intervention expected] :as d} (:second-layer r)]
    (let [n (when (symbol? test) (some-> test namespace symbol))
          v (when (and n (find-ns n)) (ns-resolve n (symbol (name test))))
          fail! #(throw (ex-info "Invalid second-layer declaration" {:wire wire :reason % :declaration d}))]
      (when-not (and (symbol? test) (or (contains? allowed-nses n)
                                  (and (= n (some-> (:test r) namespace symbol))
                                       (.startsWith (str n) "futon2."))) (var? v) (:test (meta v)))
        (fail! :not-an-admitted-deftest))
      (when-not (and (#{:value-varying :refusal :record} kind) (vector? product)
                     (= :before-reader intervention) (or (not= kind :refusal) (keyword? expected)))
        (fail! :malformed-second-layer))
      (when (and (record-only? wire) (not= :record kind))
        (fail! :computational-use-still-to-do))
      (let [run (latest (str n)) p (:payload run)
            id (:evidence/id run) pin (:git-head p) revision (last-commit v)
            evidence (cond
                       (not (true? (:warrant? p)))
                       {:absent :no-warrant :found-id id :lookup-reason (:reason run)}
                       (not (and revision pin (ancestor? revision pin)))
                       {:absent :stale-warrant :found-id id :git-head pin :test-revision revision}
                       :else {:warrant-id id :git-head pin :test-revision revision :ran-at (:ran-at p)})]
        {:declared (assoc d :test (symbol (str (ns-name (:ns (meta v)))) (str (:name (meta v)))))
         :evidence evidence}))
    {:absent :no-second-layer-test}))
