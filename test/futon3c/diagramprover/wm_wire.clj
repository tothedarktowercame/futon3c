(ns futon3c.diagramprover.wm-wire
  "Support for the War Machine wire tests: the first-layer check and the
  live-record reads. What a wire test is, and the three statuses, are
  defined once, in futon3c.diagramprover.wm-wire-ledger-test's docstring."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
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

(defn- source-repo [v]
  (when-let [url (some-> (:file (meta v)) io/resource)]
    (let [source (.getCanonicalPath (io/file (.toURI url)))]
      (or (some (fn [[repo root]]
                  (when (.startsWith source (str root java.io.File/separator)) repo))
                [["futon2" "/home/joe/code/futon2"]
                 ["futon3c" "/home/joe/code/futon3c"]])
          (loop [dir (.getParentFile (io/file source))]
            (when dir
              (let [dotgit (io/file dir ".git")]
                (cond
                  (.isFile dotgit)
                  (let [pointer (slurp dotgit)]
                    (cond
                      (.contains pointer "/home/joe/code/futon2/.git/") "futon2"
                      (.contains pointer "/home/joe/code/futon3c/.git/") "futon3c"
                      :else nil))
                  (.isDirectory dotgit)
                  (when (#{"futon2" "futon3c"} (.getName dir)) (.getName dir))
                  :else (recur (.getParentFile dir))))))))))

(defn- warrant-evidence [answer]
  (cond
    (= :current (:status answer))
    {:warrant-id (:entry-id answer) :git-head (:git-head answer) :ran-at (:ran-at answer)}

    (and (= :missing (:status answer)) (= :no-current-warrant (:kind answer)))
    (let [{:keys [reason request-id run-requested-at found-entry-id request-state]} (:data answer)]
      {:absent (if (= :stale reason) :stale-warrant :no-warrant)
       :request-id request-id :request-state request-state
       :run-requested-at run-requested-at :found-id found-entry-id})

    (= :missing (:status answer))
    {:absent :no-warrant :found-id (get-in answer [:data :found-entry-id])
     :lookup-reason (:kind answer) :detail (get-in answer [:data :reason])}

    :else {:absent :warrant-lookup-failed :lookup answer}))

(defn second-layer
  "Resolve a declaration independently of its execution evidence. Product paths
  address the test's reader observation (or the returned product itself).
  Callers supply the registry lookup so admission fixtures need no local store."
  [{:keys [wire] :as r} {:keys [allowed-nses lookup record-only?]}]
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
      (let [repo (source-repo v)
            evidence (if repo
                       (warrant-evidence (lookup (str n) repo))
                       {:absent :test-repository-unknown :source-file (:file (meta v))})]
        {:declared (assoc d :test (symbol (str (ns-name (:ns (meta v)))) (str (:name (meta v)))))
         :evidence evidence}))
    {:absent :no-second-layer-test}))
