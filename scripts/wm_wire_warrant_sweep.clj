(ns wm-wire-warrant-sweep
  "Run from futon3c root:
   clojure -Sdeps '{:aliases {:sweep {:extra-paths [\"scripts\"]}}}' -M:sweep -m wm-wire-warrant-sweep [--only NS] [--register]
   Default: read-only closure audit; never loads wire namespaces or runs tests.
   --register runs stale/nonpassing/missing namespaces serially at one HEAD."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.evidence.http-backend :as http]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.ledger :as artifacts]))

(def classes [:current :stale-closure :not-passing :no-warrant :read-failed])
(defn wire-namespaces [file]
  (with-open [r (java.io.PushbackReader. (io/reader file))]
    (loop []
      (let [f (read {:eof ::eof} r)]
        (cond (= ::eof f) (throw (ex-info "wire-test-nses not found" {:file file}))
              (and (seq? f) (= 'def (first f)) (= 'wire-test-nses (second f)))
              (let [q (nth f 2)]
                (when-not (and (= 'quote (first q)) (vector? (second q)))
                  (throw (ex-info "Expected literal quoted namespace vector" {:form f})))
                (mapv str (distinct (second q))))
              :else (recur))))))

(defn classify [root payload]
  (cond
    (nil? payload) {:class :no-warrant}
    (or (= :registry-read-failed (:reason payload)) (= :read-failed (:error/code payload)))
    {:class :read-failed :reason :registry-read-failed :detail payload}
    (not (and (= 0 (get-in payload [:results :failures]))
              (= 0 (get-in payload [:results :errors])))) {:class :not-passing}
    (not (true? (:warrant? payload))) {:class :no-warrant}
    (not (seq (:load-closure payload))) {:class :no-warrant :reason :missing-load-closure}
    :else
    (let [changed (registry/closure-diff
                   (registry/closure-shas (:load-closure payload))
                   (registry/current-closure-shas root (:load-closure payload)))]
      (if (seq changed) {:class :stale-closure :changed-paths (vec (take 5 changed))
                        :changed-count (count changed)} {:class :current}))))

(defn assess [root namespace hit read-payload]
  (try
    (merge {:namespace namespace :entry-id (:entry-id hit)}
           (if hit
             (let [payload (read-payload (:entry-id hit))]
               (if payload (classify root payload)
                   {:class :read-failed :reason :registry-read-failed :detail :indexed-entry-missing}))
             {:class :no-warrant}))
    (catch Exception e
      {:namespace namespace :entry-id (:entry-id hit) :class :read-failed
       :reason :registry-read-failed :detail (or (ex-data e) (ex-message e))})))

(defn scan [root namespaces read-payload]
  (try
    (let [index (:namespaces (registry/namespace-ledger (str root "/data/test-registry/namespace-ledger.edn")))]
      (mapv #(assess root % (get index %) read-payload) namespaces))
    (catch Exception e
      (mapv #(hash-map :namespace % :class :read-failed :reason :registry-read-failed
                      :detail (or (ex-data e) (ex-message e))) namespaces))))

(defn summary [rows]
  {:counts (merge (zipmap classes (repeat 0)) (frequencies (map :class rows)))
   :non-current (filterv #(not= :current (:class %)) rows)})

(defn failure-snippet [result]
  (let [text (str (:out result) "\n" (:err result))
        row (try (edn/read-string (last (str/split-lines (:out result)))) (catch Exception _ nil))
        artifact (get-in row [:payload :log-artifact])
        file (when (:sha256 artifact) (artifacts/resolve-file (:sha256 artifact)))
        log (if file (slurp file) text)
        at (re-find #"(?m)^(?:FAIL|ERROR) in .*" log)
        excerpt (if at (subs log (str/index-of log at)) text)]
    (subs excerpt 0 (min 300 (count excerpt)))))

(defn register-one! [root revision namespace]
  (let [script (.getCanonicalPath (io/file root "../futon2/scripts/wm/register-warrant.sh"))
        result (shell/sh "timeout" "600s" script "--pinned" revision namespace
                         :dir root :env (assoc (into {} (System/getenv))
                                              "AUTHOR" (or (System/getenv "AUTHOR") "wire-warrant-sweep")
                                              "CODE_PATHS" "test/futon3c/diagramprover/wm_wire.clj"))]
    (cond-> {:namespace namespace :registration-exit (:exit result)}
      (not= 0 (:exit result)) (assoc :failure (failure-snippet result)))))

(defn -main [& args]
  (let [register? (boolean (some #{"--register"} args))
        only (second (drop-while #(not= "--only" %) args))
        _ (when (or (and (some #{"--only"} args) (nil? only))
                    (seq (remove #{"--register" "--only" only} args)))
            (throw (ex-info "Usage: [--register] [--only NS]" {:args args})))
        root (.getCanonicalPath (io/file "."))
        all (wire-namespaces "test/futon3c/diagramprover/wm_wire_ledger_test.clj")
        _ (when (and only (not (some #{only} all))) (throw (ex-info "Unknown wire namespace" {:namespace only})))
        namespaces (if only [only] all)
        backend (http/make-http-backend (or (System/getenv "AGENCY_URL") "http://localhost:7070"))
        read-payload #(-> (registry/read-chain! backend %) last :payload)
        rows (scan root namespaces read-payload)]
    (prn (assoc (summary rows) :mode (if register? :before-register :dry-run)))
    (when register?
      (let [revision (shell/sh "git" "rev-parse" "HEAD" :dir root)]
        (when-not (zero? (:exit revision)) (throw (ex-info "Cannot pin HEAD" revision)))
        (doseq [row rows :when (not (#{:current :read-failed} (:class row)))]
          (prn {:namespace (:namespace row) :action :register :previous-class (:class row)}) (flush)
          (prn (register-one! root (str/trim (:out revision)) (:namespace row))) (flush))
      (prn (assoc (summary (scan root namespaces read-payload)) :mode :after-register))))
    (shutdown-agents)))
