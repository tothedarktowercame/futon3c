(ns futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32-test)
;; The packet named the operation
;; "flight assembly/click+http/handle-wm-click-start" and inputs
;; "click|live-census". The reader's support call is
;; (support/click :http :wants <mutation>): flight/click-wants and
;; flight/judge-opts build the value, then flight-runner/http-click-fn posts
;; it through futon3c.transport.http/handle-wm-click-start and the runner IO
;; captures the handler-produced options. The record names that entry point.
(def operation 'futon2.aif.flight-runner/http-click-fn)

(defn- case-fields [mutation]
  (let [o (support/click :http :wants mutation)]
    (cond-> {:writer (:writer o)
             :reader (:reader o)}
      (= mutation :absent) (assoc :result (:result o)))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support-click 'futon3c.diagramprover.wm-wire-ask-out-support/click
            :click-arguments {:kind :http :field :wants}
            :support-live-census 'futon3c.diagramprover.wm-wire-ask-out-support/live-census
            :support-live-records-read 'futon3c.diagramprover.wm-wire-ask-out-support/live-records-read
            :http-boundary 'futon3c.transport.http/handle-wm-click-start
            :mutations [:none :absent :different]}
   :live-records-read support/live-records-read
   :live-census (support/live-census)
   :cases (into {} (map (juxt identity case-fields)) [:none :absent :different])
   :left-out {:result-for-present-cases "the captured click options for :none and :different contain the issue! function object created inside handle-wm-click-start, which is not EDN-readable; readers check only (nil? (:result o)) for the absent case"
              :judge-opts-beyond-wants "readers check only the :wants field the writer/reader pair carries, not the rest of the judge options"
              :temporary-store "the flight store lives in a per-run temporary directory; no recorded value contains a path, clock or uuid, so no relation was needed"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "ask-out-live-census-g32@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "ask-out-live-census-g32@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest ask-out-live-census-g32-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable ask-out-live-census-g32 record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
