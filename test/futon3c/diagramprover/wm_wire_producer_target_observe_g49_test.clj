(ns futon3c.diagramprover.wm-wire-producer-target-observe-g49-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-target-support :as support]
            [futon3c.diagramprover.wm-wire-target-readers-11a :as products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-target-observe-g49-test)
;; The packet named the operation "flight/run!". The source says: the
;; reader's live-check reads the pinned flight-d00574c8 record (no product
;; operation); support/observe :flight-ask-fn builds a flight with
;; driver/resolve-target + flight/start and drives fr/ask-fn;
;; products/products :ask drives flight/start + fr/ask-fn over two
;; targets. The record names what the source says.
(def operation ['futon2.aif.flight-driver/resolve-target
                'futon2.aif.flight/start
                'futon2.aif.flight-runner/ask-fn])

(defn- sanitize
  "Make a value EDN-stable: function objects and per-run temporary paths
  become stated placeholders (see :left-out in the record)."
  [x]
  (cond (fn? x) {:left-out :function}
        (and (string? x) (re-find #"^/tmp/" x)) {:left-out :temporary-path}
        (map? x) (into {} (map (fn [[k v]] [k (sanitize v)])) x)
        (sequential? x) (mapv sanitize x)
        (set? x) (into #{} (map sanitize) x)
        :else x))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support-live-check 'futon3c.diagramprover.wm-wire-target-support/live-check
            :support-observe 'futon3c.diagramprover.wm-wire-target-support/observe
            :support-products 'futon3c.diagramprover.wm-wire-target-readers-11a/products
            :kind :flight-ask-fn
            :tampers {:observe-absent {:absent :target-not-carried}
                      :observe-different "M-other-target"}
            :products-reader :ask}
   :live-records-read support/live-records-read
   :live-check (sanitize (support/live-check :flight-ask-fn))
   :observe (sanitize (support/observe :flight-ask-fn identity))
   :observe-absent (sanitize (support/observe :flight-ask-fn (constantly {:absent :target-not-carried})))
   :observe-different (sanitize (support/observe :flight-ask-fn (constantly "M-other-target")))
   :products-ask (sanitize (products/products :ask))
   :left-out {:read-text "flight :want-source carries a read-text function object, not EDN-readable; the reader checks the flights only up to (dissoc :target) equality, which the placeholder preserves"
              :store "w/tmp-dir gives a per-run temporary directory under /tmp; no reader checks the path"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "target-observe-g49@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "target-observe-g49@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest target-observe-g49-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable target-observe-g49 record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
