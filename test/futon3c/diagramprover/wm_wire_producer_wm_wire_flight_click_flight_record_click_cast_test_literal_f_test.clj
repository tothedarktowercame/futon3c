(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-click-record-products-9a :as products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f-test)
;; The packet named the operation "flight-driver/run-flight!" and the input
;; ".../literal-fixture". Neither occurs in the source: the reader runs no
;; flight; its writer/reader ends are read from the pinned record
;; flight-ada87008.edn (itself written by an earlier real run), and its
;; product calls are fr/click-cast (the different-cast case) and
;; products/products :cast (the second layer, whose product entry point is
;; futon2.aif.flight/record-click). The record names what the source says.
(def operation 'futon2.aif.flight/record-click)

(def record-pin
  {:path (str w/spike-dir "/flight-ada87008/flight-ada87008.edn")
   :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"})
(def earlier-record-pin
  {:path (str w/spike-dir "/flight-278b6988.edn")
   :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"})
(def different-cast-flags {:author "claude-13" :reviewer "claude-6" :repair-reviewer "kimi-2"})

(defn build-record []
  (let [r (w/read-record (:path record-pin))]
    {:producer producer
     :operation operation
     :inputs {:pinned-record record-pin
              :pinned-earlier-record earlier-record-pin
              :wire-ends-paths {:writer [:plan :resolved-steps :cast]
                                :reader [:flight :clicks 0 :cast]}
              :different-cast {:call 'futon2.aif.flight-runner/click-cast
                               :flags different-cast-flags}
              :second-layer {:call 'futon3c.diagramprover.wm-wire-click-record-products-9a/products
                             :field :cast
                             :scenarios 'futon3c.diagramprover.wm-wire-click-record-products-9a/scenarios}}
     :pinned {:record (assoc record-pin :observed-sha256 (w/sha256-file (:path record-pin)))
              :earlier-record (assoc earlier-record-pin :observed-sha256 (w/sha256-file (:path earlier-record-pin)))}
     :wire-ends {:writer (get-in r [:plan :resolved-steps :cast])
                 :reader (get-in r [:flight :clicks 0 :cast])}
     :different-cast (fr/click-cast different-cast-flags)
     :products (into {} (map (fn [[status after]]
                               [status (select-keys (products/products :cast after)
                                                    [:written :products :statuses :carried :unchanged])]))
                     products/scenarios)
     :left-out {:needs-entries "record-click's :needs entries concern the status/detail fields; the reader's cast scenario never checks :needs"
                :full-click-entry-and-flight-record "the reader checks only the two :cast ends and re-pins each whole file by sha256 itself; the rest of the record is not recorded here"
                :run-flight "no flight is run: the packet's run-flight!/literal-fixture names do not occur in the source; the ends come from the pinned record"}}))

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "wm-wire-flight-click-flight-record-click-cast-test-literal-f@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "wm-wire-flight-click-flight-record-click-cast-test-literal-f@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest literal-f-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable literal-f record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
