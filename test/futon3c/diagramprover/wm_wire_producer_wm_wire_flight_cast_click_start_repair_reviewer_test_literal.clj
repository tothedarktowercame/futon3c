(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-cast-click-start-repair-reviewer-test-literal-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-cast-click-start-repair-reviewer-test-literal-test)
(def operation 'futon3c.transport.http/handle-wm-click-start)
(def wire-id [:flight-cast :click-start :repair-reviewer])
(def stem "wm-wire-flight-cast-click-start-repair-reviewer-test-literal")

(defn- observe [opts]
  (let [cast (fr/click-cast opts)
        captured (atom nil)
        body (merge {:flight-edn (pr-str {:target "M-wire" :wants []})
                     :run-id "wire-run"
                     :issuing-caller "wm-flight"
                     :trigger "duree-click-on-demand"}
                    (into {} (filter (comp string? val)) cast))
        response (with-redefs [service/click! (fn [received]
                                                (reset! captured received)
                                                {:click-id "wire-click-1"})
                               service/cast-preflight-refusal (fn [_] nil)]
                   (@#'http/handle-wm-click-start
                    {:body (json/generate-string body)} {}))]
    {:writer (:repair-reviewer cast)
     :reader (:repair-reviewer @captured)
     :response-status (:status response)}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:seats {:author "claude-6" :reviewer "claude-13" :repair-reviewer "kimi-2"}
            :cases [:primary :absent :different]}
   :wires {wire-id
           {:primary (observe {:author "claude-6" :reviewer "claude-13" :repair-reviewer "kimi-2"})
            :interventions
            {:absent (observe {:author "claude-6" :reviewer "claude-13"})
             :different (observe {:author "claude-6" :reviewer "claude-13"
                                  :repair-reviewer "claude-13"})}}}
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
