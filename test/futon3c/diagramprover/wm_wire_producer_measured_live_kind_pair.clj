(ns futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire-abstention-kind-products :as products] [futon3c.diagramprover.wm-wire-measured-support :as support])
  (:import [java.security MessageDigest]))
(def producer 'futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair-test)
(def operation 'futon2.aif.flight/run!)
(def wire-id [:r9-decision :flight-record-click :kind])
(defn- second-layer [] (let [[a b] (products/pair) clean (fn [f] (-> f (update :clicks #(mapv (fn [c] (update c :abstention dissoc :kind)) %)) (update :needs #(mapv (fn [n] (dissoc n :kind)) %))))]
  {:carrier-kinds [(get-in a [:carrier :abstention :kind]) (get-in b [:carrier :abstention :kind])]
   :carrier-otherwise-equal? (= (update (:carrier a) :abstention dissoc :kind) (update (:carrier b) :abstention dissoc :kind))
   :paths (into {} (for [p [:direct :loop] :let [ra (get-in a [:readback p]) rb (get-in b [:readback p])]] [p {:click-kinds (mapv #(get-in % [:clicks 0 :abstention :kind]) [ra rb]) :need-kinds (mapv #(get-in % [:needs 0 :kind]) [ra rb]) :statuses [(:status ra) (:status rb)] :click-counts [(count (:clicks ra)) (count (:clicks rb))] :otherwise-equal? (= (clean ra) (clean rb))}]))}))
(defn build-record [] {:producer producer :operation operation :inputs {:mutations [:none :absent :different]} :wires {wire-id {:primary (support/kind-observe :none) :interventions {:absent (support/kind-observe :absent) :different (support/kind-observe :different)} :live-pair (support/live-kind-pair) :second-layer (second-layer)}} :left-out {}})
(def stem "measured-live-kind-pair")
(defn- text [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- files [] (filter #(.startsWith (.getName %) (str stem "@")) (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write! [r] (let [s (text r) f (io/file "test/fixtures/wire-producers" (str stem "@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "record exists" {}))) (spit f s) (println f)))
(defn- leaves [x] (letfn [(walk [p v] (if (map? v) (mapcat (fn [[k z]] (walk (conj p k) z)) v) [p]))] (walk [] x)))
(deftest producer-test (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [fs (files)] (is (= 1 (count fs))) (let [e (edn/read-string (slurp (first fs)))] (doseq [p (leaves e)] (testing (pr-str p) (is (= (get-in e p) (get-in a p))))))))))
