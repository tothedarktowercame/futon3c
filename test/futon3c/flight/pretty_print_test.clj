(ns futon3c.flight.pretty-print-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.flight.pretty-print :as pretty]))

(def mapping
  (pretty/load-mapping "data/flight-pretty-print-mapping.edn"))

(def fixture
  {:flight/id "live-test"
   :flight/derivation :thin
   :flight/links [{:type :cites-finding :to "live-prior"}]
   :organs
   {:field-read {:judgment {:gauge {:count 2}
                            :neighbourhood {:sorry {:kind :derivation-thin}}}
                 :ground "source"}
    :act {:sorry {:kind :proposal-mode}}
    :measurement {:judgment {:predicted {:g -1.0 :g-grain :one-step-action}
                             :realised {:g -1.0 :g-grain :one-step-action}
                             :error 0.0
                             :class nil}
                  :ground "thin"}}})

(deftest renders-all-canonical-organs-in-order
  (let [out (pretty/render-flight mapping fixture)
        positions (map #(.indexOf out (str "## " (name %)))
                       (:canonical-organ-order mapping))]
    (is (every? #(<= 0 %) positions))
    (is (= positions (sort positions)))
    (is (str/includes? out "Atlas-role: :source-material"))
    (is (str/includes? out "Hole-kind: :proposal-mode"))
    (is (str/includes? out ":missing-organ"))))

(deftest renders-byte-stably
  (let [a (pretty/render-flight mapping fixture)
        b (pretty/render-flight mapping fixture)]
    (is (= a b))))

(deftest maps_nil_measurement_class_for_thin_records
  (let [out (pretty/render-flight mapping fixture)]
    (is (str/includes? out "Atlas-role: :derivation-thin-prose-lost"))))
