(ns futon3c.diagramprover.wm-wire-producer-family-decision-test
  "Family test namespace for the 19 decision wire producers. Each producer is a
  support namespace (its deftest is a plain test var); one deftest per producer
  runs that var so the whole family carries a single warrant."
  (:require [clojure.test :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g19 :as ask-out-g19]
            [futon3c.diagramprover.wm-wire-producer-c2-measured-live-records-read :as c2-measured]
            [futon3c.diagramprover.wm-wire-producer-construction-decision :as construction-decision]
            [futon3c.diagramprover.wm-wire-producer-construction-family :as construction-family]
            [futon3c.diagramprover.wm-wire-producer-construction-kernel :as construction-kernel]
            [futon3c.diagramprover.wm-wire-producer-construction-live-digest :as construction-live-digest]
            [futon3c.diagramprover.wm-wire-producer-publication-locators-observe :as locators]
            [futon3c.diagramprover.wm-wire-producer-rates-observe :as rates-observe]
            [futon3c.diagramprover.wm-wire-producer-rates-observe-g28 :as rates-g28]
            [futon3c.diagramprover.wm-wire-producer-rates-observe-g29 :as rates-g29]
            [futon3c.diagramprover.wm-wire-producer-rates-observe-g30 :as rates-g30]
            [futon3c.diagramprover.wm-wire-producer-rates-products-measured-product :as measured-product]
            [futon3c.diagramprover.wm-wire-producer-rates-products-measured-record :as measured-record]
            [futon3c.diagramprover.wm-wire-producer-selection-out-observe :as selection-observe]
            [futon3c.diagramprover.wm-wire-producer-selection-out-refusal :as selection-refusal]
            [futon3c.diagramprover.wm-wire-producer-temperature-observe :as temperature]
            [futon3c.diagramprover.wm-wire-producer-token-input-observe :as token-input]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes :as cascade-spec]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit :as argmax-lit]))

(defmacro ^:private defproducer
  "Define one family deftest named after the producer's record stem. Runs the
  producer's test var via clojure.test/test-vars so every assertion is reported
  once; the testing context names the producer test var and record stem, so a
  failure identifies both the producer and the record field path."
  [stem test-sym]
  `(deftest ~(symbol stem)
     (testing ~(str "producer " test-sym " record " stem)
       (clojure.test/test-vars [(var ~test-sym)]))))

(defproducer "ask-out-live-census-g19" ask-out-g19/ask-out-live-census-g19-producer)
(defproducer "c2-measured-live-records-read" c2-measured/c2-measured-producer)
(defproducer "construction-decision" construction-decision/construction-decision-producer)
(defproducer "construction-family" construction-family/construction-family-producer)
(defproducer "construction-kernel" construction-kernel/construction-kernel-producer)
(defproducer "construction-live-digest" construction-live-digest/construction-live-digest-producer)
(defproducer "publication-locators-observe" locators/locators-producer)
(defproducer "rates-observe" rates-observe/rates-observe-producer)
(defproducer "rates-observe-g28" rates-g28/rates-observe-g28-producer)
(defproducer "rates-observe-g29" rates-g29/rates-observe-g29-producer)
(defproducer "rates-observe-g30" rates-g30/rates-observe-g30-producer)
(defproducer "rates-products-measured-product" measured-product/measurement-product-producer)
(defproducer "rates-products-measured-record" measured-record/rates-product-producer)
(defproducer "selection-out-observe" selection-observe/selection-out-observe-producer)
(defproducer "selection-out-refusal" selection-refusal/selection-out-refusal-producer)
(defproducer "temperature-observe" temperature/temperature-observe-producer)
(defproducer "token-input-observe" token-input/token-input-observe-producer)
(defproducer "wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes" cascade-spec/cascade-spec-producer)
(defproducer "wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit" argmax-lit/selection-law-argmax-producer)
