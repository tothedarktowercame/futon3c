(ns futon3c.apm.coined-pattern-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.coined-pattern :as sut]))

(deftest coined-pattern-is-proposed-and-carries-its-witness
  (let [result (sut/pattern-entities
                {:depositor "f36-scribe"
                 :new-pattern-rationales
                 {"math-formalization/new-route" "No existing route fits."}
                 :candidates
                 [{:memory-id "memory-1"
                   :pattern-ids ["math-formalization/new-route"]}]})
        entity (first (:entities result))]
    (is (:ok result))
    (is (= "pattern/library" (:type entity)))
    (is (= "proposed" (get-in entity [:props "attachment-status"])))
    (is (= "f36-scribe" (get-in entity [:props "pattern/coiner"])))
    (is (= ["memory-1"]
           (get-in entity [:props "pattern/witness-memory-ids"])))))

(deftest unwitnessed-coined-pattern-is-rejected-before-publication
  (let [result (sut/pattern-entities
                {:depositor "f36-scribe"
                 :new-pattern-rationales
                 {"math-formalization/unwitnessed" "No existing route fits."}
                 :candidates []})]
    (is (false? (:ok result)))
    (is (= [:pattern-without-witness] (:findings result)))
    (is (= ["math-formalization/unwitnessed"] (:pattern-ids result)))))

(deftest historical-pattern-file-is-an-explicit-witness
  (let [path "holes/labs/M-apm-demonstration/pattern-library-zai-scribe-f34-a95J03.md"
        entities (sut/file-pattern-entities path "f34-zai-scribe")]
    (is (= 3 (count entities)))
    (is (= "math-strategy/route-map-before-reconstruction"
           (:id (first entities))))
    (is (= [path]
           (get-in (first entities) [:props "pattern/witness-deposits"])))
    (is (every? #(= "proposed" (get-in % [:props "attachment-status"]))
                entities))))
