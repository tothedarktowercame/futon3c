(ns futon3c.watcher.projections.flexiarg-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as file-ingest]
            [futon3c.watcher.projections.flexiarg :as sut]))

(def orchestration-pattern-path
  "/home/joe/code/futon3/library/orchestration/state-in-substrate-deltas-in-messages.flexiarg")

(deftest collect-file-projects-canonical-pattern-packet
  (testing "the watcher reuses the canonical parser and keeps structured slots"
    (let [{:keys [ns aliases vars tests is-test?]} (sut/collect-file orchestration-pattern-path)
          v (first vars)
          slot-keys (mapv :slot/name-key (:pattern/slots v))]
      (is (= "flexiarg.orchestration" ns))
      (is (= {} aliases))
      (is (false? is-test?))
      (is (= [] tests))
      (is (= 1 (count vars)))
      (is (= "flexiarg.orchestration/state-in-substrate-deltas-in-messages"
             (:var/qname v)))
      (is (= "orchestration/state-in-substrate-deltas-in-messages"
             (:pattern/id v)))
      (is (= "Lift State Into Shared Substrate; Keep Messages As Deltas"
             (:pattern/title v)))
      (is (= ["📁/?"] (:pattern/sigils-raw v)))
      (is (true? (:pattern/sigil-pending v)))
      (is (= 10 (count (:pattern/slots v))))
      (is (= ["conclusion" "context" "if" "however" "then"
              "because" "next-steps" "does-not-apply"
              "instances" "related"]
             slot-keys))
      (is (str/includes? (get-in v [:pattern/slots 2 :slot/text])
                         "more than one back-and-forth")))))

(deftest ingest-one-file-emits-pattern-props-and-slot-edges
  (testing "phase-4.5 ingest posts rich pattern props and one deterministic edge per slot"
    (let [hx-calls (atom [])
          doc-calls (atom [])]
      (with-redefs [file-ingest/post-hyperedge! (fn [hx-type endpoints labels props]
                                                  (swap! hx-calls conj {:hx-type hx-type
                                                                        :endpoints endpoints
                                                                        :labels labels
                                                                        :props props})
                                                  {:ok? true})
                    file-ingest/post-hyperedge-doc! (fn [payload]
                                                     (swap! doc-calls conj payload)
                                                     {:ok? true})]
        (let [stats (file-ingest/ingest-one-file! {:path orchestration-pattern-path
                                                   :label "futon3"
                                                   :root-ctx {:by-ns {}}})
              var-call (some #(when (= "code/v05/var" (:hx-type %)) %) @hx-calls)
              contains-call (some #(when (= "code/v05/contains" (:hx-type %)) %) @hx-calls)
              first-slot (first @doc-calls)]
          (is (= {:vertices 2 :edges 11 :failed 0} stats))
          (is (= ["futon3/flexiarg.orchestration/state-in-substrate-deltas-in-messages"]
                 (:endpoints var-call)))
          (is (= "Lift State Into Shared Substrate; Keep Messages As Deltas"
                 (get-in var-call [:props "pattern/title"])))
          (is (= true (get-in var-call [:props "pattern/sigil-pending"])))
          (is (str/includes? (get-in var-call [:props "pattern/if"])
                             "more than one back-and-forth"))
          (is (= 10 (count (get-in var-call [:props "pattern/slots"]))))
          (is (= ["futon3/flexiarg.orchestration"
                  "futon3/flexiarg.orchestration/state-in-substrate-deltas-in-messages"]
                 (:endpoints contains-call)))
          (is (= 10 (count @doc-calls)))
          (is (= "hx:code/v05/pattern-slot:flexiarg.orchestration/state-in-substrate-deltas-in-messages:0:conclusion"
                 (:id first-slot)))
          (is (= ["futon3/flexiarg.orchestration/state-in-substrate-deltas-in-messages"
                  "slot/conclusion"]
                 (:endpoints first-slot)))
          (is (= "orchestration/state-in-substrate-deltas-in-messages"
                 (get-in first-slot [:props "pattern/id"])))
          (is (= "conclusion" (get-in first-slot [:props "slot/name-key"])))
          (is (str/includes? (get-in first-slot [:props "slot/text"])
                             "lift state into a shared substrate")))))))
