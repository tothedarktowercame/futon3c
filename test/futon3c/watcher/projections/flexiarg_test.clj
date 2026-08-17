(ns futon3c.watcher.projections.flexiarg-test
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [babashka.http-client :as http]
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
      (is (map? (:pattern/directives v)))
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
          (is (= {:vertices 2 :edges 11 :failed 0 :retracted 0} stats))
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

(deftest watcher-reports-only-unknown-directives-per-file
  (let [file (io/file (System/getProperty "java.io.tmpdir")
                      (str "flexiarg-directives-" (java.util.UUID/randomUUID)
                           ".flexiarg"))]
    (try
      (spit file (str "@flexiarg demo/reporter\n"
                      "@bits 01010101\n"
                      "@wibble invented\n"
                      "! conclusion:\n  reporter fixture\n"))
      (let [result (atom nil)
            output (with-out-str (reset! result (sut/collect-file file)))
            directives (get-in @result [:vars 0 :pattern/directives])]
        (is (not (contains? directives :bits)))
        (is (not (contains? directives :wibble)))
        (is (str/includes? output "FLEXIARG DIRECTIVE UNKNOWN"))
        (is (str/includes? output "@wibble=1"))
        (is (not (str/includes? output "@bits=")))
        (is (not (str/includes? output "known-not-ingested"))))
      (finally (.delete file)))))

(deftest flexiarg-dispatch-emits-canonical-entities-and-relations-only
  (let [path "/home/joe/code/futon3/library/baldwin/two-claims-not-one.flexiarg"
        entities (atom [])
        relations (atom [])
        hyperedges (atom [])]
    (with-redefs [file-ingest/post-entities-batch!
                  (fn [payload]
                    (swap! entities into payload)
                    {:ok? true :count (count payload)
                     :entities (mapv #(assoc % :id (:name %)) payload)})
                  file-ingest/post-relations-batch!
                  (fn [payload]
                    (swap! relations into payload)
                    {:ok? true :count (count payload) :relations payload})
                  file-ingest/post-hyperedge!
                  (fn [& args]
                    (swap! hyperedges conj args)
                    {:ok? true})
                  file-ingest/post-hyperedge-doc!
                  (fn [& args]
                    (swap! hyperedges conj args)
                    {:ok? true})]
      (let [result (file-ingest/dispatch! {:path path
                                           :root "/home/joe/code/futon3"
                                           :label "futon3-d"})
            names (set (map :name @entities))
            relation-types (set (map :type @relations))
            pid "baldwin/two-claims-not-one"
            facets #{"conclusion" "context" "if" "however"
                     "then" "because" "next-steps"}]
        (is (= :pattern (:status result)))
        (is (= facets (set (:facets result))))
        (is (= (conj (set (map #(str pid "/" %) facets)) pid) names))
        (is (= (set (map #(str ":pattern/has-" %) facets)) relation-types))
        (is (= 7 (count @relations)))
        (is (not (contains? names (str pid "/counterfactual"))))
        (is (empty? @hyperedges))))))

(deftest clojure-dispatch-keeps-code-ingest-path
  (let [calls (atom [])]
    (with-redefs [file-ingest/collect-repo (fn [_] {:root :context})
                  file-ingest/ingest-one-file!
                  (fn [args]
                    (swap! calls conj args)
                    {:vertices 1 :edges 0 :failed 0})
                  file-ingest/ingest-flexiarg!
                  (fn [_]
                    (throw (ex-info "flexiarg path must not handle Clojure" {})))]
      (let [path "/home/joe/code/futon3c/src/futon3c/watcher/projections/flexiarg.clj"
            result (file-ingest/dispatch! {:path path
                                           :root "/home/joe/code/futon3c"
                                           :label "futon3c-d"})]
        (is (= :ingested (:status result)))
        (is (= path (:path result)))
        (is (= 1 (count @calls)))))))

(deftest relation-batch-failure-is-not-silently-dropped
  (with-redefs [http/post
                (fn [& _]
                  {:status 400 :body "{\"error\":\"missing endpoint\"}"})]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"relation batch write failed"
         (file-ingest/post-relations-batch!
         [{:type ":pattern/has-if" :src "missing" :dst "also-missing"}])))))
