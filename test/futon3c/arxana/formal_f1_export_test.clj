(ns futon3c.arxana.formal-f1-export-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f1b-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})
              provider (artifact/in-memory-provider)]
          (try
            (f node {:graph-store (store-http/http-store
                              (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                              f1/service-principal)
                :artifact-provider provider}
               provider)
            (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(deftest mark-occurrences-use-codepoints-and-do-not-collapse
  (let [text "a😀🈳: b 🈳 c"
        marks (f1a/mark-occurrences "repeat" text)]
    (is (= 2 (count marks)))
    (is (= [[2 3] [7 8]] (mapv #(get-in % [:locator :codepoint-range]) marks)))
    (is (= [:colon :plain] (mapv :form marks)))
    (is (= 2 (count (set (map :id marks)))))))

(deftest corrected-fixture-mark-identities-are-pinned
  (let [plans (f1a/plan-fixtures)]
    (is (= [["arxana:record:sha256:085a3c5cf37febb0bd6579641ad1a24bbe6176dba59c46d821af92886bf82fcf"]
            ["arxana:record:sha256:a20137b594c30f3fd96884dd148d79a9b74112b94f72c8ed413637109195111b"]
            ["arxana:record:sha256:464264de56a6025e7ad01633314d5f2dbe4fb2e2577ae8b2d55b2ae4578bad0b"]
            []]
           (mapv #(mapv :id (:marks %)) plans)))
    (is (= [[{:codepoint-range [0 1]}]
            [{:codepoint-range [0 1]}]
            [{:codepoint-range [147 148]}]
            []]
           (mapv #(mapv :locator (:marks %)) plans)))))

(deftest deterministic-export-and-codepoint-manifest
  (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
        a (export/export plans)
        b (export/export (f1a/add-operator-judgement (f1a/plan-fixtures)))]
    (is (= (vec (:markdown/bytes a)) (vec (:markdown/bytes b))))
    (is (= (:manifest/text a) (:manifest/text b)))
    (is (= export/exporter (get-in a [:manifest :producer])))
    (is (= (:markdown/revision-id a) (get-in a [:manifest :artifact/revision-id])))
    (doseq [[plan entry] (map vector plans (get-in a [:manifest :turns]))]
      (let [source (String. ^bytes (:bytes plan) StandardCharsets/UTF_8)]
        (is (= source (export/codepoint-substring (:markdown a) (first (:range entry))
                                                  (second (:range entry)))))
        (doseq [{:keys [glyph range]} (:marks entry)]
          (is (= glyph (export/codepoint-substring (:markdown a) (first range) (second range)))))
        ;; Independent of the materialiser: the 象 analysis records each
        ;; fragment's text. The successor reading (turn-iWVzvT) covers s1.
        (let [analysis (json/parse-string
                        (slurp (str (System/getProperty "user.home")
                                    "/.emacs-graph/session-turn-analysis/"
                                    (:turn/id plan) ".json.analysis.json"))
                        true)
              texts (vec (for [sentence (:sentences analysis)
                               fragment (:fragments sentence)]
                           (:text fragment)))
              expected (cond-> texts
                         (= "turn-iWVzvT" (:turn/id plan)) (conj (first texts)))]
          (is (= expected
                 (mapv (fn [{:keys [range]}]
                         (export/codepoint-substring (:markdown a) (first range) (second range)))
                       (:interpretations entry)))
              (str (:turn/id plan) ": each interpretation range covers its fragment text")))))))

(deftest private-stack-ingest-and-drift-gate
  (with-stack
    (fn [node cfg provider]
      (let [exported (export/export (f1a/add-operator-judgement (f1a/plan-fixtures)))
            first-run (export/ingest! cfg exported)
            second-run (export/ingest! cfg (export/export
                                            (f1a/add-operator-judgement
                                             (f1a/plan-fixtures))))
            stored (:ok (artifact/revision-bytes provider (:markdown/revision-id exported)))
            manifest-stored (:ok (artifact/revision-bytes provider
                                                           (:manifest/revision-id exported)))]
        (is (:ok (:markdown first-run)))
        (is (:ok (:manifest first-run)))
        (is (= (:markdown/revision-id exported)
               (f1/current-head node "formal-arxana:f1-conversation"))
            "the reserved revision route is the persistence authority")
        (is (= (:markdown/revision-id exported)
               (get-in second-run [:markdown :ok :artifact/revision-id])))
        (is (= (vec (:markdown/bytes exported)) (vec (:bytes stored)))
            "re-exported bytes equal the bytes stored under the named revision")
        (is (= (vec (:manifest/bytes exported)) (vec (:bytes manifest-stored))))))))
