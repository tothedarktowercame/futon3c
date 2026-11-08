(ns futon3c.arxana.formal-f3-generate-test
  (:require [clojure.set]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f3-correspond :as f3b]
            [futon3c.arxana.formal-f3-generate :as f3c]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(def alignment-path
  "/home/joe/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json")

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f3c-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              cfg {:graph-store
                   (store-http/http-store
                    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                    f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)}]
          (try (f cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn get-record [cfg id]
  (get-in (service/dispatch
           cfg {:protocol/version "arxana.v1" :message/id (str "f3c:get:" id)
                :operation :entity/get :principal/id "formal-f3c-test"
                :body {:entity/id id}})
          [:ok :record]))

(defn assessments-of [cfg id]
  (get-in (service/dispatch
           cfg {:protocol/version "arxana.v1" :message/id (str "f3c:ass:" id)
                :operation :graph/query :principal/id "formal-f3c-test"
                :body {:query/type :assessments :subject/id id :query/limit 50}})
          [:ok :result/assessments]))

(deftest generated-synthetic-and-residue-records-remain-distinct
  (with-stack
    (fn [cfg]
      (let [lean-bytes (f3/pinned-bytes "/home/joe/code/apm-lean" f3/fixture-commit
                                       f3/lean-main-path)
            informal-bytes (f3/pinned-bytes "/home/joe/code/apm-lean"
                                            f3/historical-informal-commit
                                            f3/informal-path)
            base (f3/ingest-manifestations!
                  cfg [{:artifact/id "formal-f3:a93J02/lean-main"
                        :media-type "text/x-lean; charset=utf-8" :bytes lean-bytes}
                       {:artifact/id "formal-f3:a93J02/authored-informal"
                        :media-type "text/markdown; charset=utf-8" :bytes informal-bytes}])
            lean-revision (:artifact/revision-id (first base))
            informal-revision (:artifact/revision-id (second base))
            alignment (f3b/read-alignment alignment-path)
            ;; F3b's records exist first, as in the real pipeline, so F3c
            ;; could (wrongly) assess the genuine divergence.
            f3b-rows (f3b/materialise! cfg (f3b/plan alignment informal-revision))
            _ (is (every? #(every? :ok (:writes %)) f3b-rows))
            result (f3c/materialise! cfg {:lean-revision lean-revision
                                          :informal-revision informal-revision
                                          :alignment alignment})
            revisions (vals (:revisions result))]
        (testing "generated, synthetic, witness, and authored artifacts are separate"
          (is (every? #(get-in % [:response :ok]) (:ingested result))
              "every F3c artifact was ingested as its own new artifact")
          (is (empty? (clojure.set/intersection
                       (set (map :artifact/id (:ingested result)))
                       #{"formal-f3:a93J02/lean-main" "formal-f3:a93J02/authored-informal"})))
          (is (= 3 (count (set revisions))))
          (is (not-any? #{informal-revision} revisions))
          (is (not-any? #{lean-revision} revisions))
          (is (= f3c/synthetic-label
                 (get-in result [:mutation :asserted-content :label])))
          (doseq [{:keys [artifact/id media-type]} (:ingested result)
                  :when (not= id f3c/generated-artifact-id)]
            (is (re-find #"SYNTHETIC" id) id)
            (is (re-find #"fixture=SYNTHETIC-negative" media-type) id))
          (is (= f3c/synthetic-label
                 (get-in result [:synthetic-claim :asserted-content :fixture/label]))))
        (testing "generation run records the complete model contract and both revisions"
          (is (= f3c/model (get-in result [:generation :asserted-content :model])))
          (is (= f3c/generation-prompt (get-in result [:generation :asserted-content :prompt])))
          (is (= f3c/generation-configuration
                 (get-in result [:generation :asserted-content :configuration])))
          (is (re-find #"Model-generated candidate explanation"
                       (get-in result [:generation :asserted-content :output])))
          (is (= lean-revision
                 (get-in result [:generation :endpoints 0 :target :artifact/revision-id])))
          (is (= (get (:revisions result) f3c/generated-artifact-id)
                 (get-in result [:generation :endpoints 1 :target :artifact/revision-id]))))
        (testing "model output remains a candidate"
          (is (= :model (get-in result [:generated-evidence :basis])))
          (is (= :candidate (get-in result [:generated-assessment :verdict])))
          (is (not= :accepted (get-in result [:generated-assessment :verdict]))))
        (testing "fixture author, not kernel success alone, records contradiction"
          (is (= :passed (get-in result [:kernel :attestation :content :outcome])))
          (is (= f3c/synthetic-label
                 (get-in result [:kernel :claim :asserted-content :fixture/label])))
          (is (= f3c/synthetic-label
                 (get-in result [:kernel :attestation :content :fixture/label])))
          (is (= "codex-21" (get-in result [:contradiction :actor])))
          (is (= :fixture-author (get-in result [:contradiction :basis])))
          (is (= :contradicted (get-in result [:contradiction :standing])))
          (is (= (get-in result [:kernel :attestation :id])
                 (get-in result [:contradiction :kernel/attestation-id])))
          (is (some? (get-record cfg (get-in result [:kernel :attestation :id]))))
          (let [stored (assessments-of cfg (get-in result [:synthetic-claim :id]))
                kernel-id (get-in result [:kernel :attestation :id])]
            (is (= 1 (count stored)))
            (is (= [{:attestation/id kernel-id}] (:source-refs (first stored)))
                "the stored contradiction cites the kernel attestation")
            (is (= :contradicted (:standing (first stored))))))
        (testing "ambiguous and unaligned residues are public records"
          (is (seq (get-in result [:residues :ambiguous])))
          (is (= ["tsum_alternating_harmonic_mul_pow"]
                 (mapv #(get-in % [:endpoints 0 :target :declaration/name])
                       (get-in result [:residues :unaligned]))))
          (doseq [record (concat (get-in result [:residues :ambiguous])
                                 (get-in result [:residues :unaligned]))]
            (is (= record (get-record cfg (:id record))))))
        (testing "all writes succeeded and F3b divergence stays non-contradicted"
          (is (every? :ok (:writes result)))
          (is (:ok (get-in result [:kernel :claim/write])))
          (is (:ok (get-in result [:kernel :evidence/write])))
          (let [divergence (first (filter :divergence?
                                          (:records (f3b/plan alignment informal-revision))))]
            (is (= false (get-in divergence [:evidence :content :contradiction?])))
            (is (= [["joe" :accepted]]
                   (mapv (juxt :actor :verdict)
                         (assessments-of cfg (get-in divergence [:claim :id]))))
                "the genuine divergence keeps only Joe's acceptance")
            (is (= :unverified (get-in divergence [:evidence :standing])))))))))
