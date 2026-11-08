(ns futon3c.arxana.formal-f1-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (with-open [node (xtn/start-node)]
    (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"})]
      (try
        (f node {:graph-store (store-http/http-store
                               (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                               f1/service-principal)
                 :artifact-provider (artifact/in-memory-provider)})
        (finally (f1-server/stop-server! server))))))

(deftest fixture-plans-preserve-c1-golden-identities
  (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
        golden (:f1 (edn/read-string
                     (slurp "test/resources/arxana/formal_arxana/c1/golden-records.edn")))
        ids (set (mapcat #(map :id (:claims %)) plans))]
    (doseq [{:keys [expected-id]} golden]
      (is (ids expected-id)))))

(deftest materialises-real-private-stack-idempotently
  (with-stack
    (fn [node cfg]
      (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
            first-run (f1a/materialize! cfg plans)
            ;; A re-run is a fresh process: plan again from the files.
            replan (f1a/add-operator-judgement (f1a/plan-fixtures))
            second-run (f1a/materialize! cfg replan)
            iw (last plans)
            [original _ _ successor supersedes] (:claims iw)]
        (is (every? :ok (map :ingest first-run)))
        (is (every? #(every? :ok (:writes %)) first-run))
        (is (every? #(every? :ok (:writes %)) second-run))
        (is (every? #(f1/outcome-by-message node (str "f1a:" (:id %)))
                    (mapcat #(concat (:claims %) (:resolutions %) (:attestations %)
                                     (:assessments %) (:marks %) (:references %))
                            plans))
            "every deterministic message id has exactly one durable outcome")
        (is (= original (f1/clusion-by-id node (:id original))))
        (is (= successor (f1/clusion-by-id node (:id successor))))
        (is (= supersedes (f1/clusion-by-id node (:id supersedes))))
        (is (= :ask-action (get-in original [:endpoints 1 :target :intent/id])))
        (is (= :constrain (get-in successor [:endpoints 1 :target :intent/id])))
        (is (= (map #(update % :bytes vec) plans) (map #(update % :bytes vec) replan))
            "planning is deterministic, so a re-run writes nothing new")
        (let [[s1 s2 s3] (:claims iw)
              verdicts (fn [c] (mapv :verdict (f1/assessments-for node (:id c))))]
          (is (= [:rejected] (verdicts s1)) "s1: rejected, once")
          (is (= [:accepted] (verdicts s2)) "s2: accepted, once")
          (is (= [:accepted] (verdicts s3)) "s3: accepted, once")
          (is (= [] (verdicts successor)) "the successor is a candidate, not accepted")
          (is (= original s1) "the rejected original reading is kept")
          (doseq [a (mapcat #(f1/assessments-for node (:id %)) [s1 s2 s3])]
            (is (= "joe" (:actor a)))
            (is (= (:quote f1a/operator-judgement) (:quote a)) "Joe's words verbatim")
            (is (= [{:turn/id "turn-0ozt4x"
                     :evidence/id "emacs-5d1f629e9f6fcd876a973fad309feff9"}]
                   (:source-refs a)))))
        (is (= {:predecessor (:id original) :successor (:id successor)}
               (into {} (map (juxt :role (comp :clusion/id :target))
                             (:endpoints supersedes)))))))))

(deftest operator-quote-matches-the-evidence-text
  ;; Text of evidence emacs-5d1f629e9f6fcd876a973fad309feff9 (futon1b :7073),
  ;; copied 2026-11-08; the quote must not be paraphrased.
  (is (= (str "s2 and s3 are good; s1 is plausible, but \uD83C\uDE32 (constrain) would "
              "probably be better, because \uD83C\uDE38 is typically used for informational "
              "requests, where is this \"please\" isn't really an ask but a boundary")
         (:quote f1a/operator-judgement))))
