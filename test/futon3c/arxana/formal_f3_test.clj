(ns futon3c.arxana.formal-f3-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]))

(def futon3c-root "/home/joe/code/futon3c")
(def apm-root "/home/joe/worktrees/apm-lean/formal-arxana-f3a")

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f3a-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              provider (artifact/in-memory-provider)
              cfg {:graph-store (store-http/http-store
                                 (str "http://127.0.0.1:"
                                      (.getPort (.getAddress server)))
                                 f1/service-principal)
                   :artifact-provider provider}]
          (try (f cfg provider)
               (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-f3a-test" :body body})

(defn dispatch [cfg mid op body]
  (service/dispatch cfg (request mid op body)))

(deftest f3a-manifestations-kernel-adapter-and-locators
  (with-stack
    (fn [cfg _provider]
      (let [plans (f3/manifestation-plans futon3c-root apm-root)
            ingested (f3/ingest-manifestations! cfg plans)
            by-role (into {} (map (juxt :role identity) ingested))
            run (edn/read-string
                 (slurp "test/resources/arxana/formal_arxana/a93J02-kernel-run.edn"))
            lean (by-role :readable-lean)
            kernel (f3/append-kernel-run! cfg (:artifact/revision-id lean) run)]
        (testing "manifestations stay separate while equal bytes share identity"
          (is (= (count plans) (count (set (map :artifact/id ingested)))))
          (is (every? #(get-in % [:response :ok]) ingested))
          (is (= (get-in by-role [:authored-informal :artifact/revision-id])
                 (get-in by-role [:historical-authored-informal-copy
                                  :artifact/revision-id])))
          (is (not= (get-in by-role [:authored-informal :artifact/revision-id])
                    (get-in by-role [:authored-informal-latex :artifact/revision-id]))
              "the current apm-lean copy is a newer LaTeX-normalised revision")
          (is (= {:tool "latexml_oxide" :version "0.7.5"
                  :log/artifact-id "formal-f3:a93J02/latexml-log"}
                 (get-in by-role [:generated-html :producer])))
          (is (= "3b65504af07eb81231138c5520ce84b56f8360e49a6cca6752314a9606e64133"
                 (->> plans
                      (filter #(= :producer-log (:role %))) first :bytes
                      digest/sha256-hex))))
        (testing "kernel evidence exists only as an adapter-produced attestation"
          (is (= :passed (:outcome run)))
          (is (true? (:mathlib/unchanged? run)))
          (is (:ok (:claim/write kernel)))
          (is (:ok (:evidence/write kernel)))
          (let [stored (get-in (dispatch cfg "kernel-get" :entity/get
                                         {:entity/id (get-in kernel [:attestation :id])})
                               [:ok :record])]
            (is (= :kernel (:basis stored)))
            (is (= "futon3c.arxana/evidence-adapter" (:producer stored)))
            (is (= (set (map :sha256 (:inputs run)))
                   (set (map :sha256 (:source-refs stored)))))))
        (testing "a failed run is retained as contradicted kernel evidence"
          (let [output "synthetic failing gate output\n"
                failed (assoc run :outcome :failed :exit-status 1 :output output
                              :output/sha256
                              (digest/sha256-hex (.getBytes output StandardCharsets/UTF_8)))
                result (f3/append-kernel-run! cfg (:artifact/revision-id lean) failed)
                record (get-in (dispatch cfg "failed-get" :entity/get
                                         {:entity/id (get-in result [:attestation :id])})
                               [:ok :record])]
            (is (:ok (:evidence/write result)))
            (is (= :contradicted (:standing record)))
            (is (= :failed (get-in record [:content :outcome])))))
        (testing "declaration identity ignores ranges; range evidence is revision-specific"
          (let [source (String. ^bytes (:bytes (first (filter #(= :readable-lean (:role %)) plans)))
                                StandardCharsets/UTF_8)
                edited (str "\n\n" source)
                original-rid (:artifact/revision-id lean)
                edit-response (dispatch
                               cfg "lean-whitespace-edit" :revision/ingest
                               {:artifact/id (:artifact/id lean) :bytes edited
                                :media-type "text/x-lean; charset=utf-8"
                                :expected/revision-id original-rid})
                edited-rid (get-in edit-response [:ok :artifact/revision-id])
                name "alternating_harmonic_eq_log_two"
                before-claim (f3/declaration-location name original-rid)
                after-claim (f3/declaration-location name edited-rid)
                before-range (f3/declaration-range source name)
                after-range (f3/declaration-range edited name)
                before-evidence (f3/declaration-range-attestation
                                 before-claim original-rid before-range)
                after-evidence (f3/declaration-range-attestation
                                after-claim edited-rid after-range)]
            (is (:ok edit-response) (pr-str edit-response))
            (is (= (get-in before-claim [:endpoints 0 :target :declaration/id])
                   (get-in after-claim [:endpoints 0 :target :declaration/id])))
            (is (not= (:id before-claim) (:id after-claim)))
            (is (= (mapv #(+ 2 %) before-range) after-range))
            (is (not= (:id before-evidence) (:id after-evidence)))))))))
