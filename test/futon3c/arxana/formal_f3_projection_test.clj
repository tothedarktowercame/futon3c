(ns futon3c.arxana.formal-f3-projection-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f3-correspond :as f3b]
            [futon3c.arxana.formal-f3-generate :as f3c]
            [futon3c.arxana.formal-f3-relocation :as relocation]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(def alignment-path "/home/joe/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json")
(defn req [id op body] {:protocol/version "arxana.v1" :message/id id :operation op
                        :principal/id "formal-f3d-test" :body body})
(defn get-record [cfg id] (get-in (service/dispatch cfg (req (str "get:" id) :entity/get
                                                             {:entity/id id})) [:ok :record]))
(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f3d-" ".db") _ (.delete db)]
    (try (with-open [node (xtn/start-node)]
           (let [s (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                             :arxana-index-path (.getPath db)})
                 provider (artifact/in-memory-provider)
                 cfg {:graph-store (store-http/http-store
                                    (str "http://127.0.0.1:" (.getPort (.getAddress s)))
                                    f1/service-principal)
                      :artifact-provider provider}]
             (try (f cfg provider) (finally (f1-server/stop-server! s)))))
         (finally (.delete db)))))

(defn prepare! [cfg provider]
  (let [informal (f3/pinned-bytes "/home/joe/code/apm-lean" f3/historical-informal-commit
                                  f3/informal-path)
        lean (f3/pinned-bytes "/home/joe/code/apm-lean" f3/fixture-commit f3/lean-main-path)
        base (f3/ingest-manifestations! cfg [{:artifact/id "formal-f3:a93J02/authored-informal"
                                             :role :authored-informal
                                             :media-type "text/markdown; charset=utf-8" :bytes informal}
                                            {:artifact/id "formal-f3:a93J02/lean-main"
                                             :role :readable-lean
                                             :media-type "text/x-lean; charset=utf-8" :bytes lean}])
        informal-rid (:artifact/revision-id (first base)) lean-rid (:artifact/revision-id (second base))
        alignment (f3b/read-alignment alignment-path) plan (f3b/plan alignment informal-rid)
        rows (f3b/materialise! cfg plan)
        c (f3c/materialise! cfg {:lean-revision lean-rid :informal-revision informal-rid
                                 :alignment alignment})
        location (f3/declaration-location "alternating_harmonic_eq_log_two" lean-rid)
        lw (service/dispatch cfg (req "f3d:location" :clusion/assert {:record location}))
        step->claims (into {} (for [[step rs] (group-by :step/id (:records plan))]
                                [step (:claim (first rs))]))
        exp (relocation/exported (String. ^bytes informal "UTF-8") informal-rid step->claims)
        edited (relocation/edit-successor exp)
        successor (relocation/ingest-successor! cfg informal-rid edited)
        target-rid (get-in successor [:ok :artifact/revision-id])
        relocated (relocation/relocate! cfg provider exp target-rid)
        corr (vec (concat
                   (for [r (:records plan)] {:record/id (get-in r [:claim :id])
                                             :step/id (:step/id r)
                                             :class (if (:divergence? r) :divergence :genuine)})
                   [{:record/id (get-in c [:generated-claim :id]) :step/id "generated" :class :generated}
                    {:record/id (get-in c [:synthetic-claim :id]) :step/id "SYNTHETIC" :class :synthetic}]))
        manifestations (vec (concat
                             (for [x base] {:artifact/id (:artifact/id x)
                                           :artifact/revision-id (:artifact/revision-id x)
                                           :role (:role x)
                                           :record/ids (vec (cons (:id location) (map :record/id corr)))})
                             (for [x (:ingested c)] {:artifact/id (:artifact/id x)
                                                    :artifact/revision-id (:artifact/revision-id x)
                                                    :role (if (= (:artifact/id x) f3c/synthetic-artifact-id)
                                                            :synthetic-negative :generated)
                                                    :synthetic/label (when (= (:artifact/id x) f3c/synthetic-artifact-id)
                                                                       :SYNTHETIC-negative)
                                                    :record/ids (vec (cons
                                                                      (if (= (:artifact/id x) f3c/synthetic-artifact-id)
                                                                        (get-in c [:mutation :id])
                                                                        (get-in c [:generation :id]))
                                                                      (map :record/id corr)))})))
        manifest {:manifest/version 1 :correspondences corr :manifestations manifestations}
        mi (service/dispatch cfg (req "f3d:manifest" :revision/ingest
                                      {:artifact/id "formal-f3:projection-manifest"
                                       :bytes (pr-str manifest) :media-type "application/edn"
                                       :expected/revision-id :absent}))]
    {:informal-rid informal-rid :lean-rid lean-rid :location location :location-write lw
     :plan plan :rows rows :c c :target-rid target-rid :relocated relocated
     :manifest-rid (get-in mi [:ok :artifact/revision-id])}))

(deftest f3d-relocation-and-public-projections
  (with-stack
    (fn [cfg provider]
      (let [p (prepare! cfg provider)
            project #(get-in (service/dispatch cfg (req (str "project:" (name %)) %
                                                        {:manifest/revision-id (:manifest-rid p)})) [:ok])
            outline (project :formal-f3/correspondence)
            manifestations (project :formal-f3/manifestations)
            statuses (frequencies (map #(get-in % [:resolution :result/status])
                                       (get-in p [:relocated :result :results])))]
        (testing "relocations are stored evidence and claims remain immutable"
          (is (pos? (get statuses :resolved 0)))
          (is (pos? (get statuses :ambiguous 0)))
          (is (pos? (get statuses :missing 0)))
          (doseq [w (get-in p [:relocated :writes])]
            (let [id (get-in w [:ok :record/id]) stored (get-record cfg id)]
              (is (= id (:id stored)))
              (is (= (:informal-rid p) (get-in stored [:content :source/revision-id])))
              (is (= (:target-rid p) (get-in stored [:content :target/revision-id])))))
          (doseq [r (:records (:plan p))]
            (is (= (:claim r) (get-record cfg (get-in r [:claim :id]))))))
        (testing "Lean identity and declaration location are unchanged"
          (is (= (:location p) (get-record cfg (get-in p [:location :id]))))
          (is (= (:lean-rid p) (get-in (get-record cfg (get-in p [:location :id]))
                                       [:endpoints 1 :target :artifact/revision-id]))))
        (testing "views preserve IDs, synthetic boundary, and divergence"
          (let [of (:projection/fragments outline)
                sf (first (filter #(= :synthetic (:class %)) of))
                df (first (filter #(= :methodological-divergence (:relation/kind %)) of))]
            (is (= :SYNTHETIC-negative (:synthetic/label sf)))
            (is (some #(= :contradicted (:standing %)) (:assessments sf)))
            (is (false? (:divergence/contradicted? df)))
            (is (every? (set (mapcat :record/ids (:projection/fragments manifestations)))
                        (map :correspondence/id of)))
            (doseq [view [outline manifestations] fragment (:projection/fragments view)
                    id (:record/ids fragment)]
              (is (= id (:id (get-record cfg id)))))))))))
