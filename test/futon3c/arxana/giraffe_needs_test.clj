(ns futon3c.arxana.giraffe-needs-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.giraffe-needs :as sut]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]))

(defn- fixture []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "giraffe-needs-" (make-array java.nio.file.attribute.FileAttribute 0)))
        doc (io/file dir "M-a.md")
        batch (io/file dir "batch.jsonl")
        _ (spit doc "source")
        source-sha (digest/sha256-hex (.getBytes "source" "UTF-8"))
        raw {:id "reading-a" :status "read"
             :source {:path (str doc) :sha256 source-sha}
             :edges [{:to "M-b" :intended_class "closure"}]}
        line (json/generate-string raw)
        _ (spit batch (str line "\n"))
        pin {:batch (str batch) :line 1
             :sha256 (digest/sha256-hex (.getBytes line "UTF-8"))}
        run {:run "run-1" :manifest {:readings-generation {:path (str dir)}}
             :input-path (str (io/file dir "input.edn")) :input-sha "input-sha"
             :members {"reading-a" {:id "reading-a" :status :read :record-pin pin}}
             :input {:edges [{:from "M-a" :to "M-b" :class :closure
                              :reading-member "reading-a"}
                             {:from "M-a" :to "M-c" :class :alternative
                              :reading-member "reading-a"}
                             {:from "M-a" :to "M-missing" :class :closure
                              :reading-member "reading-a"}]}}]
    {:run run :batch batch}))

(deftest writer-batches-by-reading-with-canonical-direction-and-skips
  (let [{:keys [run]} (fixture) calls (atom [])
        cfg {:graph-store :stub :item-ids ["M-a" "M-b"] :written-at "2026-11-16T20:00:00Z"}]
    (with-redefs [sut/current-run (constantly run)
                  http/append-batch! (fn [_ id rows] (swap! calls conj [id rows]) {:ok {}})]
      (let [out (sut/write! cfg)
            rows (second (first @calls))
            claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))]
        (is (= 1 (count @calls)) "one batch for the reading")
        (is (= 1 (:written out)))
        (is (= 1 (:skipped out)) "uninventoried endpoint counted")
        (is (= :closure (get-in claim [:asserted-content :class])))
        (is (= (:id (inventory/item-record "M-a"))
               (get-in claim [:endpoints 0 :target :ref/id])) "from is needing")
        (is (= (:id (inventory/item-record "M-b"))
               (get-in claim [:endpoints 1 :target :ref/id])) "to is supplier")))))

(deftest changed-reading-has-a-new-message-id
  (let [{:keys [run]} (fixture)
        ids (for [stamp ["2026-11-16T20:00:00Z" "2026-11-16T20:01:00Z"]]
              (with-redefs [sut/current-run (constantly run)]
                (get-in (sut/plan {:item-ids ["M-a" "M-b"] :written-at stamp})
                        [:readings 0 :message-id])))]
    (is (= 2 (count (distinct ids))))))

(deftest reader-rebuilds-shape-and-takes-latest-attestation-in-either-order
  (let [{:keys [run]} (fixture)
        p (with-redefs [sut/current-run (constantly run)]
            (sut/plan {:item-ids ["M-a" "M-b"] :written-at "2026-11-16T20:00:00Z"}))
        rows (get-in p [:readings 0 :rows])
        claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))
        old (:record (first (filter #(= :evidence/append (:operation %)) rows)))
        new (assoc-in old [:content :written-at] "2026-11-16T20:01:00Z")
        new (assoc new :id "att-new")
        entities {(:id claim) claim (:id old) old (:id new) new}
        run-order
        (fn [att-ids]
          (with-redefs [store/bounded-query!
                        (fn [_ q]
                          (case (:query/type q)
                            :role-reverse {:ok {:status :ok :truncated? false
                                                :edge-ids (if (= (:endpoint-id q)
                                                                 (:id (inventory/item-record "M-a")))
                                                            [(:id claim)] [])}}
                            :attestations (do (is (= (:id claim) (:subject-id q)))
                                              {:ok {:attestation-ids att-ids}})))
                        store/get-entity (fn [_ id] {:ok (get entities id)})]
            (sut/read-needs-gcc {:graph-store :stub :item-ids ["M-a" "M-b"]})))]
    (doseq [order [[(:id old) (:id new)] [(:id new) (:id old)]]]
      (let [out (run-order order)]
        (is (= [["M-a" "M-b"]] (mapcat :needs (:readings out))))
        (is (= "reading-a" (get-in out [:readings 0 :record :reading :id])))
        (is (= "input-sha" (get-in out [:basis-records 0 :sha256])))))))

(deftest backfill-dry-run-never-writes
  (let [{:keys [run]} (fixture)]
    (with-redefs [sut/current-run (constantly run)
                  http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
      (is (= {:would-write 1 :empty 0}
             (:outcomes (sut/backfill! {:item-ids ["M-a" "M-b"]} {:write? false})))))))
