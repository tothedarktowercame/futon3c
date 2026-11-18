(ns futon3c.arxana.giraffe-needs-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.giraffe-needs :as sut]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]))

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
                  store/bounded-query! (fn [_ _] {:ok {:status :ok :attestation-ids []}})
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

(deftest unchanged-reading-is-not-reattested-and-a-changed-pin-is
  (let [{:keys [run batch]} (fixture)
        entities (atom {}) calls (atom [])
        cfg {:graph-store :stub-dedup :item-ids ["M-a" "M-b"]
             :written-at "2026-11-16T20:00:00Z" :pause-ms 0}
        query (fn [_ q]
                (case (:query/type q)
                  :role-reverse
                  {:ok {:status :ok :truncated? false
                        :edge-ids (->> @entities vals
                                       (filter #(= [:needs 1] (:type-revision %)))
                                       (mapv :id))}}
                  :attestations
                  {:ok {:status :ok :attestation-ids
                        (->> @entities vals
                             (filter #(= (:subject-id q) (:subject-clusion-id %)))
                             (mapv :id))}}))]
    (with-redefs [sut/current-run (fn [_] run)
                  store/bounded-query! query
                  store/get-entity (fn [_ id] {:ok (get @entities id)})
                  http/append-batch!
                  (fn [_ _ rows]
                    (swap! calls conj rows)
                    (swap! entities into (map (comp (juxt :id identity) :record) rows))
                    {:ok {}})]
      (is (= 1 (:written (sut/write! cfg))))
      (is (zero? (:written (sut/write! cfg))))
      (is (= 1 (count @calls)) "the unchanged second pass appends nothing")
      (let [line (json/generate-string (assoc (json/parse-string (first (str/split-lines (slurp batch))) true)
                                               :revision 2))
            pin {:batch (str batch) :line 1
                 :sha256 (digest/sha256-hex (.getBytes line "UTF-8"))}
            changed (assoc-in run [:members "reading-a" :record-pin] pin)]
        (spit batch (str line "\n"))
        (with-redefs [sut/current-run (constantly changed)]
          (is (= 1 (:written (sut/write! (assoc cfg :written-at "2026-11-16T20:01:00Z"))))))
        (is (= 2 (count @calls)) "a changed reading line gets one new attestation")
        (with-redefs [sut/party-run (constantly "run-2")]
          (is (= (:sha256 pin)
                 (get-in (sut/read-needs-gcc
                          (assoc cfg :member-pins {"reading-a" pin}))
                         [:readings 0 :record :line-sha256]))
              "the reader selects the changed pin, not the older attestation"))))))

(deftest reader-finds-current-pin-behind-sixty-older-attestations
  (let [{:keys [run]} (fixture)
        p (with-redefs [sut/current-run (constantly run)]
            (sut/plan {:item-ids ["M-a" "M-b"] :written-at "2026-11-16T20:00:00Z"}))
        rows (get-in p [:readings 0 :rows])
        claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))
        current (:record (first (filter #(= :evidence/append (:operation %)) rows)))
        old (mapv (fn [n]
                    (-> current
                        (assoc :id (str "att-old-" n))
                        (assoc-in [:content :record-pin :sha256] (str "old-sha-" n))
                        (assoc-in [:content :written-at] (format "2026-11-16T19:%02d:00Z" n))))
                  (range 60))
        entities (into {(:id claim) claim (:id current) current}
                       (map (juxt :id identity) old))]
    (with-redefs [sut/party-run (constantly "run-2")
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse {:ok {:status :ok :truncated? false :edge-ids [(:id claim)]}}
                      :attestations {:ok {:status :ok
                                          :attestation-ids (conj (mapv :id old) (:id current))}}))
                  store/get-entity (fn [_ id] {:ok (get entities id)})]
      (is (= [["M-a" "M-b"]]
             (mapcat :needs
                     (:readings (sut/read-needs-gcc
                                 {:graph-store :stub-sixty :item-ids ["M-a" "M-b"]
                                  :member-pins {"reading-a" (get-in current [:content :record-pin])}}))))))))

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
        planned (:record (first (filter #(= :evidence/append (:operation %)) rows)))
        ;; The older attestation is from an earlier run, so picking it (by
        ;; index order) is visible: its edge is not the party run's.
        old (assoc-in planned [:content :run] "run-0")
        new (assoc-in planned [:content :written-at] "2026-11-16T20:01:00Z")
        new (assoc new :id "att-new")
        entities {(:id claim) claim (:id old) old (:id new) new}
        run-order
        (fn [att-ids]
          (with-redefs [sut/party-run (constantly "run-1")
                        store/bounded-query!
                        (fn [_ q]
                          (case (:query/type q)
                            :role-reverse {:ok {:status :ok :truncated? false
                                                :edge-ids (if (= (:endpoint-id q)
                                                                 (:id (inventory/item-record "M-a")))
                                                            [(:id claim)] [])}}
                            :attestations (do (is (= (:id claim) (:subject-id q)))
                                              {:ok {:status :ok :attestation-ids att-ids}})))
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

(deftest party-run-switch-reuses-an-unchanged-pinned-reading
  ;; Run identity is not the reading identity. A new party run with the same
  ;; record pin reuses the existing attestation and is not served from cache.
  (let [{:keys [run]} (fixture)
        p (with-redefs [sut/current-run (constantly run)]
            (sut/plan {:item-ids ["M-a" "M-b"] :written-at "2026-11-16T20:00:00Z"}))
        rows (get-in p [:readings 0 :rows])
        claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))
        att (:record (first (filter #(= :evidence/append (:operation %)) rows)))
        party (atom "run-2") queries (atom 0)
        pin (get-in att [:content :record-pin])
        cfg {:graph-store :stub-run-scope :item-ids ["M-a" "M-b"] :cache-needs? true
             :member-pins {"reading-a" pin}}]
    (with-redefs [sut/party-run (fn [_] @party)
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse (do (swap! queries inc)
                                        {:ok {:status :ok :truncated? false
                                              :edge-ids (if (= (:endpoint-id q)
                                                               (:id (inventory/item-record "M-a")))
                                                          [(:id claim)] [])}})
                      :attestations {:ok {:status :ok :attestation-ids [(:id att)]}}))
                  store/get-entity (fn [_ id] {:ok (get {(:id claim) claim (:id att) att} id)})]
      (is (= [["M-a" "M-b"]] (mapcat :needs (:readings (sut/read-needs-gcc cfg))))
          "run-1's attestation serves run-2 because its reading pin is unchanged")
      (reset! party "run-1")
      (is (= [["M-a" "M-b"]] (mapcat :needs (:readings (sut/read-needs-gcc cfg)))))
      (let [n @queries]
        (sut/read-needs-gcc cfg)
        (is (= n @queries) "same run: cached")))))

(deftest a-reading-with-two-needs-edges-reads-both
  ;; Stamps are strings; live readings carry several edges each.
  (let [{:keys [run]} (fixture)
        run (update-in run [:input :edges] conj
                       {:from "M-a" :to "M-c" :class :closure :reading-member "reading-a"})
        p (with-redefs [sut/current-run (constantly run)]
            (sut/plan {:item-ids ["M-a" "M-b" "M-c"] :written-at "2026-11-16T20:00:00Z"}))
        rows (get-in p [:readings 0 :rows])
        claims (map :record (filter #(= :clusion/assert (:operation %)) rows))
        atts (map :record (filter #(= :evidence/append (:operation %)) rows))
        att-of (into {} (map (fn [a] [(:subject-clusion-id a) a]) atts))
        entities (merge (into {} (map (juxt :id identity) claims))
                        (into {} (map (juxt :id identity) atts)))]
    (with-redefs [sut/party-run (constantly "run-1")
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse {:ok {:status :ok :truncated? false
                                          :edge-ids (if (= (:endpoint-id q)
                                                           (:id (inventory/item-record "M-a")))
                                                      (mapv :id claims) [])}}
                      :attestations {:ok {:status :ok :attestation-ids [(:id (att-of (:subject-id q)))]}}))
                  store/get-entity (fn [_ id] {:ok (get entities id)})]
      (is (= #{["M-a" "M-b"] ["M-a" "M-c"]}
             (set (mapcat :needs (:readings (sut/read-needs-gcc
                                              {:graph-store :stub-two
                                               :item-ids ["M-a" "M-b" "M-c"]})))))))))

(deftest two-readings-asserting-one-edge-both-keep-it
  ;; Same endpoints and class = one clusion, attested once per reading.
  (let [{:keys [run]} (fixture)
        p (with-redefs [sut/current-run (constantly run)]
            (sut/plan {:item-ids ["M-a" "M-b"] :written-at "2026-11-16T20:00:00Z"}))
        rows (get-in p [:readings 0 :rows])
        claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))
        att-a (:record (first (filter #(= :evidence/append (:operation %)) rows)))
        att-b (-> att-a (assoc :id "att-b") (assoc-in [:content :reading-member] "reading-b")
                  (assoc-in [:content :written-at] "2026-11-16T20:00:01Z"))
        entities {(:id claim) claim (:id att-a) att-a "att-b" att-b}]
    (with-redefs [sut/party-run (constantly "run-1")
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse {:ok {:status :ok :truncated? false
                                          :edge-ids (if (= (:endpoint-id q)
                                                           (:id (inventory/item-record "M-a")))
                                                      [(:id claim)] [])}}
                      :attestations {:ok {:status :ok :attestation-ids ["att-b" (:id att-a)]}}))
                  store/get-entity (fn [_ id] {:ok (get entities id)})]
      (is (= #{"giraffe-reading/reading-a" "giraffe-reading/reading-b"}
             (set (map (comp :id :record)
                       (:readings (sut/read-needs-gcc {:graph-store :stub-shared
                                                       :item-ids ["M-a" "M-b"]})))))))))

(deftest census-skips-what-the-writer-skips-and-verifies-only-missing-endpoints
  ;; CL-47 review: the pass input had 93 members, the writer published 92
  ;; (one edge's endpoint was not an inventory item), so a census over the
  ;; input made every run incomplete.
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "giraffe-census-" (make-array java.nio.file.attribute.FileAttribute 0)))
        census #'sut/census
        verified (atom [])]
    (.mkdirs (io/file dir "run-1"))
    (spit (io/file dir "run-1" "input.edn")
          (pr-str {:edges [{:from "M-a" :to "M-b" :class :closure :reading-member "r-ab"}
                           {:from "M-a" :to "M-gone" :class :closure :reading-member "r-gone"}
                           {:from "M-a" :to "M-b" :class :alternative :reading-member "r-alt"}
                           {:from "M-a" :to "M-new" :class :closure :reading-member "r-new"}]}))
    (with-redefs [inventory/current-items (fn [& _] [{:id "M-a"} {:id "M-b"} {:id "M-new"}])
                  mentions/inventory
                  (fn [_ text] (swap! verified conj text)
                    ;; M-new is in the file census but has no graph record yet.
                    (filterv #(str/includes? text %) ["M-a" "M-b"]))]
      (is (= ["r-ab" "r-new"] (sut/expected-members {:runs-dir (str dir)} "run-1"
                                                     ["M-a" "M-b" "M-new"])))
      (is (= ["r-ab" "r-new"] (census {:runs-dir (str dir)} "run-1" ["r-ab" "r-new"])))
      (is (empty? @verified) "nothing missing: no graph lookups")
      (is (= ["r-ab"] (census {:runs-dir (str dir)} "run-1" ["r-ab"]))
          "an endpoint without a graph record is skipped by the writer, so not expected")
      (is (= 1 (count @verified))))))

(deftest lagging-attestation-index-is-unavailable-not-a-dropped-reading
  ;; CL-46: attestation reads that answered :stale-index came back empty and
  ;; silently dropped readings (70 or 85 of 92).
  (let [run-attestations #'sut/run-attestations
        answers (atom nil)]
    (with-redefs [store/bounded-query! (fn [_ _] (let [a (first @answers)]
                                                   (swap! answers #(or (next %) %)) a))
                  store/get-entity (fn [_ _] {:ok nil})]
      (reset! answers [{:ok {:status :stale-index}}])
      (is (= :needs-unavailable
             (try (run-attestations {:graph-store :stub} {:id "claim"} nil) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (reset! answers [{:ok {:status :stale-index}} {:ok {:status :ok :attestation-ids []}}])
      (is (= [] (vec (run-attestations {:graph-store :stub} {:id "claim"} nil))))
      ;; The query caps silently; a full page may hide the party run's.
      (reset! answers [{:ok {:status :ok :attestation-ids (mapv str (range 200))}}])
      (is (= :needs-unavailable
             (try (run-attestations {:graph-store :stub} {:id "claim"} nil) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))


(deftest needs-cache-keys-on-store-identity-and-keeps-one-run
  ;; CL-55: each default config builds a new HttpClient, so keying on the
  ;; HttpStore object missed the cache every time (45.9 s instead of 2 ms).
  (reset! @#'sut/read-cache {})
  (let [reads (atom 0)
        run (atom "run-1")
        store (fn [] {:base-url "http://store.example" :principal "p" :client (Object.)})
        cfg (fn [] {:graph-store (store) :item-ids ["M-a"] :cache-needs? true})]
    (with-redefs [sut/party-run (fn [_] @run)
                  sut/read-needs-gcc* (fn [_ r] (swap! reads inc) {:readings [] :run r})]
      (sut/read-needs-gcc (cfg))
      (sut/read-needs-gcc (cfg))
      (is (= 1 @reads) "a second config for the same store hits the cache")
      (reset! run "run-2")
      (sut/read-needs-gcc (cfg))
      (is (= 2 @reads))
      (is (= #{"run-2"} (set (map #(peek (key %)) @@#'sut/read-cache)))
          "entries for runs the party moved past are dropped"))))
