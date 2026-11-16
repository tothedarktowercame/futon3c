(ns futon3c.circus.hou-store-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.arxana.digest :as digest]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.circus.hou :as hou]
            [futon3c.circus.hou-store :as sut]
            [futon3c.circus.hou-test :as toy]
            [futon3c.xiang.turn-store :as turns]))

(def turn {:id "toy" :evidence_id "toy-evidence" :created_at "2026-01-01" :analysis_status "analyzed"
           :source_text "M-a M-b" :reply_text ""})
(def cfg {:read-evidence (fn [_] {:evidence/body {:text "M-a M-b"}}) :graph-store :stub :turn-store {:dir "/invented"} :item-ids ["M-a" "M-b" "M-c" "M-d"]})

(deftest atomic-focus-batch-and-typed-refusal
  (let [calls (atom [])]
    (with-redefs [sut/snapshot (fn [_ _] (toy/fixture))
                  http/append-batch! (fn [_ message rows] (swap! calls conj [message rows]) {:ok {}})]
      (let [out (sut/write-focus! cfg turn) rows (second (first @calls))]
        (is (:written out))
        (is (= 1 (count @calls)))
        (is (= [:hou/focus 1] (get-in rows [0 :record :type-revision])))
        (is (every? #(= [:hou/focus-relation 1] (get-in % [:record :type-revision])) (butlast (rest rows))))
        (is (= :evidence/append (:operation (last rows))))
        (is (= :parser (get-in (last rows) [:record :basis])))))
    (reset! calls [])
    (with-redefs [sut/snapshot (fn [_ _] (throw (AssertionError. "readiness must precede I/O")))
                  mentions/inventory (fn [_ _] ["M-a"])
                  http/append-batch! (fn [_ _ rows] (swap! calls conj rows) {:ok {}})]
      (is (= {:failed :turnNotSettled} (sut/write-focus! cfg (assoc turn :analysis_status "requested"))))
      (is (= [:attempt/append] (mapv :operation (first @calls)))))))

(deftest reader-shape-and-link-disagreement
  (let [in (toy/fixture) out (hou/hou-run in) h (:written out) links (:links out)
        rows (into {(:id h) h} (map (juxt :id identity)) links)
        state (atom rows)]
    (with-redefs [sut/newest-headers (fn [_] [h])
                  sut/snapshot (fn [_ _] in)
                  turns/read-record (fn [_ _] turn)
                  store/get-entity (fn [_ id] {:ok (get @state id)})
                  store/bounded-query! (fn [_ q]
                                         {:ok {:status :ok :truncated? false
                                               :total (count (filter #(= [:hou/focus-relation 1] (:type-revision %)) (vals @state)))
                                               :edge-ids (mapv :id (filter #(= (:endpoint-id q)
                                                                                (get-in % [:endpoints (if (= :item (:endpoint-role q)) 1 0) :target :clusion/id]))
                                                                          (filter #(= [:hou/focus-relation 1] (:type-revision %)) (vals @state))))}})]
      (is (= {:relations {"M-a" :focus} :evidence-pin (get-in h [:asserted-content :hou/evidence-pin])}
             (sut/newest-focus-input cfg ["M-a"])))
      (let [old (last links)
            bad (records/make-clusion (assoc-in (dissoc old :id) [:asserted-content :relation] :focus))]
        (swap! state #(-> % (dissoc (:id old)) (assoc (:id bad) bad))))
      (is (nil? (sut/newest-focus-input cfg ["M-a"]))))))

(defn temp-world []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "hou-fixture-" (make-array java.nio.file.attribute.FileAttribute 0)))
        run (io/file root "toy-run") generation (io/file root "generation")
        doc (io/file root "toy.txt") batch (io/file generation "batch-1.jsonl")
        _ (.mkdirs run) _ (.mkdirs generation) _ (spit doc "toy evidence")
        sha #(digest/sha256-hex (.getBytes % "UTF-8"))
        pin {:path (str doc) :sha256 (sha "toy evidence")}
        raw {:id "M-b" :source pin :edges [{:kind "depends-on" :to "M-c" :evidence pin}]}
        line (json/generate-string raw)
        _ (spit batch (str line "\n"))
        input {:edges [{:from :M-b :to :M-c :class :closure :reading-member "M-b"}
                       {:from :M-b :to :M-d :class :alternative :reading-member "M-b"}]
               :graph-coverage {:members [{:id "M-b" :status :read
                                          :record-pin {:batch (str batch) :line 1 :sha256 (sha line)}}]}}
        _ (spit (io/file run "input.edn") (pr-str input))
        _ (spit (io/file run "manifest.edn")
                (pr-str {:files [{:file "input.edn" :sha256 (sha (pr-str input))}]
                         :readings-generation {:path (str generation)}}))
        party (io/file root "party.json")
        _ (spit party (json/generate-string {:sources {:giraffe_run "toy-run"}}))]
    {:root root :doc doc :batch batch :run run
     :cfg (merge cfg {:runs-dir (str root) :party-path (str party)})}))

(defn delete-world! [world]
  (doseq [f (reverse (file-seq (:root world)))] (io/delete-file f)))

(deftest actual-snapshot-port-checks-pins-and-normalized-needs
  (let [w (temp-world)]
    (try
      (let [c (:cfg w) n (sut/read-needs c)
            plans (map #(mentions/plan-field c turn %) [:source_text :reply_text])
            records (into {} (map (fn [r] [(:id r) r])) (map :record (mapcat :records (mapcat :batches plans))))]
        (is (= [["M-b" "M-c"]] (mapcat :needs (:readings n))))
        (with-redefs [store/get-entity (fn [_ id] {:ok (get records id)})]
          (let [snap (sut/snapshot c turn) out (hou/hou-run snap)]
            (is (:written out))
            (is (= 4 (get-in out [:written :asserted-content :hou/hop-bound])))
            (is (some #(= "turn-record/toy" (first %))
                      (get-in out [:written :asserted-content :hou/evidence-pin]))))))
      (spit (:doc w) "changed evidence")
      (is (= :evidenceChanged (try (sut/read-needs (:cfg w)) (catch Exception e (:reason (ex-data e))))))
      (spit (:doc w) "toy evidence")
      (spit (:batch w) "{}\n")
      (is (= :invalidEvidence (try (sut/read-needs (:cfg w)) (catch Exception e (:reason (ex-data e))))))
      (finally (delete-world! w)))))

(deftest readings-outside-the-current-item-inventory-do-not-poison-focus
  (let [w (temp-world)]
    (try
      (let [sha #(digest/sha256-hex (.getBytes % "UTF-8"))
            input-file (io/file (:run w) "input.edn")
            manifest-file (io/file (:run w) "manifest.edn")
            input (read-string (slurp input-file))
            old-raw {:id "E-old-requisition" :source {:path (str (:doc w)) :sha256 (sha "toy evidence")}
                     :edges [{:kind "depends-on" :to "E-old-input"
                              :evidence {:path (str (:doc w)) :sha256 (sha "toy evidence")}}]}
            old-line (json/generate-string old-raw)
            old-pin {:batch (str (:batch w)) :line 2 :sha256 (sha old-line)}
            input (-> input
                      (update :edges conj {:from :E-old-requisition :to :E-old-input
                                           :class :closure :reading-member "E-old-requisition"})
                      (update-in [:graph-coverage :members] conj
                                 {:id "E-old-requisition" :status :read :record-pin old-pin}))
            first-line (first (str/split-lines (slurp (:batch w))))]
        (spit (:batch w) (str first-line "\n" old-line "\n"))
        (spit input-file (pr-str input))
        (spit manifest-file
              (pr-str {:files [{:file "input.edn" :sha256 (sha (pr-str input))}]
                       :readings-generation {:path (str (io/file (:root w) "generation"))}}))
        (let [out (sut/read-needs (:cfg w))]
          (is (= ["giraffe-reading/M-b"] (mapv #(get-in % [:record :id]) (:readings out))))
          (is (not-any? #(= "giraffe-reading/E-old-requisition" (first %))
                        (get-in (hou/hou-run
                                 (assoc (toy/fixture) :readings (:readings out)))
                                [:written :asserted-content :hou/evidence-pin])))))
      (finally (delete-world! w)))))

(deftest newest-empty-focus-does-not-resurrect-old-relations
  (let [in (assoc (toy/fixture "none named" "" []) :at "2026-01-02")
        h (:written (hou/hou-run in))
        old (:written (hou/hou-run (toy/fixture)))]
    (with-redefs [sut/newest-headers (fn [_] [h old])
                  sut/snapshot (fn [_ _] in) turns/read-record (fn [_ _] turn)
                  store/bounded-query! (fn [_ _] {:ok {:status :ok :truncated? false :total 0 :edge-ids []}})]
      (is (= {} (:relations (sut/newest-focus-input cfg ["M-a"])))))))

(deftest header-discovery-uses-evidence-not-target-links
  (let [h (:written (hou/hou-run (toy/fixture))) calls (atom [])]
    (with-redefs [turns/list-records (fn [& _] [{:id "toy" :record turn}])
                  store/bounded-query! (fn [_ q] (swap! calls conj q)
                                         {:ok {:status :ok :truncated? false :edge-ids [(:id h)]}})
                  store/get-entity (fn [_ _] {:ok h})]
      (is (= [h] (sut/newest-headers cfg)))
      (is (= [:hou/focus 1] (:edge-type (first @calls))))
      (is (= :evidence (:endpoint-role (first @calls)))))))

(deftest full-link-set-is-checked-beyond-the-index-page-limit
  (let [base (:written (hou/hou-run (toy/fixture)))
        h (records/make-clusion
           (assoc-in (dissoc base :id) [:asserted-content :hou/relations]
                     (mapv #(vector (str "M-toy-" %) :associated) (range 201))))
        links (hou/relation-links h) rows (into {} (map (juxt :id identity)) links)
        total (atom 201)]
    (with-redefs [store/bounded-query! (fn [& _] {:ok {:status :ok :truncated? true :total @total
                                                       :edge-ids (mapv :id (take 200 links))}})
                  store/get-entity (fn [_ id] {:ok (get rows id)})]
      (is (= links (#'sut/complete-header-links cfg h)))
      (reset! total 202)
      (is (thrown? clojure.lang.ExceptionInfo (#'sut/complete-header-links cfg h))))))

(deftest no-mention-evidence-is-valid-without-stored-reference
  (let [c (assoc cfg :read-evidence (fn [_] {:evidence/body {:text "Nothing named."}})
                 :read-needs (fn [_] {:readings [] :basis-records [] :sources []}))
        calls (atom [])]
    (with-redefs [store/get-entity (fn [& _] (throw (AssertionError. "no reference store read needed")))
                  http/append-batch! (fn [_ _ rows] (swap! calls conj rows) {:ok {}})]
      (let [snap (sut/snapshot c turn)
            out (sut/write-focus! c turn)]
        (is (:written out))
        (is (= [] (get-in out [:written :asserted-content :hou/relations])))
        (is (= 1 (count @calls)))
        (is (= [:clusion/assert :evidence/append] (mapv :operation (first @calls))))
        (is (= {:failed :invalidEvidence}
               (hou/hou-run (assoc-in snap [:text-by-ref (:source-ref snap)] "Changed."))))))))

(deftest quoted-mentions-are-pinned-not-focus
  (let [text ">>> M-a\n>>>\nM-b"
        c (assoc cfg :read-evidence (fn [_] {:evidence/body {:text text}})
                 :read-needs (fn [_] {:readings [{:record {:id "toy-needs"} :needs [["M-b" "M-a"]]}]
                                      :basis-records [] :sources []}))
        plan (mentions/plan-field c turn :source_text)
        stored (into {} (map (juxt :id identity)) (:mentions plan))]
    (with-redefs [store/get-entity (fn [_ id] {:ok (get stored id)})]
      (let [out (hou/hou-run (sut/snapshot c turn))]
        (is (= [["M-b" :focus]] (get-in out [:written :asserted-content :hou/relations])))
        (is (every? (set (map first (get-in out [:written :asserted-content :hou/evidence-pin]))) (keys stored)))
        (is (= {:failed :invalidEvidence}
               (hou/hou-run (update (sut/snapshot c turn) :stored dissoc (first (keys stored))))))))))
