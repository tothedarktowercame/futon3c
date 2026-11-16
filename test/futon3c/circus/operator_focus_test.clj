(ns futon3c.circus.operator-focus-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.operator-focus :as sut]
            [futon3c.circus.hou-store :as hou-store]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.turn-mention-writer :as writer]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.xiang.turn-store :as turns]))

(def items [{:id "M-a" :state :open} {:id "M-b" :state :open}
            {:id "E-c" :state :closed :closed-at "2026-01-01T05:00:00Z" :closing-commit "toy-commit"}])
(def base {:until "2026-01-01T08:00:00Z" :window-hours 6 :items items :needs [["M-a" "M-b"]]})
(defn turn [id at seat named tag]
  {:id id :created-at (str "2026-01-01T" at ":00:00Z") :seat seat :named named :mission-id tag
   :pin [[id "toy-digest"]]})

(deftest names-override-tags-and-seats-aggregate
  (let [out (sut/focus-window (assoc base :turns [(turn "a" "03" "seat-a" ["M-a"] "M-b")
                                                 (turn "b" "04" "seat-b" [] "M-a")
                                                 (turn "c" "05" "seat-c" [] nil)
                                                 (turn "d" "06" "seat-a" ["E-c"] nil)]))]
    (is (= [{:item "M-a" :turn-count 2 :seats ["seat-a" "seat-b"] :most-recent "2026-01-01T04:00:00Z"}]
           (get-in out [:report :focus])))
    (is (= [{:seat "seat-c" :turn-count 1}] (get-in out [:report :no-mission])))
    (is (= ["E-c"] (mapv :item (get-in out [:report :just-closed]))))
    (is (= {"M-a" :focus "M-b" :associated} (get-in out [:focus-input :relations])))
    (is (= 4 (count (get-in out [:focus-input :evidence-pin]))))
    (is (= #{:relations :evidence-pin :header-ids} (set (keys (:focus-input out)))))))

(deftest window-boundaries-and-ranking
  (let [out (sut/focus-window (assoc base :turns [(turn "excluded" "02" "s" ["M-a"] nil)
                                                 (turn "included" "08" "s" ["M-b"] nil)
                                                 (turn "future" "09" "s" ["M-a"] nil)
                                                 (turn "earlier" "03" "s" ["M-a"] nil)]))]
    (is (= 2 (get-in out [:report :turns])))
    (is (= ["M-b" "M-a"] (mapv :item (get-in out [:report :focus]))))
    (is (= #{"included" "earlier"} (set (map first (get-in out [:focus-input :evidence-pin])))))))

(deftest closure-is-a-status-transition
  (is (sut/closing-change "**Status:** OPEN" "**Status:** DONE (toy)"))
  (is (sut/closing-change "Status: OPEN" "Status: CLOSED"))
  (is (not (sut/closing-change "Status: DONE old words" "Status: DONE new words")))
  (is (not (sut/closing-change "Status: OPEN" "Status: OPEN\nOrdinary prose says CLOSED"))))

(deftest stored-headers-drive-window-and-pending-blocks-mission-fallback
  (with-redefs [inventory/current-items (fn [_] items)
                sut/with-closures (fn [xs _ _] xs)
                sut/with-gcc-successors (fn [_ xs _] xs)
                turns/list-records (fn [& _]
                                     [{:id "headered" :record {:origin "operator" :created_at "2026-01-01T03:00:00Z"}}
                                      {:id "pending" :record {:origin "operator" :created_at "2026-01-01T04:00:00Z"}}
                                      {:id "tagged-pending" :record {:origin "operator" :created_at "2026-01-01T05:00:00Z"
                                                                     :mission_id "M-b"}}])
                hou-store/headers-for-turn
                (fn [_ turn]
                  (if (= "headered" (:id turn))
                    {:headers [{:id "hou-header-1" :asserted-content
                                {:hou/turn "headered" :hou/relations [["M-a" :focus] ["M-b" :associated]]}}]}
                    {:pending :header-absent}))
                writer/evidence-text (fn [& _] (throw (AssertionError. "evidence text path called")))]
    (let [out (sut/operator-focus {:turn-store {:dir "/invented"}
                                   :read-needs (fn [_] {:readings [] :basis-records []})} base)]
      (is (= ["M-a"] (mapv :item (get-in out [:report :focus]))))
      (is (= {:headered 1 :pending 2 :mission-tag 0 :no-mission 0}
             (get-in out [:report :turn-kinds])))
      (is (= ["hou-header-1"] (get-in out [:focus-input :header-ids])))
      (is (= 2 (count (filter #(= :pending (:reason %)) (get-in out [:report :notices]))))))))

(deftest closure-time-comes-from-git-not-file-mtime
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "focus-git" (make-array java.nio.file.attribute.FileAttribute 0)))
        doc (io/file root "holes" "M-toy.md")
        run (fn [& args] (let [r (apply shell/sh "git" "-C" (str root) args)]
                          (is (zero? (:exit r))) r))
        commit (fn [at]
                 (run "add" "holes/M-toy.md")
                 (run "-c" "user.name=Toy" "-c" "user.email=toy@example.invalid"
                      "-c" "commit.gpgsign=false" "commit" "-qm" "fixture"
                      :env (assoc (into {} (System/getenv)) "GIT_AUTHOR_DATE" at "GIT_COMMITTER_DATE" at)))]
    (try
      (.mkdirs (.getParentFile doc)) (run "init" "-q")
      (spit doc "# M-toy\nStatus: OPEN\n") (commit "2026-01-01T01:00:00Z")
      (spit doc "# M-toy\nStatus: DONE\n") (commit "2026-01-01T04:00:00Z")
      (let [xs (sut/with-closures [{:id "M-toy" :path (str doc) :state :closed}]
                                  "2026-01-01T08:00:00Z" 6)]
        (is (= (sut/millis "2026-01-01T04:00:00Z") (sut/millis (:closed-at (first xs)))))
        (is (= 40 (count (:closing-commit (first xs))))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))

(deftest superseded-tags-follow-successors-but-names-do-not
  (let [old {:id "M-old" :state :closed :successors ["M-new"]}
        opts (assoc base :items [old {:id "M-new" :state :open}]
                    :turns [(turn "tagged" "03" "seat-a" [] "M-old")
                            (turn "named" "04" "seat-b" ["M-old"] "M-new")])
        out (sut/focus-window opts)]
    (is (= [{:item "M-new" :turn-count 1 :seats ["seat-a"] :most-recent "2026-01-01T03:00:00Z"}]
           (get-in out [:report :focus])))
    (is (= 1 (:turn-count (first (get-in out [:report :notices])))))
    (is (= "M-old" (:item (first (get-in out [:report :notices]))))))
  (let [out (sut/focus-window
             (assoc base :items [{:id "M-a" :state :closed :successors ["M-b"]}
                                {:id "M-b" :state :closed :successors ["M-a"]}]
                    :turns [(turn "cycle" "03" "seat-a" [] "M-a")]))]
    (is (= :supersession-cycle (:reason (first (get-in out [:report :notices])))))
    (is (empty? (get-in out [:report :focus]))))
  (let [by-id (into {} (for [n (range 10)]
                        [(str "M-" n) {:successors [(str "M-" (inc n))]}]))]
    (is (= :supersession-limit (get-in (sut/resolve-tag by-id "M-0") [:notice :reason]))))
  (is (= {:item "M-c"}
         (sut/resolve-tag {"M-a" {:successors ["M-b"]}
                           "M-b" {:successors ["M-c"]}
                           "M-c" {:successors []}}
                          "M-a")))
  (is (= :supersession-ambiguous
         (get-in (sut/resolve-tag {"M-a" {:successors ["M-b" "M-c"]}} "M-a")
                 [:notice :reason]))))

(deftest gcc-successors-ignore-stale-attestations-and-never-read-files
  (let [old {:id "M-old" :source {:sha256 "current"}}
        new {:id "M-new" :source {:sha256 "new"}}
        old-ref (inventory/item-record "M-old") new-ref (inventory/item-record "M-new")
        claim (records/make-clusion
               {:type-revision [:supersedes 1]
                :endpoints [{:role :successor :kind :clusion :target {:clusion/id (:id new-ref)}}
                            {:role :predecessor :kind :clusion :target {:clusion/id (:id old-ref)}}]
                :asserted-content {}})
        att (records/make-attestation
             {:subject-clusion-id (:id claim) :basis :exact :standing :verified :coverage :complete
              :producer "futon3c/item-inventory"
              :method {:name :supersession-census :version 1}
              :content {:file/sha256 "current"}})
        entities {(:id claim) claim (:id att) att}
        cfg {:graph-store :stub
             :items-by-id {"M-old" old "M-new" new}
             :item-id-by-clusion {(:id old-ref) "M-old" (:id new-ref) "M-new"}}]
    (with-redefs [clojure.core/slurp (fn [& _] (throw (AssertionError. "read mission file")))
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse (do (is (= #{:edge-type :endpoint-role :endpoint-id :limit :order :query/type}
                                                (set (keys q))))
                                        {:ok {:edge-ids [(:id claim)]}})
                      :attestations (do (is (= #{:subject-id :limit :query/type} (set (keys q))))
                                        {:ok {:attestation-ids [(:id att)]}})))
                  store/get-entity (fn [_ id] {:ok (entities id)})]
      (is (= ["M-new"] (sut/read-successors cfg old)))
      (is (= [] (sut/read-successors cfg (-> old
                                             (assoc :status-line "OPEN")
                                             (assoc-in [:source :sha256] "changed"))))
          "un-superseding changes the source SHA, so the immutable old claim is stale"))))

(deftest two-gcc-claims-for-one-predecessor-are-ambiguous
  (let [ids ["M-b" "M-c"]
        refs (into {} (map (fn [id] [id (inventory/item-record id)]))
                   (conj ids "M-a"))
        claims (mapv (fn [id]
                       (records/make-clusion
                        {:type-revision [:supersedes 1]
                         :endpoints [{:role :successor :kind :clusion
                                      :target {:clusion/id (:id (refs id))}}
                                     {:role :predecessor :kind :clusion
                                      :target {:clusion/id (:id (refs "M-a"))}}]
                         :asserted-content {}})) ids)
        atts (mapv #(records/make-attestation
                     {:subject-clusion-id (:id %) :basis :exact :standing :verified :coverage :complete
                      :producer "futon3c/item-inventory"
                      :method {:name :supersession-census :version 1}
                      :content {:file/sha256 "same"}}) claims)
        entities (into {} (map (juxt :id identity)) (concat claims atts))
        cfg {:graph-store :stub
             :item-id-by-clusion (into {} (map (fn [[id r]] [(:id r) id])) refs)}]
    (with-redefs [store/bounded-query!
                  (fn [_ q]
                    (if (= :role-reverse (:query/type q))
                      {:ok {:edge-ids (mapv :id claims)}}
                      {:ok {:attestation-ids
                            [(:id (first (filter #(= (:subject-id q) (:subject-clusion-id %)) atts)))]}}))
                  store/get-entity (fn [_ id] {:ok (entities id)})]
      (let [successors (sut/read-successors cfg {:id "M-a" :source {:sha256 "same"}})]
        (is (= ids successors))
        (is (= :supersession-ambiguous
               (get-in (sut/resolve-tag {"M-a" {:successors successors}} "M-a")
                       [:notice :reason])))))))

(deftest gcc-successors-follow-chains-from-turn-roots-only
  (let [graph {"M-a" ["M-b"] "M-b" ["M-c"] "M-c" [] "M-x" ["M-y"]}
        reads (atom [])
        items (mapv (fn [id] {:id id}) ["M-a" "M-b" "M-c" "M-x" "M-y"])]
    (with-redefs [sut/read-successors (fn [_ item] (swap! reads conj (:id item)) (graph (:id item)))]
      (let [out (into {} (map (juxt :id :successors))
                      (sut/with-gcc-successors {} items ["M-a" "M-a" "M-unknown"]))]
        (is (= {"M-a" ["M-b"] "M-b" ["M-c"] "M-c" [] "M-x" [] "M-y" []} out))
        (is (= {:item "M-c"} (sut/resolve-tag (into {} (map (fn [[k v]] [k {:successors v}])) out) "M-a")))
        (is (= ["M-a" "M-b" "M-c"] @reads) "each reachable item is read once; unrelated items are not read")))))
