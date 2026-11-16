(ns futon3c.circus.operator-focus-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.operator-focus :as sut]
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
    (is (= #{:relations :evidence-pin} (set (keys (:focus-input out)))))))

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

(deftest evidence-quotes-and-read-only-shell
  (with-redefs [inventory/current-items (fn [_] items)
                sut/with-closures (fn [xs _ _] xs)
                turns/list-records (fn [& _]
                                     [{:id "toy" :record {:origin "operator" :created_at "2026-01-01T04:00:00Z"
                                                          :agent_id "seat-a" :evidence_id "e-toy"
                                                          :source_text "QUOTE" :mission_id "M-b" :reply_text "M-a"}}
                                      {:id "agent" :record {:origin "agent" :created_at "2026-01-01T05:00:00Z"}}])]
    (let [out (sut/operator-focus {:turn-store {:dir "/invented"}
                                   :read-evidence (fn [_] {:evidence/body {:text ">>> M-a\n>>>"}})
                                   :read-needs (fn [_] {:readings [] :basis-records []})} base)]
      (is (= ["M-b"] (mapv :item (get-in out [:report :focus]))))
      (is (= ["M-a"] (get-in out [:report :associated])))
      (is (= 1 (get-in out [:report :turns])))
      (is (contains? (set (map first (get-in out [:focus-input :evidence-pin]))) "evidence/e-toy/body/text")))))

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
  (let [old {:id "M-old" :state :closed :status-line "SUPERSEDED; superseded by `M-new`"}
        opts (assoc base :items [old {:id "M-new" :state :open}]
                    :turns [(turn "tagged" "03" "seat-a" [] "M-old")
                            (turn "named" "04" "seat-b" ["M-old"] "M-new")])
        out (sut/focus-window opts)]
    (is (= [{:item "M-new" :turn-count 1 :seats ["seat-a"] :most-recent "2026-01-01T03:00:00Z"}]
           (get-in out [:report :focus])))
    (is (= 1 (:turn-count (first (get-in out [:report :notices])))))
    (is (= "M-old" (:item (first (get-in out [:report :notices]))))))
  (let [out (sut/focus-window
             (assoc base :items [{:id "M-a" :state :closed :status-line "SUPERSEDED; superseded by `M-b`"}
                                {:id "M-b" :state :closed :status-line "SUPERSEDED; superseded by `M-a`"}]
                    :turns [(turn "cycle" "03" "seat-a" [] "M-a")]))]
    (is (= :supersession-cycle (:reason (first (get-in out [:report :notices])))))
    (is (empty? (get-in out [:report :focus]))))
  (let [by-id (into {} (for [n (range 10)]
                        [(str "M-" n) {:status-line (str "SUPERSEDED; superseded by `M-" (inc n) "`")}]))]
    (is (= :supersession-limit (get-in (sut/resolve-tag by-id "M-0") [:notice :reason])))))

(deftest successor-can-be-declared-below-status-line
  (let [f (java.io.File/createTempFile "hou-successor" ".md")]
    (try
      (spit f "Status: SUPERSEDED\nThis item is superseded by `M-new`.\n")
      (let [item (first (sut/with-successors [{:id "M-old" :status-line "SUPERSEDED" :path (str f)}]))]
        (is (= "M-new" (:successor item)))
        (is (= {:item "M-new"} (sut/resolve-tag {"M-old" item} "M-old"))))
      (spit f "Status: SUPERSEDED\nsuperseded by `M-a`; superseded by `M-b`\n")
      (let [item (first (sut/with-successors [{:id "M-old" :status-line "SUPERSEDED" :path (str f)}]))]
        (is (= :supersession-ambiguous (get-in (sut/resolve-tag {"M-old" item} "M-old") [:notice :reason]))))
      (finally (.delete f)))))
