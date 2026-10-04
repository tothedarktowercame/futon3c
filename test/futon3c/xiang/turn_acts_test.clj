(ns futon3c.xiang.turn-acts-test
  "The turn->acts adapter against four real consecutive settled turns of
   session 564c8e50-c240-46fc-81ad-55afafbc63ee (claude-17 turns 391-394,
   2026-10-01T13:49-14:03Z), copied unmodified into turn_acts_fixtures/
   with claude-17's replies as text.

   The stretch: in turn 392's reply claude-17 posted the 🈸 offer
   \"Reply 'yes 1' or 'yes 2'\"; Joe accepted option 1 in turn 393
   (\"1\", read as approve) and commit d9d30ead in turn 393's happened
   carried it out. The 🈸 ask-actions of turns 391 (\"read how P11's
   records get written\") and 394 (\"do 1 now and look into 2\") Joe never
   answered."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.logic.xiang :as lx]
            [futon3c.xiang.turn-acts :as ta]))

(def ^:private fixture-dir "test/futon3c/xiang/turn_acts_fixtures")

(def ^:private turn-files
  ["turn-455grB" "turn-xJ4TAP" "turn-I7n9AZ" "turn-HUylGP"])

(defn- read-json [name]
  (json/read-str (slurp (io/file fixture-dir name)) :key-fn keyword))

(defn- fixture-turn [base]
  {:record (read-json (str base ".json"))
   :reading (read-json (str base ".json.analysis.json"))
   :reply (slurp (io/file fixture-dir (str base ".reply.txt")))})

(defn- fixture-turns []
  (mapv (fn [base]
          (let [{:keys [record] :as turn} (fixture-turn base)]
            (assoc turn :commits (ta/happened-commits (:happened_summary record)))))
        turn-files))

(def turn-ids ["claude-17-turn-391" "claude-17-turn-392"
               "claude-17-turn-393" "claude-17-turn-394"])

;; ---------------------------------------------------------------------------

(deftest happened-commits-parse
  (let [t391 (:record (fixture-turn "turn-455grB"))
        t392 (:record (fixture-turn "turn-xJ4TAP"))]
    (is (= [{:repo "futon3c" :sha "1ee5fea7"
             :subject "xiaoxiang-preview: bind keys in REPL buffers running on their own copy of the mode map (+12 -0 over 1 files)"}]
           (ta/happened-commits (:happened_summary t391))))
    (is (= [] (ta/happened-commits (:happened_summary t392))))
    (is (= [] (ta/happened-commits nil)))))

(deftest turn-acts-kinds-and-ids
  (let [{:keys [record reading reply commits]} (first (fixture-turns))
        acts (ta/turn->acts record reading reply commits)
        by-id (into {} (map (juxt :id identity) acts))]
    (testing "operator fragments keep the reading's intents, with stable ids"
      (is (= :redirect (:kind (by-id "claude-17-turn-391-f-s1-0"))))
      (is (= :report-problem (:kind (by-id "claude-17-turn-391-f-s2-0"))))
      (is (= "operator" (:author (by-id "claude-17-turn-391-f-s1-0")))))
    (testing "reply paragraphs take the kind their mark declares"
      (let [kinds (mapv :kind (filter #(str/includes? (:id %) "-r-") acts))]
        (is (= [:report-problem :approve :ask-action] kinds)
            "㊩ ㊣ and 🈸; the ㊥ gist has no kind and is skipped"))
      (is (= 1 (:skipped (meta acts))) "the gist paragraph alone"))
    (testing "a 🈸 without options is :ask-action, not :offer"
      (is (some #(= :ask-action (:kind %)) acts))
      (is (not-any? #(= :offer (:kind %)) acts)))
    (testing "the happened commit is a :commit act"
      (is (= :commit (:kind (by-id "claude-17-turn-391-c-1ee5fea7")))))
    (testing "every kind is from the kernel's table"
      (is (every? #(contains? lx/kinds (:kind %)) acts)))))

(deftest offer-options-are-detected
  (let [{:keys [record reading reply commits]} (second (fixture-turns))
        acts (ta/turn->acts record reading reply commits)
        offer (first (filter #(= :offer (:kind %)) acts))]
    (is (some? offer) "the 🈸 \"Reply 'yes 1' or 'yes 2'\" paragraph is an offer")
    (is (= [1 2] (:option offer)))
    (is (= "claude-17-turn-392" (:turn offer)))))

(deftest no-kind-intents-are-skipped-and-counted
  (let [record (:record (fixture-turn "turn-455grB"))
        reading {:sentences [{:id "s1"
                              :fragments [{:intent "frobnicate" :text "x"}
                                          {:intent "gist" :text "y"}
                                          {:intent "report" :text "z"}]}]}
        acts (ta/turn->acts record reading "" [])]
    (is (= 1 (count acts)) "only the report fragment has a kernel kind")
    (is (= :report (:kind (first acts))))
    (is (= 2 (:skipped (meta acts))) "the two no-kind intents are counted, not invented")))

(deftest session-acts-acceptance-and-carry-out
  (let [acts (ta/session-acts (fixture-turns))
        by-id (into {} (map (juxt :id identity) acts))
        offer (first (filter #(= :offer (:kind %)) acts))
        offer-id (:id offer)]
    (testing "turns are ordered by created_at"
      (is (= turn-ids (distinct (map :turn acts)))))
    (testing "Joe's approve in 393 became an :accept targeting the offer"
      (let [accept (first (filter #(= :accept (:kind %)) acts))]
        (is (= "claude-17-turn-393" (:turn accept)))
        (is (= offer-id (:target accept)))
        (is (= 1 (:option accept)))))
    (testing "the commit in 393 carries the accepted offer out"
      (is (= offer-id (:carries-out (by-id "claude-17-turn-393-c-d9d30ead")))))
    (testing "391's commit, before any acceptance, carries nothing out"
      (is (nil? (:carries-out (by-id "claude-17-turn-391-c-1ee5fea7")))))
    (testing "the kernel closes the offer in turn 393"
      (let [ports (ta/turn-ports acts "claude-17-turn-393")]
        (is (some #{offer-id} (:closed-this-turn ports)))))
    (testing "the unanswered 🈸 ask-actions are still open at the end"
      (let [ports (ta/turn-ports acts "claude-17-turn-394")
            open (set (map :act (:still-open ports)))]
        (is (contains? open "claude-17-turn-391-r-3")
            "turn 391's \"shall I read how P11's records get written\"")
        (is (some (fn [{:keys [act kind]}]
                    (and (= "claude-17-turn-394" (subs act 0 (count "claude-17-turn-394")))
                         (= :ask-action kind)))
                  (:still-open ports))
            "turn 394's \"shall I do 1 now and look into 2 afterwards\"")))
    (testing "still-open entries carry act, kind and since"
      (let [ports (ta/turn-ports acts "claude-17-turn-394")]
        (is (every? #(and (:act %) (:kind %) (:since %)) (:still-open ports)))))))
