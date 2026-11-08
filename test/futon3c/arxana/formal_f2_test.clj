(ns futon3c.arxana.formal-f2-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f2a2-" ".db") _ (.delete db)
        clock #(let [tick (atom 0)]
                 (fn [] (format "2026-11-08T12:00:%02d.000Z" (swap! tick inc))))]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})
              url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
              cfg #(hash-map :graph-store (store-http/http-store url f1/service-principal)
                             :artifact-provider (artifact/in-memory-provider)
                             :text/clock (clock))]
          (try (f node (cfg) (cfg)) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn dispatch [cfg operation body message-id]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id message-id
                         :operation operation :principal/id "formal-f2-test" :body body}))
(defn submit [cfg aid patch mid]
  (dispatch cfg :text/submit-patch {:artifact/id aid :patch patch} mid))
(defn get-record [cfg id]
  (get-in (dispatch cfg :entity/get {:entity/id id} (str "get:" id)) [:ok :record]))
(defn get-head [cfg aid]
  (get-in (dispatch cfg :revision/head {:artifact/id aid}
                    (str "head:" (random-uuid))) [:ok :head/revision-id]))
(defn get-text [cfg rid]
  (get-in (dispatch cfg :revision/get {:artifact/revision-id rid}
                    (str "revision:" rid)) [:ok :revision/text]))
(defn cp-count [^String s] (.codePointCount s 0 (.length s)))
(defn insertion [base author op-id at text]
  {:base base :author author :op-id op-id
   :hunks [{:start at :end at :replacement text}]})

(deftest two-instances-derive-sequencing-from-the-store
  (with-stack
    (fn [_ a b]
      (let [aid "formal-arxana:f1-conversation"
            exported (export/export (f1a/add-operator-judgement (f1a/plan-fixtures)))
            ingested (export/ingest! a exported)
            base (get-in ingested [:markdown :ok :artifact/revision-id])
            n (cp-count (:markdown exported))
            r1 (submit a aid (insertion base "emacs" "p1" n "\nEmacs α🈳\n") "p1")
            r2 (submit b aid (insertion base "code-server" "p2" 2 "VS象\n") "p2")
            h2 (get-head b aid)]
        (is (= :applied (get-in r1 [:ok :result/status])))
        (is (= :rebased (get-in r2 [:ok :result/status])))
        (is (= h2 (get-in r2 [:ok :head/revision-id])))
        (is (.contains ^String (get-text a h2) "VS象"))
        (is (.contains ^String (get-text b h2) "Emacs α🈳"))
        (doseq [id [(get-in r1 [:ok :patch/id]) (get-in r1 [:ok :application/id])
                    (get-in r2 [:ok :patch/id]) (get-in r2 [:ok :application/id])]]
          (is (get-record b id)))

        (testing "three revisions behind walks and composes stored applications"
          (let [r3 (submit a aid (insertion h2 "a" "p3" 20 "three") "p3")
                h3 (get-in r3 [:ok :head/revision-id])
                r4 (submit b aid (insertion h3 "b" "p4" 30 "four") "p4")
                stale (submit a aid (insertion base "offline" "old" 40 "offline") "old")]
            (is (= :applied (get-in r3 [:ok :result/status])))
            (is (= :applied (get-in r4 [:ok :result/status])))
            (is (= :rebased (get-in stale [:ok :result/status])))
            (is (.contains ^String (get-text b (get-head b aid)) "offline"))))

        (testing "conflict made by A is chosen by fresh instance B"
          (let [head (get-head a aid)
                left (submit a aid {:base head :author "a" :op-id "left"
                                    :hunks [{:start 0 :end 1 :replacement "A"}]} "left")
                right (submit b aid {:base head :author "b" :op-id "right"
                                     :hunks [{:start 0 :end 1 :replacement "B"}]} "right")
                alt-id (second (get-in right [:ok :alternative/ids]))
                alt (get-record b alt-id)
                choice (dispatch b :text/choose
                                 {:artifact/id aid :conflict/id (get-in right [:ok :conflict/id])
                                  :alternative/id alt-id :actor "joe"
                                  :chosen-at "2026-11-08T13:00:00.000Z"}
                                 "choose")
                chosen-head (get-head a aid)]
            (is (= :applied (get-in left [:ok :result/status])))
            (is (= :conflict (get-in right [:ok :result/status])))
            (is (= :chosen (get-in choice [:ok :result/status])))
            (is (= (get-in alt [:asserted-content :result/text]) (get-text a chosen-head)))
            (is (get-record a (get-in choice [:ok :application/id])))))

        (testing "a real interleaved third write wins CAS; losing patch stays"
          (let [head (get-head a aid)
                landed (submit a aid (insertion head "landed" "landed" 50 "L") "landed")
                once (atom false)
                hooked (assoc b :text/before-cas
                              (fn [_]
                                (when (compare-and-set! once false true)
                                  (submit a aid (insertion (get-head a aid) "third" "third" 60 "T")
                                          "third"))))
                raced (submit hooked aid (insertion head "loser" "loser" 70 "R") "loser")]
            (is (= :applied (get-in landed [:ok :result/status])))
            (is (= :arxana.text/rebase-raced (get-in raced [:error :error/code])))
            (is (get-record a (get-in raced [:error :error/detail :patch/id])))))

        (testing "a raw ingest creates an explicit application-chain gap"
          (let [head (get-head a aid)
                raw (dispatch a :revision/ingest
                              {:artifact/id aid :bytes (str (get-text a head) "\nraw")
                               :media-type "text/markdown" :expected/revision-id head}
                              "raw-gap")
                gap (submit b aid (insertion head "offline" "gap" 80 "G") "gap")]
            (is (:ok raw))
            (is (= :arxana.text/application-chain-gap (get-in gap [:error :error/code])))
            (is (get-record a (get-in gap [:error :error/detail :patch/id])))))))))

(deftest choose-refuses-after-head-moves
  (with-stack
    (fn [_ a b]
      (let [aid "formal-arxana:f1-conversation"
            exported (export/export (f1a/add-operator-judgement (f1a/plan-fixtures)))
            base (get-in (export/ingest! a exported) [:markdown :ok :artifact/revision-id])
            left (submit a aid {:base base :author "a" :op-id "l"
                                :hunks [{:start 0 :end 1 :replacement "L"}]} "l")
            conflict (submit b aid {:base base :author "b" :op-id "r"
                                    :hunks [{:start 0 :end 1 :replacement "R"}]} "r")
            moved (submit a aid (insertion (get-head a aid) "third" "move" 20 "M") "move")
            choice (dispatch b :text/choose
                             {:artifact/id aid :conflict/id (get-in conflict [:ok :conflict/id])
                              :alternative/id (first (get-in conflict [:ok :alternative/ids]))
                              :actor "joe" :chosen-at "2026-11-08T14:00:00.000Z"}
                             "stale-choice")]
        (is (= :applied (get-in left [:ok :result/status])))
        (is (= :conflict (get-in conflict [:ok :result/status])))
        (is (= :applied (get-in moved [:ok :result/status])))
        (is (= :arxana.text/conflict-head-moved (get-in choice [:error :error/code])))))))
