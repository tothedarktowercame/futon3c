(ns futon3c.arxana.formal-f2-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.formal-f2 :as f2]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f2a-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})
              cfg {:graph-store (store-http/http-store
                                 (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                                 f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)
                   :text-state (f2/state)}]
          (try (f node cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn dispatch [cfg operation body message-id]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id message-id
                         :operation operation :principal/id "formal-f2-test"
                         :body body}))

(defn submit [cfg artifact-id patch message-id]
  (dispatch cfg :text/submit-patch {:artifact/id artifact-id :patch patch} message-id))

(defn record [cfg id]
  (get-in (dispatch cfg :entity/get {:entity/id id} (str "get:" id)) [:ok :record]))

(defn cp-count [^String s] (.codePointCount s 0 (.length s)))

(deftest private-stack-sequences-and-preserves-conflicts
  (with-stack
    (fn [_ cfg]
      (let [artifact-id "formal-arxana:f1-conversation"
            exported (export/export (f1a/add-operator-judgement (f1a/plan-fixtures)))
            ingested (export/ingest! cfg exported)
            base (get-in ingested [:markdown :ok :artifact/revision-id])
            n (cp-count (:markdown exported))
            p1 {:base base :author "emacs" :op-id "p1"
                :hunks [{:start n :end n :replacement "\nEmacs α🈳\n"}]}
            p2 {:base base :author "code-server" :op-id "p2"
                :hunks [{:start 2 :end 2 :replacement "VS象\n"}]}
            r1 (submit cfg artifact-id p1 "submit:p1")
            r2 (submit cfg artifact-id p2 "submit:p2")]
        (is (= :applied (get-in r1 [:ok :result/status])))
        (is (= :rebased (get-in r2 [:ok :result/status])))
        (is (record cfg (get-in r1 [:ok :patch/id])))
        (is (record cfg (get-in r2 [:ok :patch/id])))
        (let [head (get-in r2 [:ok :head/revision-id])
              text (get-in (dispatch cfg :revision/get {:artifact/revision-id head} "get:merged")
                           [:ok :revision/text])]
          (is (.contains ^String text "VS象"))
          (is (.contains ^String text "Emacs α🈳")))

        (testing "same-position conflict keeps both different alternatives"
          (let [head (get-in r2 [:ok :head/revision-id])
                a {:base head :author "emacs" :op-id "same-a"
                   :hunks [{:start 0 :end 1 :replacement "A"}]}
                b {:base head :author "code-server" :op-id "same-b"
                   :hunks [{:start 0 :end 1 :replacement "B"}]}
                ra (submit cfg artifact-id a "submit:same-a")
                rb (submit cfg artifact-id b "submit:same-b")
                ids (get-in rb [:ok :alternative/ids])
                alternatives (mapv #(record cfg %) ids)]
            (is (= :applied (get-in ra [:ok :result/status])))
            (is (= :conflict (get-in rb [:ok :result/status])))
            (is (= 2 (count ids)))
            (is (= 2 (count (set (map #(get-in % [:asserted-content :result/text])
                                      alternatives)))))
            (is (= (get-in ra [:ok :head/revision-id])
                   (get-in rb [:ok :head/revision-id])) "conflict does not move head")
            (doseq [id (conj ids (get-in rb [:ok :conflict/id]))]
              (is (record cfg id)))
            (let [choice (dispatch cfg :text/choose
                                   {:artifact/id artifact-id
                                    :conflict/id (get-in rb [:ok :conflict/id])
                                    :alternative/id (second ids)
                                    :actor "joe"
                                    :chosen-at "2026-11-08T12:00:00.000Z"
                                    :reason "explicit choice"}
                                   "choose:same-b")]
              (is (= :chosen (get-in choice [:ok :result/status])))
              (is (record cfg (get-in choice [:ok :choice/id]))))))

        (testing "nested and partial overlaps never select a winner"
          (doseq [[label h1 h2 expected]
                  [["nested" {:start 5 :end 10 :replacement "outer"}
                    {:start 6 :end 8 :replacement "inner"} :nested]
                   ["partial" {:start 12 :end 18 :replacement "left"}
                    {:start 15 :end 21 :replacement "right"} :partial-overlap]]]
            (let [current (get-in @(:text-state cfg) [:heads artifact-id])
                  ra (submit cfg artifact-id {:base current :author "a" :op-id (str label "-a")
                                              :hunks [h1]} (str label ":a"))
                  rb (submit cfg artifact-id {:base current :author "b" :op-id (str label "-b")
                                              :hunks [h2]} (str label ":b"))
                  conflict (record cfg (get-in rb [:ok :conflict/id]))]
              (is (= :applied (get-in ra [:ok :result/status])))
              (is (= :conflict (get-in rb [:ok :result/status])))
              (is (= expected (get-in conflict [:asserted-content :overlap/class]))))))

        (testing "identical edits converge without a conflict record"
          (let [current (get-in @(:text-state cfg) [:heads artifact-id])
                h {:start 25 :end 25 :replacement "same"}
                ra (submit cfg artifact-id {:base current :author "a" :op-id "ident-a"
                                            :hunks [h]} "ident:a")
                rb (submit cfg artifact-id {:base current :author "b" :op-id "ident-b"
                                            :hunks [h]} "ident:b")]
            (is (= :applied (get-in ra [:ok :result/status])))
            (is (= :rebased (get-in rb [:ok :result/status])))
            (is (nil? (get-in rb [:ok :conflict/id])))))

        (testing "a third writer racing the one-shot rebase is typed, not retried"
          (let [current (get-in @(:text-state cfg) [:heads artifact-id])
                ra (submit cfg artifact-id
                           {:base current :author "a" :op-id "race-a"
                            :hunks [{:start 30 :end 30 :replacement "landed"}]}
                           "race:a")
                stale (fn [& _]
                        {:error {:error/category :stale
                                 :error/code :test/third-writer-won}})
                rb (with-redefs-fn {#'futon3c.arxana.service/ingest-revision stale}
                     #(submit cfg artifact-id
                              {:base current :author "b" :op-id "race-b"
                               :hunks [{:start 40 :end 40 :replacement "rebased"}]}
                              "race:b"))]
            (is (= :applied (get-in ra [:ok :result/status])))
            (is (= :arxana.text/rebase-raced (get-in rb [:error :error/code])))
            (is (record cfg (get-in rb [:error :error/detail :patch/id]))
                "the losing patch remains durable")))

        (testing "choice needs named actor and fixed-width event time"
          (let [bad (dispatch cfg :text/choose
                              {:artifact/id artifact-id :conflict/id "c" :alternative/id "a"
                               :actor "" :chosen-at "2026-11-08T12:00:00Z"}
                              "choice:bad")]
            (is (= :arxana.text/choice-attribution (get-in bad [:error :error/code])))))))))
