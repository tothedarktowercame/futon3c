(ns futon3c.arxana.formal-f2-markup-test
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

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f2-markup-" ".db") _ (.delete db)
        clock #(let [tick (atom 0)]
                 (fn [] (format "2026-11-08T17:00:%02d.000Z" (swap! tick inc))))]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})
              url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
              cfg #(hash-map :graph-store (store-http/http-store url f1/service-principal)
                             :artifact-provider (artifact/in-memory-provider)
                             :text/clock (clock))]
          (try (f (cfg) (cfg)) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn- dispatch [cfg op body mid]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id mid
                         :operation op :principal/id "formal-f2-markup-test" :body body}))

(defn- head [cfg]
  (get-in (dispatch cfg :revision/head {:artifact/id "formal-arxana:f1-conversation"}
                    (str "head:" (random-uuid))) [:ok :head/revision-id]))

(defn- text [cfg rid]
  (get-in (dispatch cfg :revision/get {:artifact/revision-id rid}
                    (str "revision:" rid)) [:ok :revision/text]))

(defn- submit [cfg base op-id hunk]
  (dispatch cfg :text/submit-patch
            {:artifact/id "formal-arxana:f1-conversation"
             :patch {:base base :author (if (.startsWith ^String op-id "emacs")
                                          "emacs" "vscode")
                     :op-id op-id :hunks [hunk]}}
            op-id))

(defn- record [cfg id]
  (get-in (dispatch cfg :entity/get {:entity/id id} (str "entity:" id)) [:ok :record]))

(defn- relocations [cfg subject]
  (let [q (dispatch cfg :graph/query {:query/type :attestations :subject/id subject
                                      :query/limit 200}
                    (str "attestations:" subject ":" (random-uuid)))]
    (->> (get-in q [:ok :result/attestation-ids])
         (map #(record cfg %))
         (filter #(= "arxana.resolve/anchor" (get-in % [:method :id]))))))

(defn- at-target [cfg subject rid]
  (some #(when (= rid (get-in % [:content :target/revision-id])) %)
        (relocations cfg subject)))

(defn- candidate-range [att]
  (let [c (first (get-in att [:content :candidates]))]
    [(get-in c [:location :range :start]) (get-in c [:location :range :end])]))

(deftest semantic-markup-follows-collaborative-successors
  (with-stack
    (fn [emacs vscode]
      (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
            _ (f1a/materialize! emacs plans)
            exported (export/export plans)
            _ (export/ingest! emacs exported)
            entries (vec (mapcat :interpretations (get-in exported [:manifest :turns])))
            [unique changed deleted copied] (map :clusion/id (take 4 entries))
            base (:markdown/revision-id exported)
            first-edit (submit emacs base "emacs-away" {:start 0 :end 0 :replacement "EMACS\n"})
            second-edit (submit vscode base "vscode-away"
                                {:start (count (.toArray (.codePoints ^String (:markdown exported))))
                                 :end (count (.toArray (.codePoints ^String (:markdown exported))))
                                 :replacement "\nVSCODE"})
            shifted-head (head vscode)]
        (is (= :applied (get-in first-edit [:ok :result/status])))
        (is (= :rebased (get-in second-edit [:ok :result/status])))
        (is (seq (get-in second-edit [:ok :relocation :writes]))
            (pr-str (get-in second-edit [:ok :relocation])))
        (is (every? :ok (get-in second-edit [:ok :relocation :writes]))
            (pr-str (get-in second-edit [:ok :relocation])))
        (testing "untouched scopes relocate exactly on the shared rebased head"
          (doseq [id (map :clusion/id entries)]
            (let [att (at-target vscode id shifted-head)]
              (is att)
              (is (= :resolved (get-in att [:content :resolution/status])))
              (is (= shifted-head (get-in att [:content :target/revision-id])))
              (is (get-in att [:content :application/id])))))

        (testing "changed content is explicit missing and cites application hunks"
          (let [before (head emacs) [start end] (candidate-range (at-target emacs changed before))
                changed-result (submit emacs before "emacs-inside"
                                       {:start start :end end :replacement "edited semantic scope"})
                rid (get-in changed-result [:ok :head/revision-id])
                att (at-target vscode changed rid)]
            (is (= :missing (get-in att [:content :resolution/status])))
            (is (= :none (:coverage att)))
            (is (seq (get-in att [:content :application/hunks])))))

        (testing "whole-scope deletion is missing while the interpretation is unchanged"
          (let [before (head vscode) original (record vscode deleted)
                [start end] (candidate-range (at-target vscode deleted before))
                result (submit vscode before "vscode-delete"
                               {:start start :end end :replacement ""})
                rid (get-in result [:ok :head/revision-id])
                att (at-target emacs deleted rid)]
            (is (= :missing (get-in att [:content :resolution/status])))
            (is (= original (record emacs deleted)))))

        (testing "copy ambiguity keeps all candidates"
          (let [before (head emacs) current (text emacs before)
                att0 (at-target emacs copied before)
                [start end] (candidate-range att0)
                fragment (export/codepoint-substring current start end)
                n (.codePointCount ^String current 0 (.length ^String current))
                result (submit vscode before "vscode-copy"
                               {:start n :end n :replacement (str "\n" fragment)})
                rid (get-in result [:ok :head/revision-id])
                att (at-target emacs copied rid)]
            (is (= :ambiguous (get-in att [:content :resolution/status])))
            (is (= 2 (count (get-in att [:content :candidates]))))))

        (testing "conflict choice relocates against the chosen head"
          (let [before (head emacs)
                left (submit emacs before "emacs-conflict" {:start 1 :end 2 :replacement "L"})
                conflict (submit vscode before "vscode-conflict" {:start 1 :end 2 :replacement "R"})
                choice (dispatch vscode :text/choose
                                 {:artifact/id "formal-arxana:f1-conversation"
                                  :conflict/id (get-in conflict [:ok :conflict/id])
                                  :alternative/id (second (get-in conflict [:ok :alternative/ids]))
                                  :actor "joe" :chosen-at "2026-11-08T18:00:00.000Z"}
                                 "choose-collaborative")
                chosen (get-in choice [:ok :head/revision-id])
                att (at-target emacs unique chosen)]
            (is (= :applied (get-in left [:ok :result/status])))
            (is (= :conflict (get-in conflict [:ok :result/status])))
            (is (= :chosen (get-in choice [:ok :result/status])))
            (is (= chosen (head emacs)))
            (is (= chosen (get-in att [:content :target/revision-id])))
            (is (= (get-in choice [:ok :application/id])
                   (get-in att [:content :application/id])))))))))
