(ns futon3c.diagramprover.wm-wire-r0-wc-attempts-test
  "Wire [:r0-enact-step :wc-checker :attempts]: the enactment step's
  attempts reaching the W_c checker.

  The writer is flight-runner/enact-fn (:r0-enact-step's site), writing the
  attempts vector on the enactment record. The reader is the W_c checker,
  holes/labs/M-futon-seams/exemplar/proof2a_check.clj's check-c (the
  :wc-checker box; wc-verdict-fn runs it as
  bb proof2a_check.clj <click record> <enactment record> --wc --edn), whose
  every clause reads (:attempts enact): attempts at patterns outside the
  candidate, chosen patterns with no successful attempt, successful
  attempts with no check, a claimed token the pattern does not produce.

  No live record carries both ends (live-records-read), so the wire is
  WITNESSED-HERMETICALLY: enact-fn writes a real enactment record for
  click-001's :cand/a-registry-first (all seven patterns successfully
  attempted at the role grain), the checker runs on that record with
  --wc --edn, and its verdict corroborates the read: every decidable W_c
  clause passes (:failures []) and only the join is unverifiable, because
  click-001 predates the selection law's :candidate id. The writer's value
  is (:attempts (:enactment out)); the reader's is (:attempts record) of
  the record file the checker was given, and the bad cases show the
  checker's verdict moving with what it reads there."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's attempts, the reader's (from the record file the checker
  is given), and the checker's verdict. TAMPER edits the record file
  before it is read and checked (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-wc)]
     (d/rewrite record-path tamper)
     (let [{:keys [exit verdict] :as r} (d/run-wc record-path)]
       {:writer (:attempts enactment)
        :reader (:attempts (w/read-record record-path))
        :exit exit :verdict verdict :err (:err r)}))))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: enact-fn never wrote an :attempts, and no flight ever ran the checker on a real record"}
     {:path (:path d/click-001)
      :sha256 (:sha256 d/click-001)
      :why "the click record the checker reads here; hand-authored (claude-1), carries no selection-law :candidate, so the join is the typed :join-unverifiable"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): schema :m-futon-seams/proof2a-enactment-v1, not enact-fn's :wm/enactment-v1; its :attempts were not written by the writer"}]))

(def wire
  {:wire [:r0-enact-step :wc-checker :attempts]
   :kind :witnessed-hermetically
   :test `the-attempts-reach-the-wc-checker
   :check check
   :live-records-read live-records-read})

(deftest the-attempts-reach-the-wc-checker
  (let [o (check)]
    (is (= 7 (count (:writer o))))
    (is (every? :success (:writer o)))
    (is (= 0 (:exit o)) (str (:err o)))
    (is (= {:status :join-unverifiable :failures []}
           (select-keys (:verdict o) [:status :failures]))
        "the checker read the attempts and every decidable clause passed; only the join id is absent on this click")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :attempts {:absent :no-attempts}))]
    (is (= {:absent :no-attempts} (:reader o)))
    (is (= 0 (:exit o)))
    (is (vector? (:verdict o)) "the checker still read the field and answered")
    (is (not (w/received? o)))))

(deftest different-attempts-at-the-reader-fail-the-wire
  ;; one attempt claims a token its pattern does not produce: the checker's
  ;; verdict names the clause, evidence it read the different value
  (let [o (observe (fn [r] (update r :attempts
                                   (fn [as] (assoc-in (vec as) [1 :produced] :t/not-produced)))))]
    (is (= :t/not-produced (:produced (second (:reader o)))))
    (is (some #(re-find #"token its pattern does not produce" %) (:verdict o))
        (pr-str (:verdict o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-machine-enactment
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 _clk ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= :m-futon-seams/proof2a-enactment-v1 (:schema ex)))))
