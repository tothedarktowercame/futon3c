(ns futon3c.inbox-zero.board-consumer-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [futon3.inbox-zero.gates :as gates]
            [futon3c.inbox-zero.board-consumer :as consumer])
  (:import [java.time Instant]
           [java.util Date]))

(def now (Instant/parse "2026-09-14T20:00:00Z"))
(def old (Date/from (.minusSeconds now (* 48 3600))))
(def observation {:record/type :inbox-zero/file-observation
                  :observation/id "obs" :repo/root "/unused/demo" :repo/id "demo"
                  :worktree/id "wt" :path "README.md" :git/status :modified
                  :head/sha "head" :observed-at old :source :multi-watcher})
(def claim {:record/type :inbox-zero/session-file-claim :claim/id "claim"
            :repo/id "demo" :worktree/id "wt" :seat/id "seat:a:s"
            :path "README.md" :state :active :last-observed-at old})
(def state {:schema/version 0 :records {"obs" observation "claim" claim}})

(defn options [records executed]
  {:state-path "/unused/state.edn" :now-fn (constantly now)
   :load-fn (constantly state)
   :record! #(swap! records conj %)
   :refresh-fn (fn [s _ _] {:state s :observations []})
   :gate-specs [{:gate/name :check :cmd ["fake-check"]}]
   :execute-fn (fn [plan opts]
                 (swap! executed conj [plan opts])
                 {:verdict :committed :commit/sha "sha"})})

(defn consume [run opts]
  (with-redefs [gates/run-gates (fn [& _] {:passed? true :results []})]
    (consumer/consume! run opts)))

(deftest atomicity-blocker-retains-certificate
  (let [records (atom []) executed (atom []) run (consumer/proposal-run state now)
        result (consume run (options records executed))]
    (is (= [:inbox-zero/commit-intent :inbox-zero/refusal] (mapv :record/type result)))
    (is (empty? @executed))
    (is (= :atomic-feel-commit-unavailable (:refusal/reason (last result))))
    (is (every? #(= (:certificate run) (:certificate %)) @records))
    (doseq [digest [(get-in run [:certificate :board/digest])
                    (get-in run [:certificate :inputs/digest]) (:verbs/digest run)]]
      (is (str/includes? (consumer/commit-message run {}) digest)))))

(deftest certificate-tampering-refuses-before-io
  (doseq [tamper [#(assoc-in % [:certificate :board/digest] "forged")
                  #(assoc % :verbs/digest "forged")
                  #(assoc-in % [:inputs :in-flight-repos] ["demo"])
                  #(assoc-in % [:trace 4 :effects] [[:commit {:repo "other"}]])]]
    (let [records (atom []) executed (atom [])
          opts (assoc (options records executed) :load-fn (fn [_] (throw (Exception. "must not load"))))]
      (consume (tamper (consumer/proposal-run state now)) opts)
      (is (= [:invalid-certificate] (mapv :refusal/reason @records)))
      (is (empty? @executed)))))

(deftest new-edit-refuses-even-when-proposal-was-idle
  (doseq [late? [false true]]
    (let [records (atom []) executed (atom []) n (atom 0)
          fresh (assoc-in state [:records "recent"]
                          (assoc observation :observation/id "recent" :observed-at (Date/from now)))
          opts (assoc (options records executed)
                      :refresh-fn (fn [s _ _]
                                    {:state (if (or (not late?) (= 2 (swap! n inc))) fresh s)
                                     :observations []}))]
      (consume (consumer/proposal-run state now) opts)
      (is (= [:in-flight] (mapv :refusal/reason @records)))
      (is (empty? @executed)))))

(deftest missing-and-stale-claims-are-not-invented
  (let [clean (assoc observation :observation/id "clean" :git/status :clean
                                :observed-at (Date/from (.minusSeconds now (* 40 3600)) ))
        dirty (assoc observation :observation/id "dirty"
                                :observed-at (Date/from (.minusSeconds now (* 30 3600))))]
    (doseq [s [(update state :records dissoc "claim")
               (update state :records assoc "clean" clean "dirty" dirty)]]
      (let [records (atom []) executed (atom [])]
        (consume (consumer/proposal-run s now)
                 (assoc (options records executed) :load-fn (constantly s)))
        (is (= [:unattributed] (mapv :refusal/reason @records)))
        (is (empty? @executed))))))

(deftest safety-holds-are-durable
  (doseq [[reason transform]
          [[:validation-required #(assoc % :gate-specs [])]
           [:atomic-feel-commit-unavailable #(assoc % :execute-fn (fn [& _] {:verdict :held :held/reason :index-not-empty}))]
           [:execution-error #(assoc % :refresh-fn (fn [& _] (throw (Exception. "git unavailable"))))]]]
    (let [records (atom []) executed (atom [])]
      (consume (consumer/proposal-run state now) (transform (options records executed)))
      (is (= reason (:refusal/reason (last @records))))
      (is (= :inbox-zero/refusal (:record/type (last @records)))))))

(deftest board-busy-arm-is-recorded
  (let [records (atom []) executed (atom [])
        s (assoc-in state [:records "recent"]
                    (assoc observation :observation/id "recent" :observed-at (Date/from now)))]
    (consume (consumer/proposal-run s now) (options records executed))
    (is (= [:in-flight] (mapv :refusal/reason @records)))
    (is (empty? @executed))))

(deftest journal-failure-prevents-commit
  (let [executed (atom [])]
    (is (thrown-with-msg? Exception #"disk full"
                         (consume (consumer/proposal-run state now)
                                  (assoc (options (atom []) executed)
                                         :record! (fn [_] (throw (Exception. "disk full")))))))
    (is (empty? @executed))))

(deftest claim-changes-during-validation-refuse
  (let [n (atom 0) records (atom []) executed (atom [])
        opts (assoc (options records executed)
                    :refresh-fn (fn [s _ _]
                                  {:state (if (= 2 (swap! n inc))
                                            (assoc-in s [:records "claim" :state] :released) s)}))]
    (consume (consumer/proposal-run state now) opts)
    (is (= [:stale-attribution] (mapv :refusal/reason @records)))
    (is (empty? @executed))))

(deftest failed-gates-do-not-execute
  (let [records (atom []) executed (atom [])]
    (with-redefs [gates/run-gates (fn [& _] {:passed? false :results [{:exit 1}]})]
      (consumer/consume! (consumer/proposal-run state now) (options records executed)))
    (is (= [:gate-failed] (mapv :refusal/reason @records)))
    (is (empty? @executed))))

(deftest edit-after-intent-is-rechecked-before-git
  (let [records (atom []) executed (atom []) edited? (atom false)
        recent (assoc observation :observation/id "new-edit" :observed-at (Date/from now))
        opts (assoc (options records executed)
                    :record! (fn [r] (swap! records conj r) (reset! edited? true))
                    :refresh-fn (fn [s _ _]
                                  {:state (if @edited? (assoc-in s [:records "new-edit"] recent) s)}))]
    (consume (consumer/proposal-run state now) opts)
    (is (= [:inbox-zero/commit-intent :inbox-zero/refusal] (mapv :record/type @records)))
    (is (= :in-flight (:refusal/reason (last @records))))
    (is (empty? @executed))))

(deftest caller-options-cannot-certify-atomicity
  (doseq [opts [{} {:atomic? true} {:guard-verified? true}
                {:execute-fn (fn [& _] (throw (Exception. "must not execute")))}]]
    (let [result (consumer/atomic-commit! {:include [{:path "README.md"}]} opts)]
      (is (= :held (:verdict result)))
      (is (= :atomic-feel-commit-unavailable (:held/reason result)))
      (is (nil? (:commit/sha result))))))

(deftest edit-after-final-snapshot-cannot-reach-executor
  ;; Negative control for the original check/use gap: return an idle snapshot,
  ;; then mark the repo busy before control reaches the commit boundary.
  (let [records (atom []) executed (atom []) scans (atom 0) busy? (atom false)
        opts (assoc (options records executed)
                    :refresh-fn (fn [s _ _]
                                  (when (= 3 (swap! scans inc)) (reset! busy? true))
                                  {:state s :observations []}))]
    (consume (consumer/proposal-run state now) opts)
    (is @busy?)
    (is (= :atomic-feel-commit-unavailable (:refusal/reason (last @records))))
    (is (empty? @executed))))
