(ns futon3c.logic.xiang-test
  "The 象 kernel against three dialogue fixtures, two existing readers, and
   two laws. The fixtures are the acceptance tests: a frontend is certified
   when a history replayed through it yields these answers."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [futon3c.agency.rule-timeline :as rule-timeline]
            [futon3c.logic.xiang :as x]))

(defn- fixture [name]
  (let [rel (str "futon3c/logic/xiang_fixtures/" name ".edn")
        f (first (filter #(.exists ^java.io.File %)
                         [(io/file "test" rel) (io/file (or (System/getenv "FUTON3C_ROOT") ".") "test" rel)]))]
    (x/load-fixture f)))

(defn- t+s [k] (if (vector? k) k [k nil]))

(defn- check-expectations
  "Every :expect entry of a fixture against the kernel."
  [{:keys [history expect name]}]
  (let [db (x/db history)]
    (doseq [[when ids] (:in-force expect)]
      (let [[t s] (t+s when)]
        (is (= ids (x/in-force db t s)) (str name " in force as of " when))))
    (doseq [[when ids] (:promulgated expect)]
      (let [[t s] (t+s when)]
        (is (= ids (x/promulgated db t s)) (str name " promulgated as of " when))))
    (doseq [[when rows] (:obligations expect)]
      (let [[t s] (t+s when)]
        (is (= rows (x/obligations db t s)) (str name " obligations as of " when))))
    (doseq [[accept status] (:agreement expect)]
      (is (= status (x/agreement-status db accept)) (str name " agreement " accept)))
    (doseq [[[caller kind when] ok] (:authority expect)]
      (is (= ok (x/authority db caller kind when)) (str name " authority " caller kind when)))
    (doseq [[r acts] (:derivation expect)]
      (is (= acts (x/derivation db r)) (str name " derivation of " r)))
    (when-let [intents (:intents expect)]
      (is (set/subset? intents (set (keys (x/by-intent history)))) (str name " exercises its intents")))))

(deftest red-tape-as-the-mission-reconstructed-it
  (check-expectations (fixture "red-tape")))

(deftest an-offer-an-acceptance-a-promise-and-an-inferred-withdrawal
  (check-expectations (fixture "offer-promise")))

(deftest a-delegation-chain-with-a-retraction-and-a-deferral
  (check-expectations (fixture "handoff")))

(deftest the-two-time-axes-are-distinct
  (let [{:keys [history]} (fixture "offer-promise")
        db (x/db history)]
    (testing "an acceptance typed at 09:07 but stored at 09:30"
      (is (= {:status :agreed :offer "o1"} (x/agreement-status db "y1")))
      (is (not (contains? (x/in-force db "2026-10-02T09:10:00Z") "y1")) "as of 09:10 the store did not have it")
      (is (contains? (x/in-force db "2026-10-02T09:10:00Z" "2026-10-02T09:30:00Z") "y1") "valid 09:10, system 09:30: it counts")
      (is (contains? (x/in-force db "2026-10-02T09:30:00Z") "y1")))))

(deftest an-acceptance-with-no-visible-offer-is-refused
  (let [{:keys [history]} (fixture "offer-promise")
        ;; the offer's evidence never reached the store (the 964fa778 chain gap)
        gap (remove #(= "o1" (:id %)) history)
        db (x/db gap)]
    (is (= {:status :refused :reason :no-visible-offer} (x/agreement-status db "y1")))
    (is (not-any? #(= "y1" (:source %)) (x/obligations db "2026-10-02T12:00:00Z")))
    (testing "and an offer withdrawn before the acceptance is not visible either"
      (let [db (x/db (conj (vec history) {:id "ow" :kind :retract :author "codex-10" :target "o1"
                                          :at "2026-10-02T09:06:00Z" :agent "codex-10" :session "s10"}))]
        (is (= {:status :refused :reason :no-visible-offer} (x/agreement-status db "y1")))))))

(deftest unknown-kinds-are-refused-not-ignored
  (is (thrown? clojure.lang.ExceptionInfo
               (x/db [{:id "z" :kind :frobnicate :author "joe" :at "2026-10-03T00:00:00Z"}]))))

;; ---------------------------------------------------------------------------
;; Differential: the plain-Clojure readers against the kernel

(defn- rule-timeline-records
  "The red-tape history as the sourced rule timeline P13b reads: version 1
   applied when commit c1 landed, version 2 (followup half withdrawn) when c2
   did. The adapter is the statement of how this reader's records relate to
   acts; the test is that both answer the same."
  [history]
  (let [by-id (into {} (map (juxt :id identity) history))
        src (fn [ref] {:kind :load :ref ref})
        ev (fn [id] {:at (:at (by-id id)) :source {:ref id}})]
    [{:hx/props {:rule/timeline
                 {:family "kimi-requisition" :version 1 :kind :code :effect :requisition-with-followups
                  :adopted [(assoc (ev "r2") :basis :operator-act-reconstruction :grant-status :unrecorded)
                            (assoc (ev "r3") :basis :operator-act-reconstruction :grant-status :unrecorded)]
                  :committed [(assoc (ev "c1") :repo "futon3c" :sha (apply str (repeat 40 "a")))]
                  :live {:status :applied :at (:at (by-id "c1")) :source (src "c1")}}}}
     {:hx/props {:rule/timeline
                 {:family "kimi-requisition" :version 2 :kind :code :effect :followup-half-withdrawn
                  :adopted [(assoc (ev "w1") :basis :operator-act-reconstruction :grant-status :unrecorded)]
                  :committed [(assoc (ev "c2") :repo "futon3c" :sha (apply str (repeat 40 "b")))]
                  :live {:status :applied :at (:at (by-id "c2")) :source (src "c2")}}}}]))

(deftest rule-timeline-as-of-agrees-with-the-kernel
  (let [{:keys [history]} (fixture "red-tape")
        db (x/db history)
        records (rule-timeline-records history)
        reading (fn [t]
                  ;; the kernel's answer in the reader's three words
                  (let [force (x/in-force db t)]
                    ;; The reader has no word for "adopted, not yet committed":
                    ;; its "committed, not yet live" needs a commit, and in the
                    ;; kernel a commit is what applies a proposal. So a
                    ;; promulgated proposal reads as "no committed rule observed".
                    (cond (and (contains? force "r3") (not (contains? force "r2"))) "followup half withdrawn"
                          (contains? force "r2") "requisition rule applied"
                          :else "no committed rule observed")))]
    (doseq [t ["2026-09-24T16:00:00Z" "2026-09-24T16:30:00Z" "2026-09-24T17:00:00Z" "2026-09-25T21:00:00Z"]]
      (is (= (:answer (rule-timeline/as-of records "kimi-requisition" t)) (reading t)) t))
    (testing "where the two disagree on purpose: before the commit the reader sees no committed rule, the kernel sees a promulgated proposal"
      (is (= "no committed rule observed" (:answer (rule-timeline/as-of records "kimi-requisition" "2026-09-24T16:30:00Z"))))
      (is (contains? (x/promulgated db "2026-09-24T16:30:00Z") "r2")))))

(def ^:private obligations-as-of
  "agency/obligations pulls promise-history and with it the futon1b backend,
   which a standalone classpath does not have; resolve it at run time and
   skip the differential check where it cannot load (it runs on the box)."
  (try (requiring-resolve 'futon3c.agency.obligations/obligations-as-of)
       (catch Throwable _ nil)))

(deftest obligations-as-of-agrees-with-the-kernel-on-agreements
  (if-not obligations-as-of
    (println "  (skipped: futon3c.agency.obligations needs the futon1b classpath)")
  (let [{:keys [history]} (fixture "offer-promise")
        db (x/db history)
        offers [{:id "o1"} {:id "o1b"}]
        agreements [{:id "y1" :agreement/offer "o1" :agreement/offeror "codex-10"
                     :agreement/scope {:option 1} :agreement/at "2026-10-02T09:07:00Z" :act/stamp :declared}]
        inputs {:promise-history [] :promise-outcomes [] :agreements agreements :offers offers :reader-incomplete []}
        owed-by (fn [agent t] (set (map :obligation/id (:owes (obligations-as-of inputs agent (java.time.Instant/parse t))))))
        kernel (fn [agent t] (set (keep #(when (= agent (:debtor %)) (:source %)) (x/obligations db t t))))]
    (doseq [t ["2026-10-02T09:00:00Z" "2026-10-02T09:30:00Z" "2026-10-02T18:00:00Z"]]
      (is (= (owed-by "codex-10" t) (kernel "codex-10" t)) t))
    (testing "the reader has one time axis: at valid time 09:10 it already counts the agreement the store learned of at 09:30"
      (is (= #{"y1"} (owed-by "codex-10" "2026-10-02T09:10:00Z")))
      (is (= #{} (kernel "codex-10" "2026-10-02T09:10:00Z"))
          "the kernel with system time = valid time does not; the reader must be given system-as-of explicitly (P6)")))))

;; ---------------------------------------------------------------------------
;; Laws

(def ^:private t0 1759400000) ; 2026-10-02T10:13Z

(def gen-history
  "Random histories: operator rules and proposals, agent promises, commits
   that carry out proposals, withdrawals and reversals of earlier acts."
  (gen/let [n (gen/choose 2 9)]
    (gen/fmap
     (fn [picks]
       (vec (map-indexed
             (fn [i [kind who dt tgt]]
               (let [id (str "a" i)
                     earlier (when (pos? i) (str "a" (mod tgt i)))]
                 (cond-> {:id id :kind kind :author who :at (+ t0 (* i 600) dt)}
                   (= kind :promise) (assoc :beneficiary "joe")
                   (and earlier (#{:withdraw :reversal :fulfil} kind)) (assoc :target earlier)
                   (and earlier (= kind :commit)) (assoc :carries-out earlier)
                   (and (nil? earlier) (#{:withdraw :reversal :fulfil :commit} kind)) (assoc :kind :report))))
             picks)))
     (gen/vector (gen/tuple (gen/elements [:constrain :propose :promise :withdraw :reversal :fulfil :commit :report :approve])
                            (gen/elements ["joe" "claude-17" "codex-16"])
                            (gen/choose 0 599)
                            gen/nat)
                 n))))

(deftest law-appending-unrelated-acts-never-changes-the-past
  (let [result (tc/quick-check
                60
                (prop/for-all [history gen-history]
                  (let [db (x/db history)
                        last-at (apply max (map :at history))
                        t (- last-at 1)
                        later (conj history {:id "late" :kind :constrain :author "joe" :at (+ last-at 3600)})
                        db2 (x/db later)]
                    (and (= (x/in-force db t) (x/in-force db2 t))
                         (= (x/obligations db t) (x/obligations db2 t))))))]
    (is (:pass? result) (pr-str result))))

(deftest law-withdraw-then-reversal-restores-exactly
  (let [result (tc/quick-check
                60
                (prop/for-all [history gen-history]
                  (let [standing (filter #(contains? x/creating-kinds (:kind %)) history)]
                    (or (empty? standing)
                        (let [x0 (:id (last standing))
                              end (+ (apply max (map :at history)) 100)
                              with-w (conj history {:id "w" :kind :withdraw :author "joe" :target x0 :at (+ end 10)})
                              with-u (conj with-w {:id "u" :kind :reversal :author "joe" :target "w" :at (+ end 20)})
                              before (x/in-force (x/db history) (+ end 30))
                              withdrawn (x/in-force (x/db with-w) (+ end 30))
                              restored (x/in-force (x/db with-u) (+ end 30))]
                          (and (= before restored)
                               (= withdrawn (disj before x0))))))))]
    (is (:pass? result) (pr-str result))))
