(ns futon3c.agency.artifact-activation-test
  (:require [clojure.test :refer [deftest is]]
            [futon.pattern-index :as pattern-index]
            [futon3c.agency.artifact-activation :as activation]
            [futon3c.social.shapes :as shapes])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def observed-at "2026-09-28T21:00:00Z")
(def descriptor
  {:name :futon3a/notions-search
   :model "sentence-transformers/all-MiniLM-L6-v2"
   :script-sha256 "script-a"
   :index-sha256 "index-a"})
(def artifact {:kind :invoke-job :id "invoke-work-1" :observed-at observed-at})
(def hits
  [{:id "agency/state-atomicity" :title "Atomic" :score 0.8}
   {:id "social/idempotent-handoff" :title "Handoff" :score 0.7}
   {:id "象/限定随论" :title "限定随论" :score 0.6}])

(deftest exact-citations-are-not-weak
  (let [known (mapv :id hits)]
    (is (= ["agency/state-atomicity"]
           (activation/cited-ids "Use ~agency/state-atomicity." known)))
    (is (= ["social/idempotent-handoff"]
           (activation/cited-ids "Use `social/idempotent-handoff`." known)))
    (is (= ["agency/state-atomicity"]
           (activation/cited-ids "[atomic](agency/state-atomicity)" known)))
    (is (= ["象/限定随论"]
           (activation/cited-ids "遵守 象/限定随论。" known)))
    (is (empty? (activation/cited-ids "state-atomicity" known)))
    (let [record (activation/activation-record
                  artifact "~agency/state-atomicity and 象/限定随论" descriptor hits)]
      (is (shapes/valid? shapes/EvidenceEntry record))
      (is (= ["agency/state-atomicity" "象/限定随论"]
             (get-in record [:evidence/body :cited])))
      (is (= ["social/idempotent-handoff"]
             (get-in record [:evidence/body :weak]))))))

(defn- fake-store [] (atom {}))

(defmacro with-fake-recording [store & body]
  `(binding [activation/*descriptor-fn* (constantly descriptor)
             activation/*search-fn* (fn [_# _#] hits)
             activation/*get-fn* (fn [_# id#] (get @~store id#))
             activation/*append-fn*
             (fn [_# entry#]
               (swap! ~store assoc (:derived/id entry#) entry#)
               {:ok true :entry entry#})]
     ~@body))

(defn- sha256-text [text]
  (-> (HexFormat/of)
      (.formatHex (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes text StandardCharsets/UTF_8)))))

(deftest records-text-digest-without-copying-large-text
  (let [marker "DISTINCTIVE-EVIDENCE-CLEANUP-MARKER"
        text (str (apply str (repeat 11000 "x")) marker)
        expected-sha (sha256-text text)
        store (fake-store)]
    (with-fake-recording store
      (let [result (activation/record! store artifact text)
            entry (:entry result)
            printed (pr-str entry)]
        (is (= :recorded (:status result)))
        (is (= expected-sha (get-in entry [:derived/body :text-sha256])))
        (is (not (contains? (:derived/body entry) :text)))
        (is (not (contains? (:derived/body entry) :text-truncated?)))
        (is (not (.contains printed marker)))
        (is (.contains printed expected-sha))
        ;; Legacy records retain their copied text, but replay comparison
        ;; ignores only the retired fields and does not append or conflict.
        (swap! store assoc (:derived/id entry)
               (update entry :derived/body assoc
                       :text text :text-truncated? false))
        (is (= :existing (:status (activation/record! store artifact text))))
        (is (= 1 (count @store)))))))

(deftest record-is-idempotent-and-input-sensitive
  (let [store (fake-store)]
    (with-fake-recording store
      (let [first-result (activation/record! store artifact "packet")
            second-result (activation/record! store artifact "packet")]
        (is (= :recorded (:status first-result)))
        (is (= :existing (:status second-result)))
        (is (= 1 (count @store)))
        (activation/record! store artifact "changed packet")
        (is (= 2 (count @store)))))
    (binding [activation/*descriptor-fn*
              (constantly (assoc descriptor :index-sha256 "index-b"))
              activation/*search-fn* (fn [_ _] hits)
              activation/*get-fn* (fn [_ id] (get @store id))
              activation/*append-fn* (fn [_ entry]
                                       (swap! store assoc (:derived/id entry) entry)
                                       {:ok true :entry entry})]
      (activation/record! store artifact "packet")
      (is (= 3 (count @store)) "a different index produces a different id"))))

(deftest search-failure-is-a-record
  (let [store (fake-store)]
    (binding [activation/*descriptor-fn* (constantly descriptor)
              activation/*search-fn*
              (fn [_ _] (throw (ex-info "offline" {:error/code :search-offline})))
              activation/*get-fn* (fn [_ id] (get @store id))
              activation/*append-fn* (fn [_ entry]
                                       (swap! store assoc (:derived/id entry) entry)
                                       {:ok true :entry entry})]
      (is (= :recorded (:status (activation/record! store artifact "packet"))))
      (let [entry (first (vals @store))]
        (is (= {:reason :search-offline :message "offline"}
               (get-in entry [:derived/body :error])))
        (is (empty? (get-in entry [:derived/body :hits])))))))

(deftest submit-does-not-wait-for-search
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        appended (promise)
        started (System/nanoTime)]
    (binding [activation/*descriptor-fn* (constantly descriptor)
              activation/*search-fn* (fn [_ _]
                                       (.countDown entered)
                                       (.await release 2 TimeUnit/SECONDS)
                                       hits)
              activation/*get-fn* (fn [_ _] nil)
              activation/*append-fn* (fn [_ entry]
                                       (deliver appended entry)
                                       {:ok true :entry entry})]
      (is (= :submitted (activation/submit-work! nil artifact "packet")))
      (is (< (/ (- (System/nanoTime) started) 1000000.0) 100.0))
      (is (.await entered 1 TimeUnit/SECONDS))
      (is (not (realized? appended)))
      (.countDown release)
      (is (some? (deref appended 1000 nil))))))

(deftest missing-record-detects-the-bad-case
  (let [success (activation/activation-record artifact "packet" descriptor hits)
        failure (activation/activation-record
                 (assoc artifact :id "invoke-work-2") "packet" descriptor
                 {:error {:reason :search-failed :message "failed"}})]
    (is (= [{:job-id "invoke-missing" :reason :activation-missing}]
           (activation/missing-activations
            ["invoke-work-1" "invoke-work-2" "invoke-missing"]
            [success failure])))))

(deftest empty-search-is-a-typed-failure-not-an-empty-success
  (doseq [result [nil []]]
    (let [appended (atom nil)]
      (binding [activation/*search-fn* (fn [_ _] result)
                activation/*descriptor-fn* (constantly descriptor)
                activation/*get-fn* (fn [_ _] nil)
                activation/*append-fn* (fn [_ entry] (reset! appended entry) {:ok true :entry entry})]
        (activation/record! nil artifact "packet"))
      (is (= :search-returned-nothing
             (get-in @appended [:derived/body :error :reason]))))))

(deftest activation-maps-to-migration-compatible-derived-row
  (let [entry (activation/activation-record artifact "packet" descriptor hits)
        row (activation/derived-record entry)]
    (is (= (str "derived:artifact-activation:" (:evidence/id entry))
           (:derived/id row)))
    (is (= :artifact-activation (:derived/family row)))
    (is (= "f75fb0fff" (:derived/writer-version row)))
    (is (= [(:evidence/id entry)] (:derived/source-act-ids row)))
    (is (= (:evidence/body entry) (:derived/body row)))))

(deftest real-descriptor-hashes-script-and-checkpoints
  (let [script (java.io.File/createTempFile "notions" ".py")
        checkpoint (atom {:embedding {:seq 1 :library_head "a"}
                          :sigil {:seq 1 :embedding_seq 1 :library_head "a"}})]
    (spit script "print(1)")
    (with-redefs [activation/script-file (constantly script)
                  pattern-index/checkpoint #(deref checkpoint)]
      (let [d1 (activation/retrieval-descriptor)
            d2 (activation/retrieval-descriptor)]
        (is (re-matches #"[0-9a-f]{64}" (:index-sha256 d1)))
        (is (not= (:script-sha256 d1) (:index-sha256 d1)))
        (is (= d1 d2))
        (swap! checkpoint assoc-in [:sigil :seq] 2)
        (is (not= (:index-sha256 d1)
                  (:index-sha256 (activation/retrieval-descriptor))))))))

(deftest markdown-sections-own-text-until-any-next-heading
  (let [text (str "preamble\n"
                  "# Root\nroot body\n"
                  "## 子節\nchild body\n"
                  "```md\n# not a heading\n```\n"
                  "### Tail\nlast")]
    (is (= [{:heading "Root" :start-line 2 :end-line 3
             :text "# Root\nroot body"}
            {:heading "子節" :start-line 4 :end-line 8
             :text "## 子節\nchild body\n```md\n# not a heading\n```"}
            {:heading "Tail" :start-line 9 :end-line 10
             :text "### Tail\nlast"}]
           (activation/markdown-sections text)))))

(deftest changed-sections-ignore-line-shifts
  (let [old "# A\nalpha\n# B\nbeta"
        shifted "preamble\n\n# A\nalpha\n# B\nbeta"
        edited "# A\nchanged\n# B\nbeta"]
    (is (empty? (activation/changed-sections old shifted)))
    (is (= ["A"] (mapv :heading (activation/changed-sections old edited))))))

(deftest cap-failure-record-keeps-count
  (let [entry (activation/activation-record
               {:kind :doc-section :id "sha:path:section-cap" :observed-at observed-at}
               "" descriptor
               {:error {:reason :section-cap-exceeded :message "capped" :count 21}})]
    (is (shapes/valid? shapes/EvidenceEntry entry))
    (is (= {:reason :section-cap-exceeded :message "capped" :count 21}
           (get-in entry [:evidence/body :error])))))
