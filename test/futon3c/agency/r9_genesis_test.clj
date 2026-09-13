(ns futon3c.agency.r9-genesis-test
  (:require [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon2.aif.r9-checker :as r9] [futon3c.agency.r9-genesis :as genesis]
            [futon3c.transport.http :as http])
  (:import (java.nio.file Files) (java.security MessageDigest)))

(defn- hex [bs] (apply str (map #(format "%02x" (bit-and 0xff %)) bs)))
(defn- sha256 [path]
  (with-open [in (io/input-stream path)]
    (let [d (MessageDigest/getInstance "SHA-256") b (byte-array 8192)]
      (loop [] (let [n (.read in b)] (when (pos? n) (.update d b 0 n) (recur))))
      (hex (.digest d)))))
(def artifact-paths
  {"r9-genesis-source" "src/futon3c/agency/r9_genesis.clj"
   "r9-genesis-tests" "test/futon3c/agency/r9_genesis_test.clj"
   "lead-review" "../futon2/holes/labs/wm-contract/runs/row-19-genesis-verifier-2026-09-13/lead-review.md"})
(defn- verified [schema origin body]
  (merge {:status :verified :authority/schema schema :authority-origin origin
          :provenance {:fixture "temporary/source-backed"}} body))
(defn- pins []
  {:source {:id "r9-genesis-source" :sha256 (sha256 (artifact-paths "r9-genesis-source"))}
   :tests {:id "r9-genesis-tests" :sha256 (sha256 (artifact-paths "r9-genesis-tests"))}
   :review {:id "lead-review" :sha256 (sha256 (artifact-paths "lead-review"))}})
(defn- candidate []
  {:schema :wm/r9-genesis-candidate-v1 :kind :genesis :author "codex-23" :reviewer "codex-26"
   :author-job-id "r9-author-job" :reviewer-job-id "r9-review-job"
   :author-trace-id "trace-r9-author-job" :reviewer-trace-id "trace-r9-review-job"
   :external-root {:id "fixture-root-ref"} :acceptance {:id "fixture-acceptance-ref"}
   :artifacts (pins)})
(defn- make-job! [id agent prompt trace]
  (#'http/create-invoke-job! {:requested-job-id id :agent-id agent :prompt prompt
                              :caller "codex-26" :surface "bell" :mode "work"})
  (#'http/update-invoke-jobs-ledger!
   #(-> % (assoc-in [:jobs id :trace-id] trace) (assoc-in [:jobs id :state] "done")
           (assoc-in [:jobs id :finished-at] "2026-09-13T02:00:00Z")))
  (http/invoke-job-request-commission id))
(defn- context [c commissions]
  (let [root (verified :wm/external-root-resolution-v1 :reviewed-host-boundary
                       {:authority-root-id "fixture-root" :delegate "codex-26"
                        :verification-scope :temporary-positive-boundary-fixture})
        subject {:external-root "fixture-root"
                 :jobs {:author (:author-job-id c) :reviewer (:reviewer-job-id c)}
                 :commissions {:author (:request-digest (commissions (:author-job-id c)))
                               :reviewer (:request-digest (commissions (:reviewer-job-id c)))}
                 :artifacts (:artifacts c)}]
    {:root root
     :traces {(:author-trace-id c) (verified :wm/trace-resolution-v1 :isolated-agency-ledger {:job-id (:author-job-id c)})
              (:reviewer-trace-id c) (verified :wm/trace-resolution-v1 :isolated-agency-ledger {:job-id (:reviewer-job-id c)})}
     :acceptance (verified :wm/delegated-acceptance-resolution-v1 :independent-review-fixture
                           {:schema :delegated-canonical-branch-acceptance-v1
                            :authority :delegated-technical-lead :accepted-by "codex-26"
                            :branch "main" :reviewer-job-id (:reviewer-job-id c)
                            :review-outcome :accepted :subject subject :digest "fixture-digest"})}))
(defn- options [c commissions authority]
  {:candidate c :root-resolver (constantly (:root authority))
   :commission-resolver #(verified :wm/invoke-commission-resolution-v1 :isolated-agency-api
                                   {:resolved-job-id % :envelope (commissions %)})
   :trace-resolver #(get-in authority [:traces %])
   :artifact-resolver #(let [path (artifact-paths (:id %))]
                         (verified :wm/artifact-byte-resolution-v1 :host-filesystem-bytes
                                   {:artifact-id (:id %) :sha256 (when path (sha256 path))}))
   :acceptance-resolver (constantly (:acceptance authority))
   :predecessor-resolver #(verified :wm/anchored-checker-resolution-v1 :prior-anchor-store
                                    {:checker-source-sha256 (:checker-source-sha256 %)})})
(defn- refusal [o] (:refusal (try (genesis/verify-candidate o)
                                  (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest genesis-boundary-positive-and-source-derived-refusals
  (let [tmp (Files/createTempDirectory "r9-genesis-" (make-array java.nio.file.attribute.FileAttribute 0))
        ledger (str (.resolve tmp "invoke.edn")) archive (str (.resolve tmp "commissions"))]
    (with-redefs-fn {#'http/invoke-jobs-store-path (constantly ledger)
                     #'http/invoke-commission-archive-dir (constantly archive)}
      (fn []
       (http/reset-invoke-jobs!)
       (try
        (let [c (candidate)
              ac (make-job! (:author-job-id c) (:author c) "Implement verifier" (:author-trace-id c))
              rc (make-job! (:reviewer-job-id c) (:reviewer c) "Review verifier" (:reviewer-trace-id c))
              commissions {(:author-job-id c) ac (:reviewer-job-id c) rc}
              authority (context c commissions) o (options c commissions authority)]
          (is (= :temporary-positive-boundary-fixture (:verification-scope (genesis/verify-candidate o))))
          (is (= :r9/author-equals-reviewer (refusal (options (assoc c :reviewer (:author c)) commissions authority))))
          (is (= :r9/role-identity-missing (refusal (options (assoc c :author "") commissions authority))))
          (is (= :r9/author-reviewer-job-equal (refusal (options (assoc c :reviewer-job-id (:author-job-id c)) commissions authority))))
          (is (= :r9/mandatory-artifacts-missing (refusal (options (assoc c :artifacts {}) commissions authority))))
          (testing "correctly rehashed forged prompt is rejected against API authority"
            (let [forged (assoc-in ac [:commission :prompt] "forged")
                  forged (assoc forged :request-digest (r9/request-digest (:commission forged)))]
              (is (= :r9/delegated-acceptance-unverified
                     (refusal (assoc o :commission-resolver
                                     #(verified :wm/invoke-commission-resolution-v1 :isolated-agency-api
                                                {:resolved-job-id % :envelope (if (= % (:author-job-id c)) forged (commissions %))})))))))
          (is (= :r9/trace-job-join-mismatch (refusal (assoc o :trace-resolver #(assoc (get-in authority [:traces %]) :job-id "wrong")))))
          (is (= :r9/commission-join-mismatch
                 (refusal (assoc o :commission-resolver
                                 #(let [e (commissions %)]
                                    (verified :wm/invoke-commission-resolution-v1 :isolated-agency-api
                                              {:resolved-job-id % :envelope (if (= % (:author-job-id c)) (assoc-in e [:job-join :agent-id] "other") e)}))))))
          (is (= :r9/artifact-pin-mismatch (refusal (assoc o :artifact-resolver #(assoc ((:artifact-resolver o) %) :sha256 (apply str (repeat 64 "0")))))))
          (is (= :r9/delegated-acceptance-unverified (refusal (options (assoc-in c [:artifacts :source :sha256] (apply str (repeat 64 "a"))) commissions authority))))
          (is (= :r9/external-root-unverified (refusal (assoc o :root-resolver (constantly {:status :verified :authority/schema :wm/external-root-resolution-v1})))))
          (is (= :r9/delegated-acceptance-unverified (refusal (assoc o :acceptance-resolver (fn [_] (dissoc (:acceptance authority) :provenance))))))
          (is (= :r9/predecessor-unverified (refusal (options (assoc c :kind :successor :predecessor nil) commissions authority))))
          (is (= :verified-for-independent-review
                 (:decision (genesis/verify-candidate (options (assoc c :kind :successor :predecessor {:checker-source-sha256 (apply str (repeat 64 "b"))}) commissions authority)))))
          (is (= :r9/external-root-unverified (refusal (assoc o :root-resolver genesis/unresolved-host-event-stub)))))
        (finally (http/reset-invoke-jobs!)))))))
