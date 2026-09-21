(ns futon3c.transport.warrant-handoff-test
  "Warrant rides the handoff — the HTTP seam. A malformed warrants vector
  is a typed 400 before any job exists; a valid one is accepted and the
  normalized warrants reach the delivered-turn header via
  wrap-surface-header. No live server, no real agent invocation."
  (:require [futon3c.social.mesh-test-fixtures :as mesh-fixtures]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.warrant :as warrant]
            [futon3c.social.coordination-ledger :as coordination-ledger]
            [futon3c.transport.http :as http]))

(def agent-id "claude-6-warrant-test")

(def valid-entry-id
  "test-registry-0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(defn- json-request [m]
  {:body (json/generate-string m)})

(defn- parse-body [resp]
  (let [b (:body resp)]
    (json/parse-string (if (string? b) b (slurp b)) true)))

(defn- register-test-agent!
  []
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type :claude
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:invoke]}))

(defn- cleanup-agent!
  []
  (try (reg/unregister-agent! agent-id) (catch Throwable _)))

(use-fixtures
  :each mesh-fixtures/with-store
  (fn [f]
    ;; Redirect the invoke-jobs ledger to a temp file (auto_bellback_test style)
    ;; so handler tests never touch real invoke state.
    (let [tmp (java.io.File/createTempFile "warrant-handoff" ".edn")]
      (.delete tmp)
      (with-redefs-fn {#'http/invoke-jobs-store-path (fn [] (.getAbsolutePath tmp))}
        (fn []
          (reg/reset-registry!)
          (http/reset-invoke-jobs!)
          (register-test-agent!)
          (try (f) (finally (cleanup-agent!))))))))

(defn- handle-bell! [payload]
  (#'http/handle-bell (json-request payload) {}))

(deftest malformed-warrant-is-typed-400
  (let [response (handle-bell! {"agent-id" agent-id
                                "prompt" "review please"
                                "job-id" "warrant-400-1"
                                "warrants" [{"entry-id" "test-registry-short"
                                             "namespace" "futon3c.agency.warrant-test"
                                             "lane" "routine"
                                             "base-sha" "14ace87"}]})
        body (parse-body response)]
    (is (= 400 (:status response)))
    (is (= "warrant-invalid" (:err body)))
    (is (= "entry-id" (:field body)))
    ;; No job may exist for a refused handoff.
    (is (nil? (get-in (#'http/ensure-invoke-jobs-ledger!) [:jobs "warrant-400-1"])))))

(deftest valid-warrant-is-accepted
  (with-redefs-fn {#'http/invoke-executor (fn [_ _ f] (f))
                   #'http/run-invoke-job! (fn [_] {:ok true})
                   #'http/arse-ask! (fn [& _] (throw (ex-info "must not write ArSE" {})))}
    (fn []
      (let [response (handle-bell! {"agent-id" agent-id
                                    "prompt" "review please"
                                    "job-id" "warrant-202-1"
                                    "warrants" [{"entry-id" valid-entry-id
                                                 "namespace" "futon3c.agency.warrant-test"
                                                 "lane" "routine"
                                                 "base-sha" "14ace87"}]})
            body (parse-body response)]
        (is (= 202 (:status response)))
        (is (contains? body :job-id))))))

(deftest absent-warrants-key-leaves-handoff-untouched
  (let [captured (atom nil)]
    (with-redefs-fn {#'http/invoke-executor (fn [_ _ f] (f))
                     #'http/run-invoke-job! (fn [_] {:ok true})
                     #'futon3c.social.coordination-ledger/record-invoke-edge!
                     (fn [edge] (reset! captured edge) {:ok true})}
      (fn []
        (let [response (handle-bell! {"agent-id" agent-id
                                      "prompt" "an ordinary question"
                                      "job-id" "warrant-absent-1"})]
          (is (= 202 (:status response)))
          ;; Absent key: NO warrant status on the edge (finding 3).
          (is (map? @captured))
          (is (not (contains? @captured :warrant-status)))
          (is (not (contains? @captured :warrant-entry-ids))))))))

(deftest explicit-empty-warrants-is-typed-unwarranted
  (let [captured (atom nil)]
    (with-redefs-fn {#'http/invoke-executor (fn [_ _ f] (f))
                     #'http/run-invoke-job! (fn [_] {:ok true})
                     #'futon3c.social.coordination-ledger/record-invoke-edge!
                     (fn [edge] (reset! captured edge) {:ok true})}
      (fn []
        (let [response (handle-bell! {"agent-id" agent-id
                                      "prompt" "review please"
                                      "job-id" "warrant-empty-1"
                                      "warrants" []})]
          (is (= 202 (:status response)))
          ;; Explicit [] IS a warrant statement: typed :unwarranted (finding 3).
          (is (= :unwarranted (:warrant-status @captured)))
          (is (= [] (:warrant-entry-ids @captured))))))))

(deftest whistle-rejects-malformed-warrant
  (let [response (#'http/handle-whistle
                  (json-request {"agent-id" agent-id
                                 "prompt" "review please"
                                 "warrants" [{"entry-id" "test-registry-nope"
                                              "namespace" "futon3c.agency.warrant-test"
                                              "lane" "routine"
                                              "base-sha" "14ace87"}]})
                  {})
        body (parse-body response)]
    (is (= 400 (:status response)))
    (is (= "warrant-invalid" (:err body)))
    (is (= "entry-id" (:field body)))))

(deftest turn-header-renders-warrant-lines
  (testing "warranted handoff renders entry ids into the delivered turn"
    (let [normalized (warrant/normalize-warrants
                      [{:entry-id valid-entry-id
                        :namespace "futon3c.agency.warrant-test"
                        :lane :routine
                        :base-sha "14ace87"}])
          header (#'http/wrap-surface-header
                  "the prompt" "bell" "codex-1" agent-id {:warrants normalized})
          caller-idx (str/index-of header "Caller: codex-1")
          warrant-idx (str/index-of header "Warrants: :warranted (1)")]
      (is (some? caller-idx))
      (is (some? warrant-idx))
      (is (< caller-idx warrant-idx) "warrant lines follow the caller line")
      (is (str/includes? header valid-entry-id))
      (is (str/includes? header "lane=routine"))))
  (testing "unwarranted handoff renders the full-rerun consequence"
    (let [header (#'http/wrap-surface-header
                  "p" "bell" "codex-1" agent-id
                  {:warrants (warrant/normalize-warrants nil)})]
      (is (str/includes? header "Warrants: :unwarranted"))
      (is (str/includes? header "full rerun"))))
  (testing "no warrants key renders nothing (legacy handoffs unchanged)"
    (let [header (#'http/wrap-surface-header "p" "bell" "codex-1" agent-id nil)]
      (is (not (str/includes? header "Warrants:"))))))
