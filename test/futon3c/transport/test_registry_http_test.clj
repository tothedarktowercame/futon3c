(ns futon3c.transport.test-registry-http-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.validation :as validation]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- json-body [response]
  (json/parse-string (:body response) true))

(def check-meaning
  (str "validity-now, not the recorded mint verdict. "
       "Uncommitted drift usually means a lane is mid-edit; "
       "wait, do not re-dispatch."))

(defn- git! [root & args]
  (apply shell/sh (concat args [:dir root])))

(defn- write! [root path content]
  (let [file (io/file root path)]
    (io/make-parents file)
    (spit file content)
    file))

(deftest check-endpoint-is-the-existing-check-authority
  (let [backend (atom {:entries {} :order []})
        check! (fn [received-backend options]
                 (is (identical? backend received-backend))
                 (if (= "test-registry-real" (:entry-id options))
                   {:warrant? true :entry-id (:entry-id options)
                    :diff-paths (:changed-paths options)}
                   {:record/type :test-registry/refusal
                    :warrant? false :reason :record-not-found}))
        payload {:entry-id "test-registry-real"
                 :repo-root "/repo" :changed-paths ["src/a.clj"]}]
    (with-redefs [registry/check-record! check!]
      (let [direct (check! backend payload)
            response (http/handle-test-registry-check
                      {:body (json/generate-string payload)}
                      {:evidence-store backend})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= direct (:check body)))
        (is (= check-meaning (:meaning body)))
        (is (not (contains? body :warrant?))
            "top-level response cannot masquerade as the mint-time payload"))
      (let [response (http/handle-test-registry-check
                      {:body (json/generate-string
                              (assoc payload :entry-id "fabricated"))}
                      {:evidence-store backend})
            body (json-body response)]
        (is (false? (get-in body [:check :warrant?])))
        (is (= "record-not-found" (get-in body [:check :reason])))))))

(deftest stale-check-classifies-worktree-drift-without-changing-the-verdict
  (let [root (.toFile (Files/createTempDirectory
                       "test-registry-drift-"
                       (make-array FileAttribute 0)))
        root-path (.getAbsolutePath root)]
    (try
      (git! root-path "git" "init" "-q")
      (git! root-path "git" "config" "user.email" "test@example.com")
      (git! root-path "git" "config" "user.name" "test")
      (let [file (write! root-path "src/a.clj" "(ns a)\n")]
        (git! root-path "git" "add" "src/a.clj")
        (git! root-path "git" "commit" "-qm" "recorded")
        (let [recorded {"src/a.clj" (registry/file-sha file)}]
          (write! root-path "src/a.clj" "(ns a) ;; mid-edit\n")
          (let [observed {"src/a.clj" (registry/file-sha file)}
                dirty (registry/classify-scope-drift root-path recorded observed)]
            (is (= [{:path "src/a.clj" :classification :uncommitted}] dirty))
            (git! root-path "git" "add" "src/a.clj")
            (git! root-path "git" "commit" "-qm" "superseding change")
            (is (= [{:path "src/a.clj" :classification :committed}]
                   (registry/classify-scope-drift root-path recorded observed)))
            (let [current-check {:record/type :test-registry/refusal
                                 :warrant? false :reason :stale-sha
                                 :details {:scope-drift dirty}}]
              (with-redefs [registry/check-record! (fn [_ _] current-check)]
                (let [body (json-body
                            (http/handle-test-registry-check
                             {:body (json/generate-string
                                     {:entry-id "test-registry-stale"
                                      :repo-root root-path :changed-paths []})}
                             {:evidence-store (atom {})}))]
                  (is (false? (get-in body [:check :warrant?])))
                  (is (= "stale-sha" (get-in body [:check :reason])))
                  (is (= "uncommitted"
                         (get-in body [:check :details :scope-drift 0 :classification])))
                  (is (= check-meaning (:meaning body)))
                  (is (not (contains? body :warrant?)))))))))
      (finally
        (doseq [file (reverse (file-seq root))]
          (io/delete-file file true))))))

(deftest report-endpoint-row-count-equals-current-subject-bindings
  (let [root (.toFile (Files/createTempDirectory
                       "test-registry-http-"
                       (make-array FileAttribute 0)))
        index (io/file root "data/test-registry-validation/subjects.ednlog")
        backend (atom {:entries {} :order []})]
    (try
      (io/make-parents index)
      (spit index
            (str (pr-str {:entry/type :subject-binding :subject-id "subject-a"
                          :warrant-id "warrant-a" :actor "test"
                          :at "2026-09-19T00:00:00Z"}) "\n"
                 (pr-str {:entry/type :subject-binding :subject-id "subject-b"
                          :warrant-id "warrant-b" :actor "test"
                          :at "2026-09-19T00:00:01Z"}) "\n"))
      (with-redefs-fn
        {#'futon3c.test-registry.validation/check-binding
         (fn [_ binding] {:verdict :current :check {:warrant? true
                                                    :entry-id (:warrant-id binding)}})}
        (fn []
          (let [response (http/handle-test-registry-report
                          {} {:evidence-store backend
                              :test-registry-root (.getAbsolutePath root)})
                body (json-body response)
                binding-count (count (validation/subjects
                                      {:index-file (.getAbsolutePath index)}))]
            (is (= 200 (:status response)))
            (is (= binding-count (count (:rows body))))
            (is (= binding-count (get-in body [:summary :subjects])))
            (is (= binding-count (get-in body [:summary :current]))))))
      (finally
        (doseq [file (reverse (file-seq root))]
          (io/delete-file file true))))))

(deftest report-endpoint-reuses-authoritative-result-while-ledgers-are-unchanged
  (let [root (.toFile (Files/createTempDirectory
                       "test-registry-http-cache-"
                       (make-array FileAttribute 0)))
        calls (atom 0)
        report {:rows [{:subject-id "subject-a" :verdict :current}]
                :summary {:current 1}}
        json-report (json/parse-string (json/generate-string report) true)]
    (try
      (reset! @#'futon3c.transport.http/test-registry-report-cache nil)
      (with-redefs [validation/report! (fn [_] (swap! calls inc) report)]
        (let [config {:evidence-store (atom {})
                      :test-registry-root (.getAbsolutePath root)}]
          (is (= json-report (json-body (http/handle-test-registry-report {} config))))
          (is (= json-report (json-body (http/handle-test-registry-report {} config))))
          (is (= 1 @calls) "warm read does not recompute conformance")))
      (finally
        (reset! @#'futon3c.transport.http/test-registry-report-cache nil)
        (doseq [file (reverse (file-seq root))]
          (io/delete-file file true))))))

(deftest run-endpoint-registers-mechanically-never-a-callers-outcome
  (let [backend (atom {:entries {} :order []})
        calls (atom [])
        register! (fn [received-backend spec]
                    (swap! calls conj spec)
                    (is (identical? backend received-backend))
                    (is (not (some #(contains? spec %) [:warrant? :results :outcome])))
                    {:evidence/id "test-registry-registered"
                     :payload {:warrant? true :postcheck {:status :matched}}})
        spec {:repo-root "/repo" :command ["clojure" "-X:test" ":nses" "[a-test]"]
              :author "wm-author" :artifact-dir "/tmp/artifacts"
              :code-paths ["src"] :test-paths ["test"]}]
    (with-redefs [registry/register-run! register!]
      (let [response (http/handle-test-registry-run
                      {:body (json/generate-string spec)}
                      {:evidence-store backend})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= 1 (count @calls)))
        (is (= "test-registry-registered" (get body :evidence/id)))
        (is (true? (:warrant? body)))
        (is (= {:status "matched"} (:postcheck body))))
      ;; A caller-supplied outcome is refused before the registry is touched:
      ;; the registry runs the command; the body names what to run, never
      ;; what happened.
      (reset! calls [])
      (let [response (http/handle-test-registry-run
                      {:body (json/generate-string (assoc spec :warrant? true))}
                      {:evidence-store backend})
            body (json-body response)]
        (is (= 400 (:status response)))
        (is (= "caller-supplied-outcome-refused" (:reason body)))
        (is (empty? @calls)))
      (let [response (http/handle-test-registry-run
                      {:body (json/generate-string (dissoc spec :artifact-dir))}
                      {:evidence-store backend})
            body (json-body response)]
        (is (= 400 (:status response)))
        (is (= "run-spec-invalid" (:reason body)))))))

(deftest run-endpoint-surfaces-the-registrys-typed-refusal
  (let [backend (atom {:entries {} :order []})]
    (with-redefs [registry/register-run!
                  (fn [_ _]
                    (throw (ex-info "scope-not-committed"
                                    {:record/type :test-registry/refusal
                                     :warrant? false :reason :scope-not-committed
                                     :details {:paths ["src/a.clj"]}})))]
      (let [response (http/handle-test-registry-run
                      {:body (json/generate-string
                              {:repo-root "/repo" :command ["make" "test"]
                               :author "a" :artifact-dir "/tmp/x"})}
                      {:evidence-store backend})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= "test-registry/refusal" (:record/type body)))
        (is (false? (:warrant? body)))
        (is (= "scope-not-committed" (:reason body)))))))
