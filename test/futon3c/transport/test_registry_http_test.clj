(ns futon3c.transport.test-registry-http-test
  (:require [futon3c.test-support.git-fixture :as git-fixture]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.local-port :as local-port]
            [futon3c.test-registry.sqlite-backend :as sqlite]
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
  (apply shell/sh (concat args [:dir root :env (git-fixture/environment)])))

(defn- write! [root path content]
  (let [file (io/file root path)]
    (io/make-parents file)
    (spit file content)
    file))

(defn- temp-db []
  (let [dir (.toFile (Files/createTempDirectory
                      "test-registry-http-db-"
                      (make-array FileAttribute 0)))]
    [dir (str (io/file dir "registry.sqlite"))]))

(defn- current-request [namespace repo]
  {:request-method :get :uri "/api/alpha/test-registry/current"
   :query-string (str "namespace=" namespace "&repo=" repo)})

(deftest current-endpoint-reads-and-requests-through-the-local-port
  (let [[dir path] (temp-db)
        root (io/file dir "repo")
        root-path (.getAbsolutePath root)]
    (try
      (.mkdirs root)
      (git! root-path "git" "init" "-q")
      (git! root-path "git" "config" "user.email" "test@example.com")
      (git! root-path "git" "config" "user.name" "test")
      (let [file (write! root-path "src/a.clj" "(ns a)\n")]
        (git! root-path "git" "add" "src/a.clj")
        (git! root-path "git" "commit" "-qm" "fixture")
        (let [head (str/trim (:out (git! root-path "git" "rev-parse" "HEAD")))
              backend (sqlite/sqlite-backend path)
              intent (registry/append-record! backend {:kind :intent :author "http-test" :run/id "current"} nil)
              run (registry/append-record!
                   backend {:kind :run :author "http-test" :run/id "current"
                            :namespace "example.current-test" :command ["test"]
                            :repo/root root-path :git-head head
                            :ran-at "2026-09-28T00:00:00Z" :finished-at "2026-09-28T00:00:01Z"
                            :warrant? true :load-closure []
                            :test-files {"src/a.clj" (registry/file-sha file)}}
                   (:evidence/id intent))]
          (with-redefs [local-port/*repo-roots* {"futon3c" root-path}]
            (let [response (http/extra-routes (current-request "example.current-test" "futon3c")
                                              {:registry-db path})
                  body (json-body response)]
              (is (= 200 (:status response)))
              (is (= "current" (get-in body [:current :status])))
              (is (= (:evidence/id run) (get-in body [:current :entry-id]))))
            (spit file "(ns a) ;; changed\n")
            (let [first-body (json-body (http/handle-test-registry-current
                                         (current-request "example.current-test" "futon3c")
                                         {:registry-db path}))
                  second-body (json-body (http/handle-test-registry-current
                                          (current-request "example.current-test" "futon3c")
                                          {:registry-db path}))]
              (is (= "missing" (get-in first-body [:current :status])))
              (is (= "stale" (get-in first-body [:current :data :reason])))
              (is (= (get-in first-body [:current :data :request-id])
                     (get-in second-body [:current :data :request-id]))))
            (let [body (json-body (http/handle-test-registry-current
                                   (current-request "example.absent-test" "futon3c")
                                   {:registry-db path}))]
              (is (= "absent" (get-in body [:current :data :reason])))
              (is (integer? (get-in body [:current :data :request-id]))))))
      (is (= 400 (:status (http/handle-test-registry-current
                           (current-request "example.test" "elsewhere")
                           {:registry-db path}))))
      (let [response (http/handle-test-registry-current
                      (current-request "example.test" "futon3c")
                      {:registry-db (.getAbsolutePath dir)})
            body (json-body response)]
        (is (= 500 (:status response)))
        (is (= "local-store-unavailable" (:reason body)))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(deftest latest-endpoint-reads-only-the-local-registry
  (let [[dir path] (temp-db)
        backend (sqlite/sqlite-backend path)
        general (atom {:entries {} :order []})]
    (try
      (let [intent (registry/append-record!
                    backend {:kind :intent :author "http-test" :run/id "local"} nil)
            run (registry/append-record!
                 backend {:kind :run :author "http-test" :run/id "local"
                          :namespace "example.local-test"
                          :command ["clojure" "-M:test" "-n" "example.local-test"]
                          :ran-at "2026-09-27T23:00:00Z"
                          :finished-at "2026-09-27T23:00:01Z"
                          :warrant? true}
                 (:evidence/id intent))
            response (http/handle-test-registry-latest
                      {:query-string "namespace=example.local-test"}
                      {:registry-db path :evidence-store general})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= (:evidence/id run) (get-in body [:latest :entry-id])))
        (is (= {:entries {} :order []} @general)
            "general evidence store receives no registry query"))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(deftest unavailable-local-store-is-a-typed-server-refusal
  (let [dir (.toFile (Files/createTempDirectory
                      "test-registry-http-unavailable-"
                      (make-array FileAttribute 0)))
        response (http/handle-test-registry-latest
                  {:query-string "namespace=example.local-test"}
                  {:registry-db (.getAbsolutePath dir)})
        body (json-body response)]
    (try
      (is (= 500 (:status response)))
      (is (= "local-store-unavailable" (:reason body)))
      (is (= (.getAbsolutePath dir) (get-in body [:details :path])))
      (finally (io/delete-file dir true)))))

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
                      {:test-registry-backend backend})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= direct (:check body)))
        (is (= check-meaning (:meaning body)))
        (is (not (contains? body :warrant?))
            "top-level response cannot masquerade as the mint-time payload"))
      (let [response (http/handle-test-registry-check
                      {:body (json/generate-string
                              (assoc payload :entry-id "fabricated"))}
                      {:test-registry-backend backend})
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
                             {:test-registry-backend (atom {})}))]
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
                          {} {:test-registry-backend backend
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
        (let [config {:test-registry-backend (atom {})
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
                      {:test-registry-backend backend})
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
                      {:test-registry-backend backend})
            body (json-body response)]
        (is (= 400 (:status response)))
        (is (= "caller-supplied-outcome-refused" (:reason body)))
        (is (empty? @calls)))
      (let [response (http/handle-test-registry-run
                      {:body (json/generate-string (dissoc spec :artifact-dir))}
                      {:test-registry-backend backend})
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
                      {:test-registry-backend backend})
            body (json-body response)]
        (is (= 200 (:status response)))
        (is (= "test-registry/refusal" (:record/type body)))
        (is (false? (:warrant? body)))
        (is (= "scope-not-committed" (:reason body)))))))
