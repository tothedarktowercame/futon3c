(ns futon3c.circus.routes-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.chain-certificate :as certificate]
            [futon3c.circus.chain-record :as chain]
            [futon3c.circus.routes :as routes]
            [futon3c.giraffe.run :as giraffe]))

(defn- temp-dir [tag]
  (.toFile (java.nio.file.Files/createTempDirectory
            (str "circus-routes-" tag)
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- valid-run! [root run-id]
  (let [dir (io/file root run-id)
        files ["input.edn" "rejected.edn" "pass.edn"
               "work-clusters.json" "report.md"]
        git-sha (str/trim
                 (:out (shell/sh "git" "rev-parse" "HEAD"
                                 :dir "/home/joe/code/futon3c")))]
    (.mkdirs dir)
    (doseq [file files] (spit (io/file dir file) (str file "\n")))
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id run-id :git-sha git-sha :sources {}
                   :generated-at "2026-11-17T02:00:00Z"
                   :files (mapv (fn [file] {:file file
                                            :sha256 (giraffe/sha256 (io/file dir file))})
                                files)}))
    dir))

(defn- top-result []
  {:status :ok :items [{:item "M-one" :priority-band :focus
                        :cutoff-status :within-cutoff
                        :focus-relations [{:turn {:id "turn-one"}}]}]})

(deftest latest-selects-newest-verified-run-and-normalizes-json
  (let [root (temp-dir "latest")
        valid (valid-run! root "20261117T020000.000Z-valid")
        invalid (valid-run! root "20261117T030000.000Z-invalid")
        _ (spit (io/file invalid "input.edn") "tampered")
        response (routes/top-items-response
                  {:root (str root) :qa-top-fn (fn [_] (top-result))}
                  {"run" "latest" "n" "10"})]
    (is (= 200 (:status response)))
    (is (= (.getName valid) (get-in response [:body "run"])))
    (is (= "2026-11-17T02:00:00Z" (get-in response [:body "generated_at"])))
    (is (= "focus" (get-in response [:body "items" 0 "priority-band"])))
    (is (= "within-cutoff" (get-in response [:body "items" 0 "cutoff-status"])))
    (is (= {"path" nil "check" nil}
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate"])))))

(deftest run-and-parameter-errors-are-typed
  (let [root (temp-dir "errors")]
    (is (= {"status" "error" "reason" "run-not-found" "run" "missing"}
           (:body (routes/top-items-response {:root (str root)} {"run" "missing"}))))
    (is (= 404 (:status (routes/top-items-response {:root (str root)}
                                                     {"run" "missing"}))))
    (is (= {:status 400 :body {"status" "error" "reason" "item-required"}}
           (routes/why-not-response {:root (str root)} {"run" "latest"})))))

(deftest qa-query-refusal-is-a-typed-response
  (let [root (temp-dir "refused")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        response (routes/top-items-response
                  {:root (str root)
                   :qa-top-fn (fn [_]
                                (throw (ex-info "truncated" {:reason :truncated})))}
                  {"run" run-id})]
    (is (= 503 (:status response)))
    (is (= "error" (get-in response [:body "status"])))
    (is (= "truncated" (get-in response [:body "failure"])))))

(deftest certificate-block-reads-real-sidecar-shape
  (let [root (temp-dir "cert-run")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        cert-root (temp-dir "certs")
        cert (io/file cert-root (str "turn-one-" run-id ".lean"))
        _ (spit cert "-- fixture")
        check {"exit" 0 "ok" true "at" "2026-11-17T02:01:00Z"
               "record_sha256" "sha"}
        _ (spit (str cert ".check.json") (json/generate-string check))
        response (routes/top-items-response
                  {:root (str root) :certificate-root (str cert-root)
                   :qa-top-fn (fn [_] (top-result))}
                  {"run" run-id})]
    (is (= (.getAbsolutePath cert)
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate" "path"])))
    (is (= check
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate" "check"])))))

(deftest why-not-response-is-normalized
  (let [root (temp-dir "why")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        response (routes/why-not-response
                  {:root (str root)
                   :qa-why-fn (fn [_] {:status :ok :stage :priority
                                       :reason :operator-gated})}
                  {"run" run-id "item" "M-one"})]
    (is (= "operator-gated" (get-in response [:body "reason"])))
    (is (= "priority" (get-in response [:body "stage"])))))

(deftest write-and-check-writes-check-sidecar
  (let [dir (temp-dir "sidecar")
        output (io/file dir "certificate.lean")
        record {:turn {:id "turn-one"} :run {:run-id "run-one"}
                :gcc {} :items [] :focus-link [] :priority {:items []}
                :record/sha256 "record-sha"}]
    (with-redefs [chain/chain-record (constantly record)
                  certificate/pinned-ids (constantly [])
                  certificate/lean-check (constantly {:exit 0})]
      (is (:ok? (certificate/write-and-check!
                 {:turn-id "turn-one" :run-dir (str dir)
                  :output-path (str output)}))))
    (let [sidecar (json/parse-string (slurp (str output ".check.json")))]
      (is (= 0 (get sidecar "exit")))
      (is (true? (get sidecar "ok")))
      (is (= "record-sha" (get sidecar "record_sha256")))
      (is (string? (get sidecar "at"))))))
