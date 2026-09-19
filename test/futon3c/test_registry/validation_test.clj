(ns futon3c.test-registry.validation-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.evidence.store :as store]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.validation :as validation]
            [futon3c.test-registry.validation-adapters :as adapters])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "validation-service-" (make-array FileAttribute 0))))

(defn- cleanup! [dir]
  (doseq [f (reverse (file-seq dir))] (io/delete-file f)))

(defn- options [dir backend]
  {:backend backend
   :index-file (str (io/file dir "subjects.ednlog"))
   :queue-file (str (io/file dir "queue.ednlog"))})

(defn- entries [file]
  (if (.isFile (io/file file))
    (mapv edn/read-string (remove str/blank? (str/split-lines (slurp file))))
    []))

(defn- caught [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- warrant! [backend finished-at]
  ;; Real digest/parent envelopes; execution and fingerprints are registry
  ;; tests' concern. These tests never launch a nested test JVM or edit source.
  (let [intent (registry/append-record!
                backend {:kind :intent :author "validation-test"
                         :run/id finished-at} nil)]
    (registry/append-record!
     backend {:kind :run :author "validation-test" :run/id finished-at
              :repo/root "/isolated/specimen" :finished-at finished-at :warrant? true
              :code-files {"source.clj" "before"} :test-files {}}
     (:evidence/id intent))))

(defn- incident [at]
  {:subject-id "component/a"
   :incident {:kind :error :source "isolated-observation" :at at
              :detail "observed failure"}})

(deftest append-only-binding-and-typed-absence
  (let [dir (temp-dir) opts (options dir (atom {:entries {} :order []}))]
    (try
      (is (nil? (validation/subject-binding opts "never-checked")))
      (validation/bind-subject! opts "component/a" "warrant-1" "operator" "2026-09-19T00:00:00Z")
      (validation/bind-subject! opts "component/a" "warrant-2" "operator" "2026-09-19T00:01:00Z")
      (is (= "warrant-2" (:warrant-id (validation/subject-binding opts "component/a"))))
      (is (= 2 (count (entries (:index-file opts)))))
      (finally (cleanup! dir)))))

(deftest cli-register-and-report-share-the-configured-backend
  (let [dir (temp-dir) backend (atom {:entries {} :order []})
        opts (options dir backend)
        config (io/file dir "spec.edn") urls (atom []) calls (atom [])
        spec (assoc (dissoc opts :backend) :agency-url "http://isolated-evidence:7070"
                    :subject-id "component/a" :author "validation-test"
                    :repo-root "/isolated/specimen"
                    :command ["clojure" "-M:test" "-n" "isolated-test"])]
    (try
      (spit config (pr-str spec))
      (with-redefs [http-backend/make-http-backend
                    (fn [url] (swap! urls conj url) backend)
                    registry/register-run!
                    (fn [actual options]
                      (swap! calls conj [actual options])
                      (warrant! actual "2026-09-19T00:00:00Z"))
                    registry/check-record! (fn [actual _]
                                             (is (identical? backend actual))
                                             {:warrant? true})
                    clojure.core/shutdown-agents (fn [])]
        (let [mint (with-out-str (validation/-main "register" (str config)))]
          (is (str/includes? mint "warrant? true"))
          (is (str/includes? mint "bound component/a -> test-registry-")))
        (validation/bind-subject! opts "fabricated" "test-registry-does-not-exist"
                                  "test" "2026-09-19T00:00:00Z")
        (let [report (with-out-str (validation/-main "report" (str config)))]
          (is (str/includes? report "component/a current test-registry-"))
          (is (str/includes? report "fabricated no-warrant test-registry-does-not-exist"))
          (is (str/includes? report "SUMMARY {:current 1, :no-warrant 1}")))
        (is (= ["http://isolated-evidence:7070" "http://isolated-evidence:7070"] @urls))
        (is (identical? backend (ffirst @calls)))
        (is (not (contains? (second (first @calls)) :subject-id))))
      (finally (cleanup! dir)))))

(deftest process-default-atom-is-not-a-validation-backend
  (let [dir (temp-dir) remote (atom {:entries {} :order []})
        explicit (atom {:entries {} :order []}) urls (atom [])]
    (try
      (with-redefs [http-backend/make-http-backend (fn [url] (swap! urls conj url) remote)
                    registry/register-run! (fn [backend _]
                                             (is (identical? remote backend))
                                             (warrant! backend "2026-09-19T00:00:00Z"))]
        (is (identical? explicit (:backend (validation/resolve-options {:backend explicit}))))
        (is (identical? remote (:backend (validation/resolve-options {:agency-url "http://explicit"}))))
        (is (:warrant? (validation/register-and-bind!
                        (options dir store/!store)
                        {:agency-url "http://spec" :subject-id "component/a" :author "test"})))
        (is (= ["http://explicit" "http://spec"] @urls)))
      (finally (cleanup! dir)))))

(deftest refused-bind-and-enqueue-are-durable-without-changing-readers
  (let [dir (temp-dir) opts (options dir (atom {:entries {} :order []}))]
    (try
      (validation/bind-subject! opts "component/a" "existing" "operator" "2026-09-19T00:00:00Z")
      (let [before (validation/subjects opts)
            failure (caught #(validation/bind-subject! opts "component/a" "" "operator" "bad-time"))
            record (last (entries (:index-file opts)))]
        (is (= :binding-invalid (:reason (ex-data failure))))
        (is (= :refusal (:entry/type record)))
        (is (= :binding-invalid (:reason record)))
        (is (= (ex-data failure) {:record/type :test-registry.validation/refusal
                                 :reason (:reason record) :details (:details record)}))
        (is (= "" (get-in record [:attempted-args :warrant-id])))
        (is (string? (:at record)))
        (is (= before (validation/subjects opts))))
      (let [failure (caught #(validation/enqueue-revalidation! opts {:subject-id "component/a"}))
            record (last (entries (:queue-file opts)))]
        (is (= :incident-invalid (:reason (ex-data failure))))
        (is (= :refusal (:entry/type record)))
        (is (= :incident-invalid (:reason record)))
        (is (= [] (validation/revalidation-queue opts))))
      (testing "adapter deduplication does not treat refusals as queued incidents"
        (let [options (assoc opts :cursor-file (str (io/file dir "cursor.ednlog")))
              item (incident "2026-09-19T00:01:00Z")]
          (is (= {:enqueued 1 :skipped 0} (adapters/sweep-incidents! options :test [item])))
          (is (= {:enqueued 0 :skipped 1} (adapters/sweep-incidents! options :test [item])))
          (is (= 1 (count (validation/revalidation-queue opts))))))
      (finally (cleanup! dir)))))

(deftest refused-close-retains-the-original-incident-until-fresh-bound-warrant
  (let [dir (temp-dir) backend (atom {:entries {} :order []}) opts (options dir backend)]
    (try
      (let [old-id (:evidence/id (warrant! backend "2026-09-19T00:00:00Z"))
            fresh-id (:evidence/id (warrant! backend "2026-09-19T00:02:00Z"))
            opened (validation/enqueue-revalidation! opts (incident "2026-09-19T00:01:00Z"))
            close #(validation/close-revalidation! opts (:entry/id opened) % "reviewer" "2026-09-19T00:03:00Z")]
        (validation/bind-subject! opts "component/a" old-id "operator" "2026-09-19T00:00:00Z")
        (doseq [[id reason] [[old-id :warrant-not-fresh] [fresh-id :fresh-warrant-not-bound]]]
          (let [failure (caught #(close id)) record (last (entries (:queue-file opts)))]
            (is (= reason (:reason (ex-data failure))))
            (is (= :refusal (:entry/type record)))
            (is (= reason (:reason record)))
            (is (= id (get-in record [:attempted-args :warrant-id])))
            (is (= [opened] (validation/revalidation-queue opts)))))
        (is (= :revalidation-open (:verdict (first (validation/conformance opts)))))
        (validation/bind-subject! opts "component/a" fresh-id "reviewer" "2026-09-19T00:02:01Z")
        (close fresh-id)
        (is (= [] (validation/revalidation-queue opts)))
        (is (= [:revalidation-opened :refusal :refusal :revalidation-closed]
               (mapv :entry/type (entries (:queue-file opts)))))
        (with-redefs [registry/check-record! (fn [_ _] {:warrant? true})]
          (is (= :current (:verdict (first (validation/conformance opts)))))))
      (finally (cleanup! dir)))))

(deftest stale-warrant-remains-stale-and-unknown-queue-types-still-refuse
  (let [dir (temp-dir) backend (atom {:entries {} :order []}) opts (options dir backend)]
    (try
      (validation/bind-subject! opts "component/a"
                                (:evidence/id (warrant! backend "2026-09-19T00:00:00Z"))
                                "test" "2026-09-19T00:00:00Z")
      (with-redefs [registry/check-record!
                    (fn [_ _] {:warrant? false :reason :stale-sha
                               :details {:current {:code-files {"source.clj" "after"}
                                                   :test-files {}}}})]
        (let [row (first (validation/conformance opts))]
          (is (= :stale (:verdict row)))
          (is (= ["source.clj"] (:closure-diff row)))))
      (spit (:queue-file opts) "{:entry/type :unknown}\n")
      (is (= :queue-ledger-invalid (:reason (ex-data (caught #(validation/revalidation-queue opts))))))
      (finally (cleanup! dir)))))

(deftest registration-refusal-and-append-failure-preserve-exception-identity
  (let [dir (temp-dir) opts (options dir (atom {:entries {} :order []}))
        original (ex-info "original" {:reason :scope-not-committed :details {:path "source.clj"}})]
    (try
      (with-redefs [registry/register-run! (fn [& _] (throw original))]
        (is (identical? original (caught #(validation/register-and-bind! opts {:subject-id "component/a"}))))
        (let [record (last (entries (:index-file opts)))]
          (is (= :refusal (:entry/type record)))
          (is (= :register-and-bind! (:operation record)))
          (is (= :scope-not-committed (:reason record))))
        (let [err (java.io.StringWriter.)]
          (binding [*err* err]
            (is (identical? original
                            (caught #(validation/register-and-bind!
                                      (assoc opts :index-file (str dir)) {:subject-id "component/a"})))))
          (is (str/includes? (str err) "refusal trace append failed"))
          (is (str/includes? (str err) "scope-not-committed"))))
      (finally (cleanup! dir)))))
