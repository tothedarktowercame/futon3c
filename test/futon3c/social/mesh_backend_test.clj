(ns futon3c.social.mesh-backend-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.social.coordination-ledger :as ledger]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- failure-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- with-dev-store [store f]
  ;; Exercise the real resolver against the production-shaped configuration
  ;; var, without requiring dev (which would load unrelated serving systems).
  (let [existing (find-ns 'futon3c.dev)
        dev-ns (or existing (create-ns 'futon3c.dev))
        existing-var (ns-resolve dev-ns '!evidence-store)
        store-var (or existing-var (intern dev-ns '!evidence-store nil))]
    (try
      (with-redefs-fn {store-var (atom store)} f)
      (finally
        (when-not existing-var (ns-unmap dev-ns '!evidence-store))
        (when-not existing (remove-ns 'futon3c.dev))))))

(def request
  {:requested-job-id "mesh-backend-job" :agent-id "mesh-worker"
   :caller "mesh-caller" :surface "bell" :prompt "backend test"})

(deftest production-refuses-volatile-and-missing-backends
  (binding [ledger/*test-evidence-store* nil]
    (with-dev-store nil
      (fn []
        (doseq [[store kind] [[nil :missing]
                              [(atom {:entries {} :order []}) :raw-atom]
                              [(backend/->AtomBackend (atom {:entries {} :order []}))
                               :atom-backend]]]
          (testing (str kind)
            (doseq [operation [#(ledger/record-invoke-edge!
                                 {:from "a" :to "b" :evidence-store store})
                               #(ledger/recent-mesh-edges 10 store)]]
              (is (= {:error/code :mesh/non-durable-evidence-store :store-kind kind}
                     (failure-data operation))))))))))

(deftest production-resolves-existing-boot-authority
  (let [store (f1b/make-futon1b-backend "http://mesh-config.invalid")]
    (binding [ledger/*test-evidence-store* nil]
      (with-dev-store store
        (fn []
          (is (identical? store (ledger/mesh-evidence-store)))
          (is (identical? store (ledger/mesh-evidence-store store))))))))

(deftest job-creation-without-store-refuses-before-mutation
  ;; Exact historical bad case: HTTP creation supplies no store, the default
  ;; evidence atom exists, but dev has no configured durable backend.
  (let [jobs (atom (#'http/default-invoke-jobs-ledger))
        evidence (atom {:entries {} :order []})
        before @jobs]
    (binding [ledger/*test-evidence-store* nil]
      (with-redefs-fn
        {#'estore/!store evidence
         #'http/!invoke-jobs-ledger jobs}
        (fn []
          (is (= :mesh/non-durable-evidence-store
                 (:error/code (failure-data #(with-dev-store nil
                                              (fn [] (#'http/create-invoke-job! request)))))))
          (is (= before @jobs))
          (is (empty? (:entries @evidence))))))))

(deftest explicitly-bound-unit-store-is-shared-by-mesh-writer-and-reader
  (let [store (atom {:entries {} :order []})]
    (binding [ledger/*test-evidence-store* store]
      (let [receipt (ledger/record-invoke-edge! {:from "a" :to "b"})
            [edge] (ledger/recent-mesh-edges)]
        (is (:ok receipt))
        (is (= (:evidence/id receipt) (:evidence-id edge)))
        (is (= "a" (:from edge)))
        (is (= "b" (:to edge)))))))

(defn- with-isolated-job-ledger [root f]
  (with-redefs-fn
    {#'http/invoke-jobs-store-path (constantly (str (io/file root "jobs.edn")))
     #'http/!invoke-jobs-ledger (atom (#'http/default-invoke-jobs-ledger))
     #'http/!active-invoke-job-index (atom nil)
     #'http/!invoke-ingress-controller-config (atom {:status :inactive})
     #'http/parked-invoke-job-ids (constantly #{})
     #'bb/project-agents! (constantly nil)}
    f))

(deftest ^:slow job-edge-roundtrips-through-real-futon1b
  ;; No shared endpoints: real HTTP backend/server and XTDB store in this test
  ;; process, ephemeral loopback port and temporary disk directory. Resolve the
  ;; server only in the slow test so ordinary unit tests do not boot XTDB.
  (let [root (.toFile (Files/createTempDirectory "mesh-backend-" (make-array FileAttribute 0)))
        start! (requiring-resolve 'futon1b-server/start-server!)
        stop! (requiring-resolve 'futon1b-server/stop-server!)
        server (start! {:store-dir (str (io/file root "substrate"))
                        :bind-host "127.0.0.1" :port 0})
        node @(var-get (requiring-resolve 'futon1b-server/!node))
        base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
        store (f1b/make-futon1b-backend base)
        unused-default (atom {:entries {} :order []})]
    (try
      (binding [ledger/*test-evidence-store* nil]
        (with-isolated-job-ledger
          root
          (fn []
            (with-dev-store store
              #(with-redefs-fn {#'estore/!store unused-default}
              (fn []
                ;; The job creation request intentionally omits evidence-store.
                (is (= "mesh-backend-job" (#'http/create-invoke-job! request)))
                (is (.isFile (io/file root "jobs.edn")))
                (is (empty? (:entries @unused-default)))
                ;; Reconstruct the client; neither reader uses the atom that
                ;; swallowed these edges before this change.
                (let [reconstructed (f1b/make-futon1b-backend base)
                      handler (http/make-handler {:evidence-store reconstructed})
                      mesh (handler {:request-method :get
                                     :uri "/api/alpha/coordination/edges"
                                     :query-string "limit=10"})
                      [edge] (:edges (json/parse-string (:body mesh) true))
                      evidence (handler {:request-method :get
                                         :uri (str "/api/alpha/evidence/" (:evidence-id edge))})
                      entry (:entry (json/parse-string (:body evidence) true))]
                  (is (not (identical? store reconstructed)))
                  (is (= 200 (:status mesh)))
                  (is (= 200 (:status evidence)))
                  (is (= "mesh-backend-job" (:edge-id edge)))
                  (is (= "mesh-caller" (:from edge)))
                  (is (= "mesh-worker" (:to edge)))
                  (is (= (:evidence-id edge) (:evidence/id entry)))
                  (is (= "mesh-backend-job" (get-in entry [:evidence/body :edge/id])))
                  (is (= (:evidence-id edge)
                         (:evidence/id (estore/get-entry* reconstructed (:evidence-id edge)))))
                  (is (= [edge]
                         (json/parse-string
                          (json/generate-string (ledger/recent-mesh-edges 10 reconstructed))
                          true))))))))))
      (finally
        (stop! server)
        (.close ^java.lang.AutoCloseable node)
        (doseq [file (reverse (file-seq root))] (io/delete-file file true))))))
