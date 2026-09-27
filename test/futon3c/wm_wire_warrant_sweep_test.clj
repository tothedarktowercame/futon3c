(ns futon3c.wm-wire-warrant-sweep-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]))

(load-file "scripts/wm_wire_warrant_sweep.clj")
(def classify (ns-resolve 'wm-wire-warrant-sweep 'classify))
(def assess (ns-resolve 'wm-wire-warrant-sweep 'assess))
(def wire-namespaces (ns-resolve 'wm-wire-warrant-sweep 'wire-namespaces))

(deftest classification-uses-recorded-file-bytes
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "sweep-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file dir "source.clj")]
    (try
      (spit file "original")
      (let [run {:warrant? true :results {:failures 0 :errors 0}
                 :load-closure [{:ns "fixture" :path "source.clj" :sha256 (registry/file-sha file)}]}]
        (is (= :current (:class (classify (str dir) run))))
        (spit file "changed")
        (is (= {:class :stale-closure :changed-paths ["source.clj"] :changed-count 1}
               (classify (str dir) run)))
        (is (= :not-passing (:class (classify (str dir) (assoc-in run [:results :failures] 1)))))
        (is (= :no-warrant (:class (classify (str dir) nil))))
        (is (= :no-warrant (:class (classify (str dir) (assoc run :load-closure []))))))
      (finally (io/delete-file file true) (io/delete-file dir true)))))

(deftest failed-reads-are-not-missing-warrants
  (doseq [reader [(constantly {:reason :registry-read-failed})
                  (constantly nil)
                  (fn [_] (throw (ex-info "offline" {:reason :registry-read-failed})))]]
    (let [r (assess "." "fixture" {:entry-id "known-run"} reader)]
      (is (= :read-failed (:class r)))
      (is (= :registry-read-failed (:reason r)))))
  (is (= :no-warrant (:class (assess "." "fixture" nil (fn [_] (throw (Exception. "must not read"))))))))

(deftest namespace-list-is-read-without-loading-wire-tests
  (is (some #{"futon3c.diagramprover.wm-wire-r0-enact-step-flight-run-record-path-test"}
            (wire-namespaces "test/futon3c/diagramprover/wm_wire_ledger_test.clj"))))
