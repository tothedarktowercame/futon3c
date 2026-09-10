(ns futon3c.wm.run4-report-service-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-report-service :as sut]
            [futon3c.wm.run4-trusted-entry :as trusted]))

(deftest missing-durable-evidence-is-incomplete-not-success
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-report" (make-array java.nio.file.attribute.FileAttribute 0)))
        casting {:author "zai-2" :reviewer "codex-12" :repair-reviewer "codex-17"}
        pin-text "{:pin true}"
        pin-sha (digest/sha256 pin-text)
        manifest {:schema :wm/run4-series-pin-v1 :series-id "s" :casting casting
                  :trials [{:ordinal 1 :trial-id "t" :attempt-id "a"
                            :pin-sha256 pin-sha :packet {:path "pin"}}]}
        mf (java.io.File. root "series.edn")]
    (try
      (spit mf (pr-str manifest))
      (spit (io/file root "pin") pin-text)
      (let [text (slurp mf)
            config {:run4 {:casting casting :pin-root (.getPath root) :pin-allowlist #{"pin"}
                                   :acceptance {:control-map-root (.getPath root)
                                                :control-map-ref "missing.edn"}
                                   :series {:manifest-root (.getPath root)
                                            :manifest-ref "series.edn"
                                            :manifest-sha256 (digest/sha256 text)
                                            :manifest-allowlist #{"series.edn"}
                                            :controller-root (.getPath root)
                                            :visibility-root (.getPath root)}}}]
        (with-redefs [trusted/authenticate (fn [_ _] {:ok true})]
          (let [r (sut/report! config {})]
            (is (= :incomplete-durable-evidence (:decision r)))
            (is (false? (:accepted? r))))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f true))))))

(deftest authentication-precedes-filesystem-authority-reads
  (with-redefs [trusted/authenticate (fn [_ _] {:ok false :error :run4-authentication-failed})]
    (is (= :run4-authentication-failed
           (:reason (try (sut/report! {:run4 {:series {:manifest-root "/missing"}}} {}) nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
