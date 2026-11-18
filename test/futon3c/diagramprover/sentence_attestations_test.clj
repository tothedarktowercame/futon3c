(ns futon3c.diagramprover.sentence-attestations-test
  (:require [clojure.test :refer [deftest is]] [clojure.edn :as edn] [clojure.java.io :as io] [clojure.string :as str]
            [futon3c.diagramprover.sentence-records :as s]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.digest :as digest]))
;; Full stored captures and checksum-pinned durable runners are intentional inputs:
;; the earlier trimmed fixtures omit inventory reasons and runner provenance.
(def live-ids (set (edn/read-string (slurp "test/futon3c/diagramprover/fixtures/sentence-live-subjects.edn"))))
(def atts (delay (s/sentence-attestations (assoc s/attestation-inputs :live-subject-ids live-ids))))
(deftest identities-and-coverage
  (is (= 59 (count @atts)))
  (is (= live-ids (set (map :subject-clusion-id @atts))))
  (is (every? #(= (:id %) (:id (records/make-attestation %))) @atts))
  (is (= {:partial 53 :complete 6} (frequencies (map :coverage @atts))))
  (let [declaration (filter #(= :declaration (get-in % [:method :scope])) @atts)]
    (is (= 8 (count declaration)))
    (is (every? #(= :partial (:coverage %)) declaration))
    (is (not (every? #(= :partial (:coverage %)) (cons (assoc (first declaration) :coverage :complete) (rest declaration)))))))
(deftest method-and-runner-representation
  (let [b (s/records-from-switch-off (:switch-off s/attestation-inputs))
        rid (:id (get (:rules b) "mfuton.lean.blueprint_render.paper_proof_walk.render.proof_body"))
        outer (:id (first (:roles b)))
        app (first (filter #(and (= outer (get-in % [:endpoints 0 :target :clusion/id]))
                                 (= rid (get-in % [:endpoints 1 :target :clusion/id]))) (:applications b)))
        att (first (filter #(= (:id app) (:subject-clusion-id %)) @atts))
        m (:method att)
        checks (into {} (map #(let [[sha file] (str/split % #"\s+" 2)] [file sha])
                             (str/split-lines (slurp (str (:runners s/attestation-inputs) "/SHA256SUMS")))))]
    (is (some? att))
    (is (= [35 24 23 17] (mapv m [:inventory-entries :inventory-functions :interventions-tested :functions-tested])))
    (is (vector? (:untested m)))
    (is (= 12 (reduce + (map :count (:untested m)))))
    (is (= 12 (count (:untested-rows m))))
    (doseq [a @atts]
      (let [files (get-in a [:producer :runner-files])]
        (is (vector? files))
        (is (= (map :file files) (sort (map :file files))))
        (doseq [{:keys [file sha256]} files]
          (is (= sha256 (get checks file)))
          (is (= sha256 (digest/sha256-hex
                         (java.nio.file.Files/readAllBytes
                          (.toPath (io/file (:runners s/attestation-inputs) file)))))))))))
(deftest unknown-subject-refused
  (is (= :sentence/subject-set-mismatch
         (try (s/sentence-attestations (assoc s/attestation-inputs :live-subject-ids (conj live-ids "invented-nonlive-subject")))
              nil (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))
