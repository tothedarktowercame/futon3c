(ns futon3c.wm.run4-historical-qualification-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-qualification :as q]))

(defn- fixture []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "historical-qualification-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        out (doto (io/file root "output") .mkdir)
        source (io/file root "source.txt") manifest (io/file root "plan.edn")]
    (spit source "frozen input\n")
    {:root root :out out :source source :manifest manifest}))

(defn- plan! [{:keys [root out source manifest]} checks]
  (let [plan {:schema :wm/historical-qualification-plan-v1
              :verification-id "verification-1" :repair-id "repair-057"
              :sources [{:path (.getPath source) :sha256 (digest/sha256 (slurp source))}]
              :checks checks}]
    (spit manifest (pr-str plan))
    {:source-root (.getPath root) :output-root (.getPath out)
     :manifest-path (.getPath manifest) :manifest-sha256 (digest/sha256 (slurp manifest))}))

(defn- check [id script]
  {:id id :argv ["/bin/sh" "-c" script] :timeout-ms 2000})

(deftest ^:slow actual-output-producer
  (let [f (fixture) opts (plan! f [(check :positive "printf 'executed\\n'")
                                  (check :negative "printf 'reject\\n' >&2; exit 3")])
        r (q/produce! opts)]
    (is (= [0 3] (mapv :exit (:checks r))))
    (is (= [:positive :negative] (mapv :id (:checks r))))
    (is (= "executed\n" (get-in r [:checks 0 :stdout :utf8])))
    (is (false? (:qualification-passed? r)))
    (is (false? (:repair-admitted? r)))
    (is (= :not-performed (:independent-review r)))
    (is (thrown? clojure.lang.ExceptionInfo (q/produce! opts)))))

(deftest ^:slow positive-and-drift-controls
  (testing "passing execution does not supply review or repair admission"
    (let [f (fixture) r (q/produce! (plan! f [(check :one "exit 0")]))]
      (is (true? (:qualification-passed? r)))
      (is (false? (:repair-admitted? r)))))
  (testing "source mutation during the actual command prevents publication"
    (let [f (fixture) opts (plan! f [(check :mutate "printf changed > source.txt")])]
      (is (thrown? clojure.lang.ExceptionInfo (q/produce! opts)))
      (is (not (.exists (io/file (:out f) "verification-1.qualification.edn")))))))

(deftest malformed-population-and-stale-manifest
  (doseq [checks [[] [(check :same "true") (check :same "true")]
                  [{:id :fake :passed? true}]]]
    (is (thrown? clojure.lang.ExceptionInfo (q/produce! (plan! (fixture) checks)))))
  (let [f (fixture) opts (plan! f [(check :one "true")])]
    (spit (:manifest f) "nil")
    (is (thrown? clojure.lang.ExceptionInfo (q/produce! opts)))))

(deftest ^:slow timeout-is-not-a-pass
  (let [f (fixture)
        r (q/produce! (plan! f [{:id :bounded :argv ["/bin/sleep" "2"]
                                :timeout-ms 10}]))]
    (is (true? (get-in r [:checks 0 :timed-out?])))
    (is (false? (:qualification-passed? r)))))

(deftest supplied-results-and-output-escape-refuse
  (let [f (fixture) opts (plan! f [(check :one "true")])
        target (.toPath (io/file (:out f) "verification-1.qualification.edn"))
        external (.toPath (io/file (:root f) "untouched"))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (q/produce! (assoc opts :results [{:passed? true}]))))
    (java.nio.file.Files/createSymbolicLink
     target external (make-array java.nio.file.attribute.FileAttribute 0))
    (is (thrown? clojure.lang.ExceptionInfo (q/produce! opts)))
    (is (not (.exists (.toFile external))))))
