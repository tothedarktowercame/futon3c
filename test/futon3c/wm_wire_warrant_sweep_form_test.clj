(ns futon3c.wm-wire-warrant-sweep-form-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]))

(load-file "scripts/wm_wire_warrant_sweep.clj")
(def classify (ns-resolve 'wm-wire-warrant-sweep 'classify))

(def cases
  [{:id "unmentioned" :old "(ns fixture) (defn unused [] 1)"
    :new "(ns fixture) (defn unused [] 2)" :test "(ns consumer) (defn check [] 1)"
    :class :current-by-form :reason :changed-definitions-unreachable}
   {:id "called" :old "(ns fixture) (defn called [] 1)"
    :new "(ns fixture) (defn called [] 2)" :test "(ns consumer) (defn check [] (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "private" :old "(ns fixture) (defn- helper [] 1) (defn middle [] (helper)) (defn called [] (middle))"
    :new "(ns fixture) (defn- helper [] 2) (defn middle [] (helper)) (defn called [] (middle))"
    :test "(ns consumer) (defn check [] (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "separate-test" :old "(ns fixture) (defn called [] 1)"
    :new "(ns fixture) (defn called [] 2)" :test "(ns consumer) (defn check [] (f/called))"
    :separate-test? true :class :stale-closure :reason :changed-definition-reachable}
   {:id "ns" :old "(ns fixture) (defn unused [] 1)"
    :new "(ns other) (defn unused [] 1)" :test "(ns consumer)"
    :class :stale-closure :reason :ns-form-changed}
   {:id "method" :old "(ns fixture) (defmethod foo :x [_] 1)"
    :new "(ns fixture) (defmethod foo :x [_] 2)" :test "(ns consumer)"
    :class :stale-closure :reason :unnamed-form-changed}
   {:id "missing" :old "(ns fixture) (defn unused [] 1)"
    :new "(ns fixture) (defn unused [] 2)" :test "(ns consumer)" :missing? true
    :class :stale-closure :reason :old-content-unavailable}
   {:id "removed" :old "(ns fixture) (defn called [] 1)"
    :new "(ns fixture)" :test "(ns consumer) (defn check [] (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}])

(deftest form-grain-against-real-git-history
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "sweep-forms" (make-array java.nio.file.attribute.FileAttribute 0)))
        git (fn [& args]
              (let [r (apply shell/sh (concat ["timeout" "10s" "git"] args [:dir (str dir)]))]
                (assert (zero? (:exit r)) (pr-str r))))]
    (try
      (git "init" "-q")
      (doseq [{:keys [id old test]} cases]
        (spit (io/file dir (str id ".clj")) old)
        (spit (io/file dir (str id "_test.clj")) test))
      (git "add" ".")
      (git "-c" "user.name=fixture" "-c" "user.email=fixture@invalid" "-c" "commit.gpgsign=false" "commit" "-qm" "fixture")
      (doseq [{:keys [id new class reason missing? separate-test?]} cases]
        (let [file (io/file dir (str id ".clj"))
              test-file (io/file dir (str id "_test.clj"))
              closure [{:path (.getName file) :sha256 (if missing? "not-a-sha" (registry/file-sha file))}
                       {:path (.getName test-file) :sha256 (registry/file-sha test-file)}]
              _ (spit file new)
              result (classify (str dir) {:warrant? true :results {:failures 0 :errors 0}
                                          :load-closure (if separate-test? [(first closure)] closure)
                                          :test-files (when separate-test? {(.getName test-file) (registry/file-sha test-file)})})]
          (is (= class (:class result)) (str id " " result))
          (is (= reason (:reason result)) (str id " " result))
          (when (= reason :changed-definition-reachable)
            (is (= ['called] (get-in result [:form-check 0 :consumer :names])) (pr-str result)))
          (when (= id "private")
            (is (= ['called 'helper 'middle] (get-in result [:form-check 0 :reachable-names]))))))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f true))))))
