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
    :new "(ns fixture) (defn called [] 2)" :test "(ns consumer) (deftest check (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "private" :old "(ns fixture) (defn- helper [] 1) (defn middle [] (helper)) (defn called [] (middle))"
    :new "(ns fixture) (defn- helper [] 2) (defn middle [] (helper)) (defn called [] (middle))"
    :test "(ns consumer) (deftest check (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "separate-test" :old "(ns fixture) (defn called [] 1)"
    :new "(ns fixture) (defn called [] 2)" :test "(ns consumer) (deftest check (f/called))"
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
    :new "(ns fixture)" :test "(ns consumer) (deftest check (f/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   ;; a consumer's own var of the same short name is not a call into the changed file
   {:id "own-main" :old "(ns own-main) (defn called [] 1) (defn -main [] (called))"
    :new "(ns own-main) (defn called [] 2) (defn -main [] (called))"
    :test "(ns consumer (:require [clojure.string :as str])) (defn -main [] (str/trim \"x\"))"
    :class :current-by-form :reason :changed-definitions-unreachable}
   {:id "aliased" :old "(ns aliased) (defn called [] 1)" :new "(ns aliased) (defn called [] 2)"
    :test "(ns consumer (:require [aliased :as wm])) (deftest check (wm/called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "other-alias" :old "(ns other-alias) (defn called [] 1)" :new "(ns other-alias) (defn called [] 2)"
    :test "(ns consumer (:require [elsewhere :as e] [other-alias :as wm])) (defn check [] (e/called) (wm/untouched))"
    :class :current-by-form :reason :changed-definitions-unreachable}
   {:id "referred" :old "(ns referred) (defn called [] 1)" :new "(ns referred) (defn called [] 2)"
    :test "(ns consumer (:require [referred :refer [called]])) (deftest check (called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "refer-all" :old "(ns refer-all) (defn called [] 1)" :new "(ns refer-all) (defn called [] 2)"
    :test "(ns consumer (:require [refer-all :refer :all])) (deftest check (called))"
    :class :stale-closure :reason :changed-definition-reachable}
   {:id "by-string" :old "(ns by-string) (defn called [] 1)" :new "(ns by-string) (defn called [] 2)"
    :test "(ns consumer) (deftest check ((resolve (symbol \"by-string\" \"called\"))))"
    :class :stale-closure :reason :changed-definition-reachable}
   ;; prose about the changed namespace is not a call into it
   {:id "docstring" :old "(ns docstring) (defn called [] 1)" :new "(ns docstring) (defn called [] 2)"
    :test "(ns consumer \"see docstring/called\") ;; docstring/called\n(defn check \"Used by docstring called.\" [] 1)"
    :class :current-by-form :reason :changed-definitions-unreachable}
   {:id "run-time-require" :old "(ns run-time-require) (defn called [] 1)" :new "(ns run-time-require) (defn called [] 2)"
    :test "(ns consumer) (deftest check (require 'run-time-require) ((resolve 'called)))"
    :class :stale-closure :reason :changed-definition-reachable}
   ;; a consumer using ::alias/key and a tagged literal is still readable
   {:id "aliased-keyword" :old "(ns aliased-keyword) (defn called [] 1)" :new "(ns aliased-keyword) (defn called [] 2)"
    :test "(ns consumer (:require [elsewhere :as e])) (defn check [] [::e/k #inst \"2026-01-01\" #unknown/tag {:a 1}])"
    :class :current-by-form :reason :changed-definitions-unreachable}
   ;; file to file: the test reaches the change only through a middle file
   {:id "through-middle" :old "(ns through-middle) (defn called [] 1)" :new "(ns through-middle) (defn called [] 2)"
    :mid "(ns middle (:require [through-middle :as wm])) (defn bridge [] (wm/called)) (defn apart [] 1)"
    :test "(ns consumer (:require [middle :as m])) (deftest check (m/bridge))"
    :names ['bridge]
    :class :stale-closure :reason :changed-definition-reachable}
   ;; the middle file calls the change, but from a function the test never uses
   {:id "beside-middle" :old "(ns beside-middle) (defn called [] 1)" :new "(ns beside-middle) (defn called [] 2)"
    :mid "(ns middle (:require [beside-middle :as wm])) (defn bridge [] (wm/called)) (defn apart [] 1)"
    :test "(ns consumer (:require [middle :as m])) (deftest check (m/apart))"
    :class :current-by-form :reason :changed-definitions-unreachable}
   ;; a top-level call in the middle file runs at load, whoever calls what
   {:id "at-load" :old "(ns at-load) (defn called [] 1)" :new "(ns at-load) (defn called [] 2)"
    :mid "(ns middle (:require [at-load :as wm])) (wm/called)"
    :test "(ns consumer (:require [middle :as m])) (deftest check 1)"
    :class :stale-closure :reason :changed-definition-reachable}
   ;; a helper in the test file that no deftest uses is not a run
   {:id "unused-helper" :old "(ns unused-helper) (defn called [] 1)" :new "(ns unused-helper) (defn called [] 2)"
    :test "(ns consumer (:require [unused-helper :as wm])) (defn helper [] (wm/called)) (deftest check 1)"
    :class :current-by-form :reason :changed-definitions-unreachable}
   {:id "used-helper" :old "(ns used-helper) (defn called [] 1)" :new "(ns used-helper) (defn called [] 2)"
    :test "(ns consumer (:require [used-helper :as wm])) (defn helper [] (wm/called)) (deftest check (helper))"
    :names ['helper]
    :class :stale-closure :reason :changed-definition-reachable}
   ;; a record is used through ->Name, so its change is never cleared by name
   {:id "record" :old "(ns record) (defrecord Thing [a])" :new "(ns record) (defrecord Thing [a b])"
    :test "(ns consumer (:require [record :as r])) (deftest check (r/->Thing 1))"
    :class :stale-closure :reason :unnamed-form-changed}
   {:id "quoted" :old "(ns quoted) (defn called [] 1)" :new "(ns quoted) (defn called [] 2)"
    :test "(ns consumer) (deftest check ((requiring-resolve 'quoted/called)))"
    :class :stale-closure :reason :changed-definition-reachable}])

(deftest form-grain-against-real-git-history
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "sweep-forms" (make-array java.nio.file.attribute.FileAttribute 0)))
        git (fn [& args]
              (let [r (apply shell/sh (concat ["timeout" "10s" "git"] args [:dir (str dir)]))]
                (assert (zero? (:exit r)) (pr-str r))))]
    (try
      (git "init" "-q")
      (doseq [{:keys [id old test mid]} cases]
        (spit (io/file dir (str id ".clj")) old)
        (when mid (spit (io/file dir (str id "_mid.clj")) mid))
        (spit (io/file dir (str id "_test.clj")) test))
      (git "add" ".")
      (git "-c" "user.name=fixture" "-c" "user.email=fixture@invalid" "-c" "commit.gpgsign=false" "commit" "-qm" "fixture")
      (doseq [{:keys [id new class reason missing? separate-test? mid names]} cases]
        (let [file (io/file dir (str id ".clj"))
              test-file (io/file dir (str id "_test.clj"))
              mid-file (io/file dir (str id "_mid.clj"))
              closure (cond-> [{:path (.getName file) :sha256 (if missing? "not-a-sha" (registry/file-sha file))}
                               {:path (.getName test-file) :sha256 (registry/file-sha test-file)}]
                        mid (conj {:path (.getName mid-file) :sha256 (registry/file-sha mid-file)}))
              _ (spit file new)
              result (classify (str dir) {:warrant? true :results {:failures 0 :errors 0}
                                          :load-closure (if separate-test? [(first closure)] closure)
                                          :test-files (when separate-test? {(.getName test-file) (registry/file-sha test-file)})})]
          (is (= class (:class result)) (str id " " result))
          (is (= reason (:reason result)) (str id " " result))
          (when (= reason :changed-definition-reachable)
            (is (= (or names ['called]) (get-in result [:form-check 0 :consumer :names])) (pr-str result)))
          (when (= id "private")
            (is (= ['called 'helper 'middle] (get-in result [:form-check 0 :reachable-names]))))))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f true))))))
