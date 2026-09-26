(ns futon3c.wm.run4-run-visibility-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-run-visibility :as sut]))

(def now "2026-09-10T12:00:00Z")
(def pin (apply str (repeat 64 "a")))
(def manifest
  {:schema :wm/run4-series-pin-v1 :series-id "RUN4-test" :status :frozen
   :order :ordinal :stop-rule :attempt-each-once-even-after-fail-or-block
   :casting {:author "zai-2" :reviewer "codex-17" :repair-reviewer "codex-1"}
   :source-pins [{:path "source" :sha256 pin}]
   :trials [{:ordinal 1 :trial-id :outer :attempt-id "attempt-1" :pin-sha256 pin
             :packet {:path "packet" :sha256 pin}}]})

(defn- temp-root [] (.toFile (java.nio.file.Files/createTempDirectory "run4-vis" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- clean! [r] (doseq [f (reverse (file-seq r))] (io/delete-file f true)))
(defn- write! [f x] (spit f (str (pr-str x) "\n")))
(defn- base [text]
  {:schema :wm/run4-series-open-v1 :series-id "RUN4-test"
   :manifest-sha256 (digest/sha256 text) :trial-count 1})
(defn- started [text]
  {:schema :wm/run4-series-started-v1 :series-id "RUN4-test"
   :manifest-sha256 (digest/sha256 text) :ordinal 1 :trial-id :outer
   :attempt-id "attempt-1" :pin-sha256 pin :click-id "click-1"
   :started-at "2026-09-10T11:00:00Z" :admission-state :click-recorded})

(deftest absent-enacted-evidence-publishes-nothing
  (let [r (temp-root) target (io/file r "run-visibility.json") text (pr-str manifest)]
    (try (is (nil? (sut/observe (.getPath r) text (constantly nil) now)))
         (is (nil? (sut/publish! target nil))) (is (not (.exists target)))
         (finally (clean! r)))))

(deftest present-falsey-series-artifacts-are-corrupt-not-absent
  (doseq [bad [nil false]]
    (let [r (temp-root) text (pr-str manifest)]
      (try
        (write! (io/file r "series.edn") bad)
        (is (= :malformed
               (:reason (try (sut/observe (.getPath r) text (constantly nil) now) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (finally (clean! r))))))

(deftest present-falsey-start-and-terminal-artifacts-refuse
  (doseq [phase ["started" "terminal"] bad [nil false]]
    (let [r (temp-root) text (pr-str manifest)]
      (try
        (write! (io/file r "series.edn") (base text))
        (write! (io/file r (str "001-" phase ".edn")) bad)
        (is (= :malformed
               (:reason (try (sut/observe (.getPath r) text (constantly nil) now) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (finally (clean! r))))))

(deftest unknown-is-pending-and-never-success
  (let [r (temp-root) text (pr-str manifest)]
    (try (write! (io/file r "series.edn") (base text))
         (write! (io/file r "001-started.edn") (started text))
         (let [v (sut/observe (.getPath r) text (constantly nil) now)]
           (is (= ["working" "pending"] ((juxt :stage :result) v)))
           (is (= ["working" "pending"] ((juxt :stage :result) (first (:trials v))))))
         (finally (clean! r)))))

(deftest authoritative-terminal-join-projects-without-acceptance
  (let [r (temp-root) text (pr-str manifest)
        evidence {:task-result :succeeded :infrastructure :safe :evidence-id pin}]
    (try (write! (io/file r "series.edn") (base text))
         (write! (io/file r "001-started.edn") (started text))
         (write! (io/file r "001-terminal.edn")
                 (merge {:schema :wm/run4-series-terminal-v1 :series-id "RUN4-test"
                         :manifest-sha256 (digest/sha256 text) :ordinal 1 :trial-id :outer
                         :attempt-id "attempt-1" :pin-sha256 pin} evidence))
         (let [v (sut/observe (.getPath r) text (constantly evidence) now)
               target (io/file r "run-visibility.json")]
           (sut/publish! target v)
           (is (= ["complete" "passed"] ((juxt :stage :result) v)))
           (is (= ":outer" (get-in v [:trials 0 :trial_id])))
           (is (not-any? #{"accepted"} (cons (:stage v) (map :stage (:trials v)))))
           (is (= "wm/run-visibility-v1" (:schema (json/parse-string (slurp target) true)))))
         (finally (clean! r)))))

(deftest conflicts-and-corruption-refuse-not-green
  (let [r (temp-root) text (pr-str manifest) evidence {:task-result :failed :infrastructure :safe :evidence-id pin}]
    (try (write! (io/file r "series.edn") (base text))
         (write! (io/file r "001-started.edn") (started text))
         (write! (io/file r "001-terminal.edn")
                 {:schema :wm/run4-series-terminal-v1 :series-id "RUN4-test"
                  :manifest-sha256 (digest/sha256 text) :ordinal 1 :trial-id :outer
                  :attempt-id "attempt-1" :pin-sha256 pin :task-result :succeeded
                  :infrastructure :safe :evidence-id pin})
         (is (= :terminal-evidence-conflict
                (:reason (try (sut/observe (.getPath r) text (constantly evidence) now) nil
                              (catch clojure.lang.ExceptionInfo e (ex-data e))))))
         (spit (io/file r "001-terminal.edn") "{")
         (is (= :malformed
                (:reason (try (sut/observe (.getPath r) text (constantly nil) now) nil
                              (catch clojure.lang.ExceptionInfo e (ex-data e))))))
         (finally (clean! r)))))

(deftest validated-not-attempted-lifecycle-is-visible-and-not-success
  (let [r (temp-root) text (pr-str manifest)
        terminal {:schema :wm/run4-series-terminal-v1 :series-id "RUN4-test"
                  :manifest-sha256 (digest/sha256 text) :ordinal 1 :trial-id :outer
                  :attempt-id "attempt-1" :pin-sha256 pin :task-result :not-attempted
                  :infrastructure :unsafe :reason :busy-admission-rejected}
        lifecycle {:schema :wm/run4-series-lifecycle-view-v1 :series-id "RUN4-test"
                   :manifest-sha256 (digest/sha256 text)
                   :trials [{:ordinal 1 :trial (first (:trials manifest))
                             :started nil :terminal terminal}]}]
    (try
      (write! (io/file r "series.edn") (base text))
      (write! (io/file r "001-terminal.edn") terminal)
      (let [v (sut/observe (.getPath r) text (constantly nil) now lifecycle)
            target (io/file r "run-visibility.json")]
        (sut/publish! target v)
        (is (= ["blocked" "blocked"] ((juxt :stage :result) v)))
        (is (= "busy admission rejected" (get-in v [:trials 0 :blocked_reason])))
        (is (= "blocked" (:stage (json/parse-string (slurp target) true)))))
      (finally (clean! r)))))
