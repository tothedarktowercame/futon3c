(ns futon3c.test-registry-environment-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry-test :as fixture]))

(defn environment-fixture [f]
  (let [fingerprint registry/fingerprint
        file-sha registry/file-sha
        ambient (atom {"LC_ALL" "en_US.UTF-8" "LANG" "en_US.UTF-8" "TZ" "Pacific/Honolulu"})]
    (fixture/fixture
     (fn [context]
       ;; Stub toolchain probes, not fingerprint construction or env policy.
       (with-redefs [registry/fingerprint fingerprint
                     registry/ambient-environment (fn [] @ambient)
                     registry/file-sha (fn [file]
                                          (if (.isFile (io/file file))
                                            (file-sha file)
                                            "fixture-toolchain-sha"))
                     registry/command!
                     (fn [_ argv]
                       (cond
                         (some #{"-Spath"} argv) "/fixture/classpath"
                         (= "which" (first argv)) "/fixture/clojure"
                         (some #{"-Sdescribe"} argv) "{:config-files []}"
                         :else "{\"java.home\" \"/fixture/jdk\"}"))]
         (f (assoc context :ambient ambient)))))))

(deftest ambient-locale-does-not-decide-warrants
  (environment-fixture
   (fn [{:keys [backend options ambient]}]
     (let [first-run (registry/register-run! backend options)]
       (reset! ambient {"LC_ALL" "fr_FR.UTF-8" "LANG" "de_DE.UTF-8" "TZ" "Asia/Tokyo"})
       (let [second-run (registry/register-run! backend options)
             fp (get-in first-run [:payload :env-fingerprint])]
         (is (get-in first-run [:payload :warrant?]))
         (is (get-in second-run [:payload :warrant?]))
         (is (= fp (get-in second-run [:payload :env-fingerprint])))
         (doseq [[k v] registry/canonical-environment]
           (is (= (registry/sha v) (get-in fp [:environment k]))))
         (swap! ambient assoc "JAVA_TOOL_OPTIONS" "-Xmx128m")
         (is (not= fp (registry/fingerprint options))))))))

(deftest retired-spec-refuses-before-execution
  (environment-fixture
   (fn [{:keys [backend options calls]}]
     (doseq [value [nil {} {"TZ" "UTC"}]]
       (try
         (registry/register-run! backend (assoc options :test-environment value))
         (is false "Spec must refuse")
         (catch clojure.lang.ExceptionInfo e
           (is (= :environment-not-configurable (:reason (ex-data e))))
           (is (= registry/canonical-environment
                  (get-in (ex-data e) [:details :canonical-environment]))))))
     (is (empty? @calls)))))

(deftest nested-check-agrees-with-plain-shell-check
  (environment-fixture
   (fn [{:keys [backend options ambient]}]
     (let [prior (registry/register-run! backend options)
           plain (registry/check-record! backend (fixture/check-options prior))
           execute registry/run-process! nested (atom nil)
           outer (with-redefs [registry/run-process!
                               (fn [& args]
                                 ;; The child sees the actual canonical environment.
                                 (let [parent @ambient]
                                   (try
                                     (reset! ambient (registry/effective-environment))
                                     (reset! nested (registry/check-record! backend (fixture/check-options prior)))
                                     (apply execute args)
                                     (finally (reset! ambient parent)))))]
                   (registry/register-run! backend options))]
       (is (:warrant? plain))
       (is (:warrant? @nested))
       (is (= (dissoc plain :checked-at) (dissoc @nested :checked-at)))
       (is (get-in outer [:payload :warrant?]))))))

(deftest child-process-receives-canonical-values
  (let [execute registry/run-process!]
    (fixture/fixture
     (fn [{:keys [options]}]
       (let [log (io/file (:artifact-dir options) "child-env.log")]
         (with-redefs [registry/execution-command
                       (fn [& _] ["sh" "-c" "printf '%s|%s|%s\\n' \"$LC_ALL\" \"$LANG\" \"$TZ\"; printf 'Ran 1 tests containing 1 assertions.\\n0 failures, 0 errors.\\n'"])
                       registry/ambient-environment
                       (fn [] {"LC_ALL" "wrong" "LANG" "wrong" "TZ" "wrong"})]
           (is (= 0 (:exit (execute (:artifact-dir options) [] log)))))
         (is (str/starts-with? (slurp log) "C.UTF-8|C.UTF-8|UTC\n")))))))
