(ns futon3c.test-registry.runner
  "Registry-owned test runner. Runs the declared namespace (or one var of it)
  in THIS JVM via clojure.test, printing the standard summary (\"Ran N tests
  containing M assertions.\\nX failures, Y errors.\"), then writes the load
  closure to the file the registry names: every loaded namespace whose source
  resolves to a file: resource, plus every file: resource the run looked up by
  name through the context class loader (see recording-loader). Because the
  closure is recorded AFTER the tests run, in the same JVM, namespaces loaded
  while tests run
  (requiring-resolve, run-time require in a test body or fixture) are covered;
  a fresh-JVM require probe under-approximates them.

  Standalone on purpose: it runs on the test command's own classpath (for
  example :test-pure) and cannot depend on futon3c.test-registry.

  The registry injects this namespace as a -Sdeps alias appended after the
  declared alias, so its :main-opts replace the declared alias's runner:
    clojure -Sdeps <cfg> -M:<alias>:futon3c.test-registry/runner -n <ns> [-v <ns/var>] <closure-out>
  Exit code 0 iff zero failures and zero errors."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defonce ^{:doc "Classpath resources looked up through the recording loader:
  #{[resource-name url-string]}."} resource-lookups (atom #{}))

(defn recording-loader
  "A context class loader that delegates to PARENT and records every resource
  it finds. clojure.java.io/resource and Clojure's own DynamicClassLoader both
  resolve through the context loader, so data files the tests open by resource
  name are recorded. Lookups through another loader (Class/getResource,
  ClassLoader/getSystemResource) and plain file paths are not."
  ^ClassLoader [^ClassLoader parent]
  (proxy [ClassLoader] [parent]
    (getResource [^String nm]
      (let [u (.getResource parent nm)]
        (when u (swap! resource-lookups conj [nm (str u)]))
        u))
    (getResources [^String nm]
      (let [us (enumeration-seq (.getResources parent nm))]
        (swap! resource-lookups into (map (fn [u] [nm (str u)])) us)
        (java.util.Collections/enumeration (vec us))))))

(defn resource-entries
  "Recorded non-class file: resources, as closure entries named resource:<name>."
  []
  (vec (for [[nm u] (sort @resource-lookups)
             :when (and (str/starts-with? u "file:") (not (str/ends-with? nm ".class")))]
         {:ns (str "resource:" nm) :url u})))

(defn closure-entries
  "Every currently loaded namespace with a file: source resource."
  []
  (let [res (fn [nm]
              (let [b (-> nm (str/replace \. \/) (str/replace \- \_))]
                (or (io/resource (str b ".clj"))
                    (io/resource (str b ".cljc")))))]
    (vec (for [n (all-ns)
               :let [nm (str (ns-name n))
                     u (some-> (res nm) str)]
               :when (and u (str/starts-with? u "file:"))]
           {:ns nm :url u}))))

(defn run-var
  "Run exactly one test var and report the standard summary."
  [var-sym]
  (let [v (requiring-resolve var-sym)]
    (binding [test/*report-counters* (ref test/*initial-report-counters*)]
      (when v (test/test-vars [v]))
      (let [summary (assoc @test/*report-counters* :type :summary)]
        (test/do-report summary)
        summary))))

(defn run-and-closure
  "Run the namespace's tests, or VAR-SYM alone when given; return
  {:summary … :closure-entries …} without exiting."
  [namespace-sym var-sym]
  (let [thread (Thread/currentThread)]
    (.setContextClassLoader thread (recording-loader (.getContextClassLoader thread))))
  (require namespace-sym)
  (let [summary (if var-sym (run-var var-sym) (test/run-tests namespace-sym))]
    {:summary summary :closure-entries (into (closure-entries) (resource-entries))}))

(defn -main
  [& args]
  (let [out (last args)
        pairs (apply hash-map (butlast args))
        ns-arg (get pairs "-n")
        var-arg (get pairs "-v")]
    (when (or (not (odd? (count args))) (nil? ns-arg)
              (not (every? #{"-n" "-v"} (keys pairs))))
      (binding [*out* *err*]
        (println "usage: -m futon3c.test-registry.runner -n <namespace> [-v <ns/var>] <closure-out-path>"))
      (System/exit 2))
    (let [{:keys [summary closure-entries]} (run-and-closure (symbol ns-arg)
                                                             (some-> var-arg symbol))]
      (spit out (pr-str closure-entries))
      (shutdown-agents)
      (System/exit (if (and (zero? (:fail summary 1)) (zero? (:error summary 1))) 0 1)))))
