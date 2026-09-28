(ns futon3c.test-registry.exercised-runner
  "Prototype runner that conservatively distinguishes called namespaces from
  namespaces that were only loaded. This does not define warrant semantics."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
            [futon3c.test-registry.runner :as runner]))

(defonce exercised (atom #{}))
(defonce instrumentation (atom {}))
(defonce called-definitions (atom {}))
(def ^:dynamic *call-context* nil)

(defn- source-url [namespace]
  (let [path (-> (str (ns-name namespace))
                 (str/replace "." "/")
                 (str/replace "-" "_"))]
    (some-> (or (io/resource (str path ".clj"))
                (io/resource (str path ".cljc")))
            str)))

(defn- file-namespace? [namespace]
  (some-> (source-url namespace) (str/starts-with? "file:")))

(defn- primitive-function? [value]
  (boolean
   (some #(re-matches #"clojure\.lang\.IFn\$[A-Z]+" (.getName ^Class %))
         (mapcat #(.getInterfaces ^Class %)
                 (take-while some? (iterate #(.getSuperclass ^Class %) (class value)))))))

(defn- callable-kind [^clojure.lang.Var var value]
  (cond
    (instance? clojure.lang.MultiFn value) :multimethod
    (:protocol (meta var)) :protocol
    (and (fn? value) (primitive-function? value)) :primitive-hinted-function
    (fn? value) :function
    :else nil))

(defn- wrap-var! [namespace ^clojure.lang.Var var]
  (let [value @var
        kind (callable-kind var value)
        name (str (:name (meta var)))]
    (cond
      (= :function kind)
      (try
        (let [definition [(str (ns-name namespace)) name]]
          (alter-var-root var
                          (fn [original]
                            (fn [& args]
                              (swap! exercised conj (ns-name namespace))
                              (swap! called-definitions
                                     #(if (contains? % definition)
                                        %
                                        (assoc % definition
                                               (or *call-context*
                                                   (atom {:phase :unknown})))))
                              (apply original args)))))
        nil
        (catch Throwable _ {:name name :kind :alter-failed}))

      (contains? #{:multimethod :protocol :primitive-hinted-function} kind)
      {:name name :kind kind}
      :else nil)))

(defn- instrument-namespace! [namespace]
  (when (and (file-namespace? namespace)
             (not (contains? @instrumentation (ns-name namespace))))
    (let [failures (->> (vals (ns-interns namespace))
                        (keep #(wrap-var! namespace %))
                        (sort-by (juxt :kind :name))
                        vec)]
      (swap! instrumentation assoc (ns-name namespace)
             (if (seq failures)
               {:status :exercised :reason :not-instrumentable
                :not-instrumentable failures}
               {:status :instrumented})))))

(defn- instrument-new-namespaces! [before]
  (doseq [namespace (sort-by (comp str ns-name)
                             (remove #(contains? before (ns-name %)) (all-ns)))]
    (instrument-namespace! namespace)))

(defn- hooked-load [original-load]
  (fn [& args]
    (let [before (set (map ns-name (all-ns)))
          context (atom {:phase :load :source (str (first args))})]
      (binding [*call-context* context]
        (try
          (apply original-load args)
          (finally
            (let [loaded (sort (remove before (map ns-name (all-ns))))]
              (reset! context {:phase :load
                               :source (str (first args))
                               :namespaces (mapv str loaded)})
              (instrument-new-namespaces! before))))))))

(defn- test-source? [url]
  (boolean (and url (re-find #"/test/" url))))

(defn- namespace-entry [test-namespace namespace]
  (let [name (ns-name namespace)
        url (source-url namespace)
        installed (get @instrumentation name)
        always? (or (= test-namespace name) (test-source? url))]
    (when (and url (str/starts-with? url "file:"))
      (merge {:ns (str name) :url url}
             (cond
               always? {:status :exercised :reason :test-source}
               (= :exercised (:status installed)) installed
               (contains? @exercised name) {:status :exercised}
               (= :instrumented (:status installed)) {:status :loaded-only}
               :else {:status :exercised :reason :instrumentation-not-installed})))))

(defn exercised-entries [test-namespace]
  (let [namespaces (keep #(namespace-entry test-namespace %) (all-ns))
        namespace-urls (set (map :url namespaces))
        resources (map #(assoc % :status :exercised :reason :resource)
                       (remove #(contains? namespace-urls (:url %))
                               (runner/resource-entries)))]
    (vec (concat namespaces resources))))

(defn called-definition-entries []
  (->> @called-definitions
       (map (fn [[[namespace name] context]]
              {:ns namespace :name name :first-call @context}))
       (sort-by (juxt :ns :name))
       vec))

(defn run-and-exercised [namespace-sym var-sym]
  (reset! exercised #{})
  (reset! instrumentation {})
  (reset! called-definitions {})
  (reset! runner/resource-lookups #{})
  (let [thread (Thread/currentThread)
        original-load @#'clojure.core/load
        already-loaded (filter file-namespace? (all-ns))]
    (.setContextClassLoader thread
                            (runner/recording-loader (.getContextClassLoader thread)))
    ;; This runner and its prerequisites were loaded before the hook existed;
    ;; conservatively mark them exercised rather than claim loaded-only.
    (doseq [namespace already-loaded]
      (swap! instrumentation assoc (ns-name namespace)
             {:status :exercised :reason :instrumentation-not-installed}))
    (let [summary
          (with-redefs-fn
            {#'clojure.core/load (hooked-load original-load)}
            (fn []
              (require namespace-sym)
              (binding [*call-context*
                        (atom {:phase :test :namespace (str namespace-sym)})]
                (if var-sym
                  (runner/run-var var-sym)
                  (test/run-tests namespace-sym)))))]
      {:summary summary
       :exercised-entries (exercised-entries namespace-sym)
       :called-definitions (called-definition-entries)})))

(defn -main [& args]
  (let [out (last args)
        pairs (apply hash-map (butlast args))
        ns-arg (get pairs "-n")
        var-arg (get pairs "-v")]
    (when (or (not (odd? (count args))) (nil? ns-arg)
              (not (every? #{"-n" "-v"} (keys pairs))))
      (binding [*out* *err*]
        (println "usage: -m futon3c.test-registry.exercised-runner -n <namespace> [-v <ns/var>] <output-path>"))
      (System/exit 2))
    (let [{:keys [summary exercised-entries called-definitions]}
          (run-and-exercised (symbol ns-arg) (some-> var-arg symbol))]
      (spit out (pr-str {:namespace-entries exercised-entries
                         :called-definitions called-definitions}))
      (shutdown-agents)
      (System/exit (if (and (zero? (:fail summary 1))
                            (zero? (:error summary 1))) 0 1)))))
