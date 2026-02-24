(ns futon3c.reflection.core
  "Clojure runtime reflection — pure functions over the live JVM.

   Six functions that answer structural questions about Clojure namespaces,
   vars, dependencies, and Java classes. No side effects, no eval, no
   network calls — just metadata reads from the running runtime.

   This is the bottom-out layer for the self-representing stack: every
   strategic claim about code must resolve to data from these functions.

   Derived from: M-self-representing-stack §Reflection-Grounded Claim Surfaces"
  (:require [clojure.reflect :as reflect]
            [clojure.string :as str]
            [futon3c.reflection.envelope :as envelope])
  (:import [java.time Instant]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- var-summary
  "Extract summary metadata from a var. Used by reflect-ns / reflect-ns-full."
  [v]
  (let [m (meta v)]
    {:name (:name m)
     :arglists (:arglists m)
     :doc (:doc m)
     :file (:file m)
     :line (:line m)
     :private? (boolean (:private m))
     :macro? (boolean (:macro m))
     :dynamic? (boolean (:dynamic m))}))

(defn- ns-meta-map
  "Build a summary map for a namespace object."
  [ns-obj]
  (let [m (meta ns-obj)]
    {:ns (ns-name ns-obj)
     :doc (:doc m)
     :file (:file m)}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn list-namespaces
  "All loaded namespaces, sorted by name.

   Returns [{:ns sym :doc str :file str}].
   With a pattern argument, filters to namespaces matching the regex."
  ([]
   (->> (all-ns)
        (sort-by ns-name)
        (mapv ns-meta-map)))
  ([pattern]
   (let [re (if (instance? java.util.regex.Pattern pattern)
              pattern
              (re-pattern (str pattern)))]
     (->> (all-ns)
          (filter #(re-find re (str (ns-name %))))
          (sort-by ns-name)
          (mapv ns-meta-map)))))

(defn reflect-ns
  "Public vars in a namespace, sorted by name.

   Returns [{:name sym :arglists ... :doc ... :file ... :line ... :private? false ...}],
   or {:error ...} if the namespace doesn't exist."
  [ns-sym]
  (if-let [ns-obj (find-ns ns-sym)]
    (->> (ns-publics ns-obj)
         vals
         (sort-by #(:name (meta %)))
         (mapv var-summary))
    {:error (str "Namespace not found: " ns-sym)}))

(defn reflect-ns-full
  "All vars (public + private) in a namespace, sorted by name.

   Returns [{:name sym :arglists ... :doc ... :file ... :line ... :private? bool ...}],
   or {:error ...} if the namespace doesn't exist."
  [ns-sym]
  (if-let [ns-obj (find-ns ns-sym)]
    (->> (ns-interns ns-obj)
         vals
         (sort-by #(:name (meta %)))
         (mapv var-summary))
    {:error (str "Namespace not found: " ns-sym)}))

(defn reflect-var
  "Full metadata for one var as a ReflectionEnvelope.

   Returns a validated envelope map with :reflection/ns, :reflection/symbol,
   :reflection/file, :reflection/line, :reflection/arglists, :reflection/doc,
   :reflection/resolved-at, etc.

   Returns {:error ...} if the namespace or var doesn't exist."
  [ns-sym var-sym]
  (if-let [ns-obj (find-ns ns-sym)]
    (if-let [v (ns-resolve ns-obj var-sym)]
      (envelope/->envelope ns-sym var-sym v)
      {:error (str "Var not found: " ns-sym "/" var-sym)})
    {:error (str "Namespace not found: " ns-sym)}))

(defn reflect-deps
  "Namespace dependency graph.

   Returns {:requires [syms] :imports [classes] :required-by [syms]},
   or {:error ...} if the namespace doesn't exist.

   :required-by is computed by scanning all loaded namespaces for those
   that require the target — this can be slow for the first call but is
   typically fast (hundreds of namespaces, not thousands)."
  [ns-sym]
  (if-let [ns-obj (find-ns ns-sym)]
    (let [;; What this namespace requires
          requires (->> (ns-aliases ns-obj)
                        vals
                        (map ns-name)
                        sort
                        vec)
          ;; What this namespace imports
          imports (->> (ns-imports ns-obj)
                       vals
                       (map #(.getName ^Class %))
                       (remove #(str/starts-with? % "java.lang."))
                       sort
                       vec)
          ;; Who requires this namespace (reverse lookup)
          required-by (->> (all-ns)
                           (filter (fn [other-ns]
                                     (and (not= (ns-name other-ns) ns-sym)
                                          (some #{ns-obj} (vals (ns-aliases other-ns))))))
                           (map ns-name)
                           sort
                           vec)]
      {:requires requires
       :imports imports
       :required-by required-by})
    {:error (str "Namespace not found: " ns-sym)}))

(defn reflect-java-class
  "Clojure reflection on a Java class.

   Returns {:name str :bases [str] :flags #{keyword} :members [...]},
   or {:error ...} if the class doesn't exist.

   Members are sorted by name and include :name, :type/:return-type,
   :parameter-types, and :flags."
  [class-name]
  (try
    (let [cls (Class/forName (str class-name))
          {:keys [bases flags members]} (reflect/reflect cls)]
      {:name (.getName cls)
       :bases (->> bases (map str) sort vec)
       :flags flags
       :members (->> members
                     (sort-by (comp str :name))
                     (mapv (fn [m]
                             (cond-> {:name (str (:name m))
                                      :flags (:flags m)}
                               (:type m)
                               (assoc :type (str (:type m)))
                               (:return-type m)
                               (assoc :return-type (str (:return-type m)))
                               (:parameter-types m)
                               (assoc :parameter-types
                                      (mapv str (:parameter-types m)))))))})
    (catch ClassNotFoundException _
      {:error (str "Class not found: " class-name)})))
