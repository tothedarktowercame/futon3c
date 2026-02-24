(ns futon3c.reflection.core-test
  "Tests for the Clojure runtime reflection layer.

   Tests against known stable namespaces (clojure.core, clojure.string)
   and the reflection namespace itself (self-reflection)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.reflection.core :as r]
            [futon3c.reflection.envelope :as envelope]))

;; =============================================================================
;; list-namespaces
;; =============================================================================

(deftest list-namespaces-returns-loaded-namespaces
  (let [nses (r/list-namespaces)]
    (is (vector? nses))
    (is (pos? (count nses)))
    (is (some #(= 'clojure.core (:ns %)) nses)
        "clojure.core should always be loaded")
    (is (some #(= 'clojure.string (:ns %)) nses)
        "clojure.string should be loaded (required by core.clj)")))

(deftest list-namespaces-sorted
  (let [nses (r/list-namespaces)
        names (map :ns nses)]
    (is (= names (sort names))
        "Namespaces should be sorted by name")))

(deftest list-namespaces-entries-have-expected-keys
  (let [entry (first (r/list-namespaces))]
    (is (contains? entry :ns))
    (is (symbol? (:ns entry)))))

(deftest list-namespaces-with-pattern-filters
  (let [nses (r/list-namespaces "clojure\\.string")]
    (is (= 1 (count nses)))
    (is (= 'clojure.string (:ns (first nses))))))

(deftest list-namespaces-with-regex-pattern
  (let [nses (r/list-namespaces #"futon3c\.reflection\.")]
    (is (>= (count nses) 2) "Should find core and envelope at minimum")
    (is (every? #(re-find #"futon3c\.reflection\." (str (:ns %))) nses))))

(deftest list-namespaces-no-match-returns-empty
  (let [nses (r/list-namespaces "zzz-nonexistent-namespace-zzz")]
    (is (= [] nses))))

;; =============================================================================
;; reflect-ns
;; =============================================================================

(deftest reflect-ns-returns-public-vars
  (let [vars (r/reflect-ns 'clojure.string)]
    (is (vector? vars))
    (is (pos? (count vars)))
    (is (some #(= 'join (:name %)) vars) "clojure.string/join should be public")
    (is (some #(= 'split (:name %)) vars) "clojure.string/split should be public")))

(deftest reflect-ns-var-entries-have-metadata
  (let [join-var (->> (r/reflect-ns 'clojure.string)
                      (filter #(= 'join (:name %)))
                      first)]
    (is join-var "join should exist")
    (is (:arglists join-var) "Should have arglists")
    (is (:doc join-var) "Should have doc")
    (is (:file join-var) "Should have file")
    (is (:line join-var) "Should have line")
    (is (false? (:private? join-var)) "join is public")))

(deftest reflect-ns-excludes-private-vars
  (let [vars (r/reflect-ns 'clojure.string)
        names (set (map :name vars))]
    ;; clojure.string has private helpers; they should not appear
    (is (not (contains? names 'replace-by))
        "Private var replace-by should not be in public listing")))

(deftest reflect-ns-nonexistent-returns-error
  (let [result (r/reflect-ns 'zzz.nonexistent.namespace)]
    (is (:error result))
    (is (re-find #"not found" (:error result)))))

(deftest reflect-ns-sorted-by-name
  (let [vars (r/reflect-ns 'clojure.string)
        names (map :name vars)]
    (is (= names (sort names)))))

;; =============================================================================
;; reflect-ns-full
;; =============================================================================

(deftest reflect-ns-full-includes-private-vars
  (let [vars (r/reflect-ns-full 'clojure.string)
        names (set (map :name vars))]
    (is (contains? names 'join) "Should include public vars")
    (is (> (count vars) (count (r/reflect-ns 'clojure.string)))
        "Full listing should include more vars than public-only")))

(deftest reflect-ns-full-marks-private-vars
  (let [private-vars (->> (r/reflect-ns-full 'clojure.string)
                          (filter :private?))]
    (is (pos? (count private-vars))
        "clojure.string should have at least one private var")))

(deftest reflect-ns-full-nonexistent-returns-error
  (let [result (r/reflect-ns-full 'zzz.nonexistent.namespace)]
    (is (:error result))))

;; =============================================================================
;; reflect-var
;; =============================================================================

(deftest reflect-var-returns-envelope
  (let [env (r/reflect-var 'clojure.core 'map)]
    (is (not (:error env)) (str "Should not error: " (:error env)))
    (is (= 'clojure.core (:reflection/ns env)))
    (is (= 'map (:reflection/symbol env)))
    (is (:reflection/file env) "map should have a file")
    (is (pos? (:reflection/line env)) "map should have a line number")
    (is (:reflection/arglists env) "map should have arglists")
    (is (:reflection/doc env) "map should have doc")
    (is (inst? (:reflection/resolved-at env)) "Should have resolved-at timestamp")))

(deftest reflect-var-envelope-validates
  (let [env (r/reflect-var 'clojure.core 'map)]
    (is (envelope/valid? env)
        "Envelope should pass Malli validation")))

(deftest reflect-var-detects-macros
  (let [env (r/reflect-var 'clojure.core 'when)]
    (is (not (:error env)))
    (is (true? (:reflection/macro? env))
        "when is a macro")))

(deftest reflect-var-detects-dynamic
  ;; clojure.core/*warn-on-reflection* is a well-known dynamic var
  ;; that reliably has :dynamic true in its metadata
  (let [env (r/reflect-var 'clojure.core '*warn-on-reflection*)]
    (is (not (:error env)))
    (is (true? (:reflection/dynamic? env))
        "*warn-on-reflection* is dynamic")))

(deftest reflect-var-nonexistent-var-returns-error
  (let [result (r/reflect-var 'clojure.core 'zzz-nonexistent-var-zzz)]
    (is (:error result))
    (is (re-find #"not found" (:error result)))))

(deftest reflect-var-nonexistent-ns-returns-error
  (let [result (r/reflect-var 'zzz.nonexistent.namespace 'foo)]
    (is (:error result))
    (is (re-find #"not found" (:error result)))))

;; =============================================================================
;; reflect-deps
;; =============================================================================

(deftest reflect-deps-returns-dependency-info
  (let [deps (r/reflect-deps 'futon3c.reflection.core)]
    (is (not (:error deps)))
    (is (vector? (:requires deps)))
    (is (vector? (:imports deps)))
    (is (vector? (:required-by deps)))
    ;; core.clj requires clojure.reflect
    (is (some #(= 'clojure.reflect %) (:requires deps))
        "reflection.core should require clojure.reflect")
    ;; core.clj imports Instant
    (is (some #(= "java.time.Instant" %) (:imports deps))
        "reflection.core should import java.time.Instant")))

(deftest reflect-deps-required-by-includes-this-test
  (let [deps (r/reflect-deps 'futon3c.reflection.core)]
    (is (some #(= 'futon3c.reflection.core-test %) (:required-by deps))
        "This test namespace requires reflection.core")))

(deftest reflect-deps-nonexistent-ns-returns-error
  (let [result (r/reflect-deps 'zzz.nonexistent.namespace)]
    (is (:error result))))

;; =============================================================================
;; reflect-java-class
;; =============================================================================

(deftest reflect-java-class-returns-class-info
  (let [info (r/reflect-java-class "java.lang.String")]
    (is (not (:error info)))
    (is (= "java.lang.String" (:name info)))
    (is (vector? (:bases info)))
    (is (set? (:flags info)))
    (is (vector? (:members info)))
    (is (pos? (count (:members info))))))

(deftest reflect-java-class-members-have-structure
  (let [info (r/reflect-java-class "java.lang.String")
        length-members (->> (:members info)
                            (filter #(= "length" (:name %))))]
    (is (pos? (count length-members))
        "String should have a length method")
    (let [length (first length-members)]
      (is (:flags length))
      (is (:return-type length)))))

(deftest reflect-java-class-nonexistent-returns-error
  (let [result (r/reflect-java-class "com.nonexistent.FakeClass")]
    (is (:error result))
    (is (re-find #"not found" (:error result)))))

(deftest reflect-java-class-collection-types
  (let [info (r/reflect-java-class "java.util.HashMap")]
    (is (not (:error info)))
    (is (= "java.util.HashMap" (:name info)))
    (is (pos? (count (:members info))))))

;; =============================================================================
;; Self-reflection: reflection.core reflects on itself
;; =============================================================================

(deftest self-reflection-lists-all-public-functions
  (let [vars (r/reflect-ns 'futon3c.reflection.core)
        names (set (map :name vars))]
    (is (contains? names 'list-namespaces))
    (is (contains? names 'reflect-ns))
    (is (contains? names 'reflect-ns-full))
    (is (contains? names 'reflect-var))
    (is (contains? names 'reflect-deps))
    (is (contains? names 'reflect-java-class))
    (is (= 6 (count names))
        "Should have exactly 6 public functions")))

(deftest self-reflection-var-envelope
  (let [env (r/reflect-var 'futon3c.reflection.core 'reflect-var)]
    (is (not (:error env)))
    (is (= 'futon3c.reflection.core (:reflection/ns env)))
    (is (= 'reflect-var (:reflection/symbol env)))
    (is (= "futon3c/reflection/core.clj" (:reflection/file env)))
    (is (envelope/valid? env))))
