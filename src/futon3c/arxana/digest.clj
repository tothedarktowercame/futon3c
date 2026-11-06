(ns futon3c.arxana.digest
  "Canonical serialization and SHA-256 digests for the arxana.v1 spine.

  Canonical form (D5 canonical semantic response digest):
  - maps are key-sorted (by canonical key bytes, so keyword/string ordering is
    total and deterministic),
  - vectors/lists retain semantic order,
  - sets are forbidden in canonical payloads (order would be implicit) and are
    rejected rather than silently sorted,
  - everything serializes to a deterministic EDN byte string, UTF-8 encoded,
    SHA-256 digested."
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.nio.charset StandardCharsets]))

(defn sha256-hex
  ^String [^bytes bs]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.update md bs)
    (format "%064x" (java.math.BigInteger. 1 (.digest md)))))

(defn- canonical-key-order
  "Total deterministic order over map keys: namespace, then name/print bytes."
  [k]
  (str (namespace k) "" (name k)))

(defn canonicalize
  "Return the canonical EDN string for x. Throws ex-info on sets, which are
  not permitted in canonical payloads (their order is not semantic here)."
  [x]
  (let [sb (StringBuilder.)]
    (letfn [(emit [v]
              (cond
                (map? v)
                (do (.append sb "{")
                    (doseq [[k val] (sort-by (comp canonical-key-order key) v)]
                      (emit k) (.append sb " ") (emit val) (.append sb " "))
                    (when (pos? (count v))
                      (.setLength sb (dec (.length sb))))
                    (.append sb "}"))

                (set? v)
                (throw (ex-info "sets are not permitted in canonical arxana payloads"
                                {:value v}))

                (sequential? v)
                (do (.append sb "[")
                    (doseq [item v]
                      (emit item) (.append sb " "))
                    (when (pos? (count v))
                      (.setLength sb (dec (.length sb))))
                    (.append sb "]"))

                (keyword? v) (.append sb (pr-str (symbol v)))
                (symbol? v)  (.append sb (pr-str v))
                (string? v)  (.append sb (pr-str v))
                (nil? v)     (.append sb "nil")
                (or (true? v) (false? v)) (.append sb (str v))
                (number? v)  (.append sb (pr-str v))
                :else (throw (ex-info "value not canonicalizable"
                                      {:value v :class (class v)}))))]
      (emit x)
      (str sb))))

(defn canonical-digest
  "SHA-256 hex of the canonical bytes of x."
  ^String [x]
  (sha256-hex (.getBytes ^String (canonicalize x) StandardCharsets/UTF_8)))

(defn project
  "Select the projection's semantic fields from payload, in the registered
  order, dropping everything else (transport metadata, prose, diagnostics).
  Nested selections use [path...] vectors."
  [fields payload]
  (reduce (fn [acc f]
            (if (vector? f)
              (let [v (get-in payload f ::absent)]
                (if (= v ::absent) acc (assoc-in acc f v)))
              (let [v (get payload f ::absent)]
                (if (= v ::absent) acc (assoc acc f v)))))
          {}
          fields))

(defn projected-digest
  "Canonical digest of payload restricted to the registered projection fields."
  ^String [fields payload]
  (canonical-digest (project fields payload)))
