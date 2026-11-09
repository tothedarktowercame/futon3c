(ns futon3c.arxana.json
  "JSON wire codec for the arxana.v1 HTTP surface.

  The D3 envelope is an EDN value with qualified keywords; JSON has neither
  namespaces nor keywords, so v1 uses an explicit tagging rule, applied
  uniformly and tested round-trip:

  - map keys: qualified keywords serialize as \"ns/name\" and parse back as
    keywords (JSON object keys are always strings);
  - keyword values serialize as \":ns/name\" (leading colon) and parse back
    as keywords; plain strings never begin with \":\" in arxana.v1 payloads
    (the codec refuses to encode one rather than silently mistagging);
  - byte arrays serialize as {\"$base64\" \"...\"} and parse back as bytes;
  - everything else rides JSON's native types.

  Unknown-field refusal stays in the envelope/schema layer, not the codec."
  (:require [cheshire.core :as cheshire]
            [clojure.string :as str])
  (:import [java.util Base64]))

(defn- encode-key [k]
  (cond
    (keyword? k) (if (namespace k)
                   (str (namespace k) "/" (name k))
                   (name k))
    (string? k) k
    :else (throw (ex-info "JSON map keys must be keywords or strings" {:key k}))))

(defn encode-value
  "Walk x into JSON-native data with tagged keywords/bytes."
  [x]
  (cond
    (keyword? x) (str ":" (when (namespace x) (str (namespace x) "/")) (name x))
    (bytes? x) {"$base64" (.encodeToString (Base64/getEncoder) x)}
    (map? x) (into {} (map (fn [[k v]] [(encode-key k) (encode-value v)])) x)
    (sequential? x) (mapv encode-value x)
    (set? x) (throw (ex-info "sets are not permitted in arxana JSON payloads"
                             {:value x}))
    (string? x) (if (str/starts-with? x ":") {"$string" x} x)
    (symbol? x) (str x)
    :else x))

(defn encode
  "Serialize an arxana value to a JSON string."
  ^String [x]
  (cheshire/generate-string (encode-value x)))

(def ^:private keyword-tag-re
  #"^:([^\s:/]+/)?[^\s:/]+$")

(defn- decode-key [^String k]
  (if (re-matches #"[^\s]+" k) (keyword k) k))

(defn decode-value
  "Walk parsed JSON data back into arxana values."
  [x]
  (cond
    (map? x) (cond
               (and (= 1 (count x)) (contains? x "$base64"))
               (.decode (Base64/getDecoder) ^String (get x "$base64"))
               (and (= 1 (count x)) (contains? x "$string")) (get x "$string")
               :else (into {} (map (fn [[k v]] [(decode-key k) (decode-value v)])) x))
    (sequential? x) (mapv decode-value x)
    (string? x) (if (re-matches keyword-tag-re x)
                  (keyword (subs x 1))
                  x)
    :else x))

(defn decode
  "Parse a JSON string into an arxana value. Malformed JSON throws ex-info
  :arxana.json/malformed (the HTTP layer maps this to a typed :invalid)."
  [^String s]
  (try
    (decode-value (cheshire/parse-string s))
    (catch com.fasterxml.jackson.core.JsonProcessingException e
      (throw (ex-info "malformed JSON request body"
                      {:type :arxana.json/malformed :message (.getMessage e)})))))
