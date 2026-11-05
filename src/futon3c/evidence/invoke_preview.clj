(ns futon3c.evidence.invoke-preview
  "Content-minimal fields for durable invoke-start evidence."
  (:require [clojure.string :as str]))

(def ^:private header-prefix "--- CURRENT TURN ---\n")
(def ^:private preview-limit 600)

(defn header-preview
  "Return only PROMPT's leading transport header, capped at 600 characters.

   A transport header starts at byte zero with `--- CURRENT TURN ---` and ends
   before its first blank or `---` line. Header-like text later in a prompt is
   body text and returns the empty string."
  [prompt]
  (let [prompt (str (or prompt ""))]
    (if-not (.startsWith prompt header-prefix)
      ""
      (let [header (->> (str/split-lines prompt)
                        (take-while #(and (not (str/blank? %))
                                          (not= "---" %)))
                        (str/join "\n"))]
        (subs header 0 (min preview-limit (count header)))))))

(defn prompt-sha256
  "SHA-256 of PROMPT as lowercase hexadecimal."
  [prompt]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (.getBytes (str (or prompt "")) java.nio.charset.StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))
