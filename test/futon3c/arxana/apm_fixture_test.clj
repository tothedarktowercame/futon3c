(ns futon3c.arxana.apm-fixture-test
  "Fixture gate for the APM a93J02 two-pane demo: committed bytes hash to
  the manifest, every dom-id exists in the committed HTML, evidence quotes
  lie inside their source ranges, and the built repository carries honest
  verification standings (subsection :verified, aligned paragraph :partial,
  unverifiable paragraphs absent)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.apm-fixture :as apm]
            [futon3c.arxana.correspond :as corr]))

(deftest committed-fixture-verifies
  (let [report (apm/verify-fixture)]
    (is (:ok? report))
    (is (= 11 (:scopes report)))
    (is (pos? (:tex-codepoints report)))))

(deftest fixture-rejects-tampered-bytes
  ;; The exact bad case the check is named for: a byte drift between the
  ;; manifest and the committed artifact must refuse, against the real
  ;; resources (re-bind the resource lookup to a temp copy).
  (let [good (apm/fixture-resource "LaTeXML.css")
        orig apm/fixture-resource
        tmp (java.io.File/createTempFile "apm-css" ".css")]
    (try
      (spit tmp (str (slurp good) "\n/* drifted */"))
      (with-redefs [apm/fixture-resource
                    (fn [name] (if (= name "LaTeXML.css") tmp (orig name)))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"APM fixture rejected"
                              (apm/verify-fixture))))
      (finally (.delete tmp)))))

(deftest repository-carries-honest-standings
  (let [repo (corr/in-memory-repository)
        {:keys [assertions revisions]} (apm/assert-into-repository! repo)
        vals (vals assertions)]
    (is (= 11 (count vals)))
    (is (= 3 (count (filter #(= :verified (:verification %)) vals)))
        "the three subsection scopes carry producer-verified standing")
    (is (= 8 (count (filter #(= :partial (:verification %)) vals)))
        "the eight text-aligned paragraph scopes stay :partial")
    (is (every? #(= :source-to-target (:direction %)) vals))
    (is (every? #(= :codepoint-range (get-in % [:source :locator :kind])) vals))
    (is (every? #(= :dom-id (get-in % [:target :locator :kind])) vals))
    (is (str/starts-with? (get revisions "apm-a93J02.tex") "arxana:revision:sha256:"))
    ;; DOM ids are locator evidence only: every target id must exist in the
    ;; committed html bytes, and no assertion mints a client identity.
    (is (every? #(re-matches #"arxana:correspondence:sha256:[0-9a-f]{64}" (:id %)) vals))
    (is (every? #(= "latexml_oxide" (get-in % [:producer :tool])) vals))))
