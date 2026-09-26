(ns check-catalog
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

;; Execute only the actual pure catalog scorer from the pinned transport blob.
;; Never load the transport namespace, open sockets, or evaluate other forms.
(let [root (.getParent (io/file *file*))
      frozen (json/parse-string (slurp (io/file root "frozen.json")) true)
      expected (json/parse-string (slurp (io/file root "result.json")) true)
      pin (get-in frozen [:pins :futon3c])
      source (shell/sh "git" "-C" "/home/joe/code/futon3c" "show"
                       (str pin ":src/futon3c/transport/http.clj"))
      ids (set (map :id (:patterns frozen)))
      rows (->> (:catalog frozen)
                (filter #(contains? ids (:id %)))
                (mapv #(assoc % :pattern (:id %))))]
  (assert (zero? (:exit source)))
  (intern *ns* 'load-patterns-tsv (constantly rows))
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. (:out source)))]
    (loop []
      (let [form (read {:eof ::eof :read-cond :allow :features #{:clj}} reader)]
        (assert (not= ::eof form) "Pinned scorer not found")
        (if (and (seq? form) (= 'defn- (first form)) (= 'search-patterns (second form)))
          (eval form)
          (recur)))))
  (let [scorer (ns-resolve *ns* 'search-patterns)]
    (doseq [query (:results expected)]
      (let [actual (mapv (juxt :pattern :score) (scorer (:query query) 10))
            wanted (mapv (juxt :id :score) (get-in query [:arms :catalog_keyword_count]))]
        (assert (= actual wanted) (:id query)))))
  ;; This namespace supplies the str alias used by the extracted function.
  (assert (= "a" (str/lower-case "A")))
  (println "Pinned catalog scorer agrees with Python port for all six queries."))
