(ns futon3c.diagramprover.wm-record-carry-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wiring :as w]))

(def field [:belief {:record :legacy}])
(def carry {:field field :from {:returns-of "old"}})
(def boxes
  [{:box/id :old :site {:file "source.clj" :var "old"}
    :returns-record :legacy :writes [field]}
   {:box/id :wrapper :site {:file "source.clj" :var "wrapper"}
    :returns-record :new :reads [field]
    :writes [[:belief {:record :new}]] :carries [carry]}])

(defn- source [text f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                      "record-carry" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file root "source.clj")]
    (try (spit file text) (f (str root))
         (finally (.delete file) (.delete root)))))

(defn- check [body]
  (source (str "(defn old [] {:belief 1}) (defn wrapper [condition] " body ")")
          #(w/carry-attribution % boxes :wrapper carry)))

(deftest whole-record-and-literal-override-arms
  (doseq [[body arms]
          [["(let [legacy (old)] legacy)" #{:carried}]
           ["(let [legacy (old)] (assoc legacy :other 2))" #{:carried}]
           ["(let [legacy (old)] (if condition legacy (assoc legacy :belief 2)))"
            #{:carried :overridden}]
           ["(let [legacy (old)] (merge legacy {:other 1}))" #{:carried}]
           ["(let [legacy (old)] (merge legacy {:belief 2}))" #{:overridden}]
           ["(let [legacy (old)] (dissoc (assoc legacy :other 2) :other))" #{:carried}]
           ["(let [legacy (old)] (select-keys legacy [:belief]))" #{:carried}]]]
    (let [r (check body)] (is (:ok? r) (pr-str r)) (is (= arms (:arms r))))))

(deftest unproved-or-removed-records-are-refused
  (doseq [body ["(let [legacy (old) legacy {:belief 2}] legacy)"
               "(let [legacy (old)] (let [legacy {:belief 2}] legacy))"
               "(let [legacy (old)] (dissoc legacy :belief))"
               "(let [legacy (old)] (select-keys legacy [:other]))"
               "(let [legacy (old)] (if condition legacy {:other 2}))"
               "(let [legacy (old)] (if condition legacy))"
               "(let [legacy (old)] (merge legacy condition))"
               "(let [legacy (old)] (assoc legacy condition 2))"
               "(let [old condition legacy (old)] legacy)"
               "(let [assoc condition legacy (old)] (assoc legacy :belief 2))"
               "(old)"]]
    (is (false? (:ok? (check body))) body)))

(deftest carry-is-a-read-and-never-removes-an-independent-writer
  (source "(defn old [] {:belief 1})
           (defn wrapper [condition]
             (let [legacy (old)] (if condition legacy (assoc legacy :belief 2))))"
    (fn [root]
      (is (empty? (w/conformance root {:boxes boxes} {:heuristic? true})))
      (is (empty? (w/multiply-written (w/ingest {:boxes boxes}))))
      (let [bad (assoc-in boxes [1 :writes] [field])]
        (is (= :multiply-written (:finding (first (w/multiply-written (w/ingest {:boxes bad}))))))))))

(deftest real-initialization-receipt-carries-or-overrides
  (let [r (sh/sh "git" "-C" "/home/joe/code/futon2" "show"
                 "8b595a028330e18be92e59445cc73f65dd7c9f47:src/futon2/aif/token_belief_predecessor.clj")
        f [:continuation-belief {:record :legacy}]
        c {:field f :from {:returns-of "legacy-input-receipt"}}
        bs [{:box/id :old :site {:file "source.clj" :var "legacy-input-receipt"}
             :returns-record :legacy :writes [f]}
            {:box/id :wrapper :site {:file "source.clj" :var "initialization-input-receipt"}
             :returns-record :token-belief-input :reads [f] :carries [c]}]]
    (is (zero? (:exit r)) (:err r))
    (source (:out r)
      (fn [root]
        (let [result (w/carry-attribution root bs :wrapper c)]
          (is (:ok? result) (pr-str result))
          (is (= #{:carried :overridden} (:arms result))))))))


(deftest a-shadowed-callee-parameter-does-not-name-the-source
  (source "(defn old [] {:belief 1})
           (defn wrapper [old] (let [legacy (old)] legacy))"
    (fn [root]
      (is (false? (:ok? (w/carry-attribution root boxes :wrapper carry))))
      (is (some #(= :carry-not-proved (:finding %))
                (w/conformance root {:boxes boxes} {:heuristic? true}))))))

(deftest declared-source-must-really-write-the-field
  (source "(defn old [] {:other 1})
           (defn wrapper [] (let [legacy (old)] legacy))"
    (fn [root]
      (is (= :carry-source-write-not-proved
             (:why (w/carry-attribution root boxes :wrapper carry)))))))


(deftest always-overridden-field-is-not-an-upstream-read
  (source "(defn old [] {:belief 1})
           (defn wrapper [] (let [legacy (old)] (assoc legacy :belief 2)))"
    (fn [root]
      (is (= #{:overridden} (:arms (w/carry-attribution root boxes :wrapper carry))))
      (is (some #(and (= :wrapper (:box/id %)) (= :reads (:role %))
                      (= :declared-read-not-found (:finding %)))
                (w/conformance root {:boxes boxes} {:heuristic? true}))))))
