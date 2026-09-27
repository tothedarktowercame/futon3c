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

(def parameter-carry {:field field :from {:param 'receipt :via :inbound}})
(def parameter-boxes
  [{:box/id :caller :site {:file "source.clj" :var "caller"}
    :passes [{:id :inbound :value field :from {:literal-arg-key :belief}
              :to {:call "wrapper" :arg 1 :callee-box :wrapper}}]}
   {:box/id :wrapper :site {:file "source.clj" :var "wrapper"}
    :returns-record :new :reads [field] :carries [parameter-carry]}])

(defn- parameter-check [body edit]
  (source (str "(defn caller [] (wrapper {:belief 1} true)) "
               "(defn wrapper [receipt condition] " body ")")
          #(w/carry-attribution % (edit parameter-boxes) :wrapper parameter-carry)))

(deftest parameter-carry-composes-inbound-and-checks-all-arms
  (doseq [body ["receipt" "(assoc receipt :other 2)"
               "(if condition receipt (assoc receipt :belief 2))"
               "(merge receipt {:other 2})" "(dissoc receipt :other)"
               "(select-keys receipt [:belief])"]]
    (is (:ok? (parameter-check body identity)) body))
  (is (= #{:carried :overridden}
         (:arms (parameter-check "(if condition receipt (assoc receipt :belief 2))" identity)))))

(deftest invalid-parameter-carries-are-refused
  (doseq [body ["(let [receipt {}] receipt)"
               "(let [receipt (assoc receipt :other 2)] receipt)"
               "((fn [receipt] receipt) {})"
               "(dissoc receipt :belief)" "(select-keys receipt [:other])"
               "(if condition receipt {})" "(merge receipt condition)"
               "(transform receipt)" "(assoc receipt condition 2)"
               "(let [assoc vector] (assoc receipt :belief 2))"]]
    (is (false? (:ok? (parameter-check body identity))) body))
  (doseq [edit [#(assoc-in % [0 :passes] [])
               #(assoc-in % [0 :passes 0 :from] {:returns-of "missing"})
               #(assoc-in % [0 :passes 0 :to :arg] 2)
               #(assoc-in % [0 :passes 0 :to :call] "missing")]]
    (is (false? (:ok? (parameter-check "receipt" edit))))))

(deftest real-temporal-consumer-carries-and-overrides-its-parameter
  (let [resource (io/resource "futon2/aif/token_belief_predecessor.clj")
        file "../futon2/src/futon2/aif/token_belief_predecessor.clj"
        f [:continuation-belief {:record :initialization}]
        c {:field f :from {:param 'receipt :via :input}}
        pass {:value f :from {:returns-of "apply"}
              :to {:call "consume-temporal" :arg 1 :callee-box :consumer}}
        bs [{:box/id :input :site {:file file :var "input-receipt"} :passes [pass]}
            {:box/id :consumer :site {:file file :var "consume-temporal"}
             :returns-record :temporal :reads [f] :carries [c]}]
        r (w/carry-attribution "." bs :consumer c)]
    (is (= (.getCanonicalPath (io/file resource)) (.getCanonicalPath (io/file file))))
    ;; This inbound proves the actual apply result, not apply's function target.
    (is (:ok? (w/pass-attribution "." bs :input pass)))
    (is (:ok? r) (pr-str r))
    (is (= #{:carried :overridden} (:arms r)))
    (is (false? (:ok? (w/carry-attribution "." (assoc-in bs [0 :passes] []) :consumer c))))))
