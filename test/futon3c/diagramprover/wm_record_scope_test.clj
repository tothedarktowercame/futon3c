(ns futon3c.diagramprover.wm-record-scope-test
  "A wiring field may be scoped to a record: a :reads/:writes entry is a
  keyword (as ever) or [field {:record r}], whose vertex is [field r]. The
  same key on two records is two wires; the one-writer rule and the declared
  read/write lookups hold per vertex. An occurrence of a scoped field's key at
  a site counts for the scope only when the site names the record path
  ((get-in st [:sources :wants t]) or receiver `sources`); every other
  occurrence is the unscoped field's, so an unscoped field's occurrences stay
  where they are. Unscoped maps are unchanged: the map test's report and the
  projection fixture are compared byte for byte in their own namespaces."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.ct-projection :as ct]
            [futon3c.diagramprover.wiring :as wiring]))

(defn- box [id reads writes & [site]]
  (cond-> {:box/id id :box/kind :component :reads reads :writes writes}
    site (assoc :site site)))

(deftest vertex-keys
  (is (= :wants (wiring/vertex-key :wants)) "a keyword is its own vertex")
  (is (= [:wants :sources] (wiring/vertex-key [:wants {:record :sources}])))
  (is (= [:x {}] (wiring/vertex-key [:x {}])) "a vector with no :record is left as it was, not refused")
  (is (= :wants (wiring/vertex-field [:wants :sources])))
  (is (= :sources (wiring/vertex-record [:wants :sources])))
  (is (nil? (wiring/vertex-record :wants))))

(deftest same-key-on-two-records-each-with-one-writer-is-accepted
  (let [g (wiring/ingest
           {:spec/id :two-records
            :boxes [(box :click [] [[:wants {:record :click}]])
                    (box :assemble [] [[:wants {:record :sources}]])
                    (box :use-click [[:wants {:record :click}]] [])
                    (box :use-sources [[:wants {:record :sources}]] [])]})]
    (is (= [] (wiring/multiply-written g)))
    (is (= [] (wiring/written-never-read g)))
    (is (= [] (wiring/read-never-written g)))))

(deftest the-same-key-twice-on-one-record-is-refused
  (let [g (wiring/ingest
           {:spec/id :one-record
            :boxes [(box :a [] [[:wants {:record :sources}]])
                    (box :b [] [[:wants {:record :sources}]])]})
        f (wiring/multiply-written g)]
    (is (= 1 (count f)))
    (is (= [:wants :sources] (:field (first f))) "the finding names the scoped vertex")
    (is (= [:a :b] (:writers (first f))))))

(deftest a-scoped-and-an-unscoped-writer-of-one-key-are-two-wires
  (let [g (wiring/ingest
           {:spec/id :mixed
            :boxes [(box :click [] [:wants])
                    (box :assemble [] [[:wants {:record :sources}]])]})]
    (is (= [] (wiring/multiply-written g)))
    (testing "unscoped stays refused when two boxes write it, as before"
      (is (= [:wants]
             (map :field (wiring/multiply-written
                          (wiring/ingest {:spec/id :unscoped
                                          :boxes [(box :a [] [:wants]) (box :b [] [:wants])]}))))))))

(defn- with-root
  "Run F on a temp root holding FILES {name text}."
  [files f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-record-scope" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[n t] files] (spit (io/file root n) t))
    (f (str root))))

(def click-src "(defn click-wants [m v] (assoc m :wants v))")
(def assemble-src
  (str "(defn assemble [st t w target sources]\n"
       "  (let [st (assoc-in st [:sources :wants t] w)]\n"
       "    (get-in sources [:wants target])))"))
(def outside-src "(defn elsewhere [m] (assoc m :wants 1))")

(defn- spec-of [click-site assemble-site]
  {:spec/id :occ
   :boxes [(box :click [] [:wants] {:file click-site})
           (box :assemble [] [[:wants {:record :sources}]] {:file assemble-site})]})

(deftest occurrences-under-the-record-path-are-attributed-to-the-scope
  (with-root {"click.clj" click-src "assemble.clj" assemble-src}
    (fn [root]
      (let [spec (spec-of "click.clj" "assemble.clj")
            u (wiring/usage root spec)
            by (fn [id role] (:usage (first (filter #(and (= id (:box/id %)) (= role (:role %))) u))))]
        (is (= {:reads 1 :writes 1 :unclassified 0} (by :assemble :writes))
            "both forms name the record: the assoc-in path [:sources :wants t] (a write) and the receiver `sources` in (get-in sources [:wants target]) (a read)")
        (is (= {:reads 0 :writes 1 :unclassified 0} (by :click :writes)))
        (is (= [] (wiring/conformance root spec {:heuristic? true}))
            "every declared field is found at its site in its declared role")))))

(deftest an-occurrence-outside-the-record-is-not-attributed
  (testing "a scoped write declared where only an unscoped :wants occurs is not found"
    (with-root {"only-unscoped.clj" outside-src "click.clj" click-src}
      (fn [root]
        (let [spec {:spec/id :out
                    :boxes [(box :click [] [:wants] {:file "click.clj"})
                            (box :assemble [] [[:wants {:record :sources}]] {:file "only-unscoped.clj"})]}
              f (wiring/conformance root spec {:heuristic? true})]
          (is (some #(and (= :declaration-without-occurrence (:finding %))
                          (= [:wants :sources] (:field %)) (= :assemble (:box/id %))) f)
              (pr-str f))))))
  (testing "a site that touches both, declaring only the scope, still owes the unscoped field a declaration"
    (with-root {"both.clj" (str assemble-src "\n" outside-src) "click.clj" click-src}
      (fn [root]
        (let [spec {:spec/id :both
                    :boxes [(box :click [] [:wants] {:file "click.clj"})
                            (box :assemble [] [[:wants {:record :sources}]] {:file "both.clj"})]}
              f (wiring/conformance root spec {:heuristic? true})]
          (is (some #(and (= :occurrence-without-declaration (:finding %))
                          (= :wants (:field %))) f)
              (pr-str f))
          (is (not-any? #(= [:wants :sources] (:field %)) f)
              "the scope itself is found, in its role"))))))

(deftest a-map-with-no-scoped-entry-is-read-as-before
  (with-root {"click.clj" (str click-src "\n" outside-src)}
    (fn [root]
      (let [spec {:spec/id :plain :boxes [(box :click [] [:wants] {:file "click.clj"})]}]
        (is (= [] (wiring/conformance root spec {:heuristic? true})))
        (is (= {:reads 0 :writes 2 :unclassified 0}
               (:usage (first (wiring/usage root spec)))))))))

(deftest the-projection-names-a-scoped-port-field-at-record
  (let [m {:spec/id :proj
           :boxes [(box :assemble [] [[:wants {:record :sources}]])
                   (box :reader [[:wants {:record :click}]] [])]}
        p (ct/project m)
        ids (set (map :id (concat (get-in p [:ports :input]) (get-in p [:ports :output]))))]
    (is (contains? ids (keyword "in" "wants@click")) "read by the map, written by no box: an input port wants@click")
    (is (contains? ids (keyword "out" "wants@sources")) "written, read by no box: an output port wants@sources")
    (is (= #{"wants@click" "wants@sources"}
           (set (keep #(when (:field %) (:name %)) (concat (get-in p [:ports :input]) (get-in p [:ports :output]))))))))
