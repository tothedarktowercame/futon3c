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

;; ---------------------------------------------------------------------------
;; WM-PROVER-RECEIVER-FORMS-I: a keyword call on a receiver, and a returned map literal.

(defn- usage-of
  "The :usage of BOX's declared entry in the (single-box) spec over FILES."
  [files b role]
  (with-root files
    (fn [root]
      (:usage (first (filter #(= role (:role %))
                             (wiring/usage root {:spec/id :t :boxes [b]})))))))

(defn- reader [file & [extra]]
  (merge {:box/id :reader :box/kind :component :reads [[:target {:record :flight}]] :writes []
          :site {:file file}}
         extra))

(deftest a-keyword-call-on-the-record-is-attributed
  (testing "(:target flight): the receiver is the record's name"
    (is (= {:reads 1 :writes 0 :unclassified 0}
           (usage-of {"a.clj" "(defn read-fn [flight] (:target flight))"} (reader "a.clj") :reads))))
  (testing "(:target f): attributed only when the site declares f an alias of :flight"
    (let [src "(defn run! [f] (:target f))"]
      (is (= {:reads 0 :writes 0 :unclassified 0} (usage-of {"a.clj" src} (reader "a.clj") :reads))
          "no alias: only the record's own name counts")
      (is (= {:reads 1 :writes 0 :unclassified 0}
             (usage-of {"a.clj" src} (reader "a.clj" {:record-aliases {:flight ["f" "fl"]}}) :reads))
          "with the alias it counts")))
  (testing "(:target issued): a different receiver is another record's key, never attributed"
    (is (= {:reads 0 :writes 0 :unclassified 0}
           (usage-of {"a.clj" "(defn g [issued] (:target issued))"}
                     (reader "a.clj" {:record-aliases {:flight ["f" "fl"]}}) :reads))))
  (testing "an alias declared on another box at the same site applies to the site's text"
    (with-root {"a.clj" "(defn run! [fl] (:target fl))"}
      (fn [root]
        (let [spec {:spec/id :t
                    :boxes [(reader "a.clj")
                            {:box/id :other :box/kind :component :reads [] :writes []
                             :site {:file "a.clj"} :record-aliases {:flight ["fl"]}}]}
              u (first (filter #(= :reader (:box/id %)) (wiring/usage root spec)))]
          (is (= 1 (:reads (:usage u)))))))))

(defn- writer [file & [extra]]
  (merge {:box/id :resolve :box/kind :component :reads [] :writes [[:target {:record :flight}]]
          :site {:file file}}
         extra))

(def resolve-src
  (str "(defn resolve-target [chosen mission]\n"
       "  (if chosen\n"
       "    {:target chosen :why :given}\n"
       "    {:target (default-for mission) :why :default}))"))

(deftest a-returned-map-literal-is-a-write-of-the-declared-record
  (testing "both branches of a trailing if are writes of the record"
    (is (= {:reads 0 :writes 2 :unclassified 0}
           (usage-of {"a.clj" resolve-src} (writer "a.clj" {:returns-record :flight}) :writes))))
  (testing "without :returns-record nothing is attributed"
    (is (= {:reads 0 :writes 0 :unclassified 0}
           (usage-of {"a.clj" resolve-src} (writer "a.clj") :writes))))
  (testing "a map literal bound in a let and not returned is not attributed"
    (is (= {:reads 0 :writes 0 :unclassified 0}
           (usage-of {"a.clj" "(defn f [x] (let [m {:target x}] (count m)))"}
                     (writer "a.clj" {:returns-record :flight}) :writes))))
  (testing "a trailing let returning a map, and cond and case results, are in return position"
    (is (= 1 (:writes (usage-of {"a.clj" "(defn f [x] (let [y (inc x)] {:target y}))"}
                                (writer "a.clj" {:returns-record :flight}) :writes))))
    (is (= 2 (:writes (usage-of {"a.clj" "(defn f [x] (cond (pos? x) {:target x} :else {:target 0}))"}
                                (writer "a.clj" {:returns-record :flight}) :writes))))
    (is (= 3 (:writes (usage-of {"a.clj" "(defn f [x] (case x 1 {:target 1} 2 {:target 2} {:target 0}))"}
                                (writer "a.clj" {:returns-record :flight}) :writes)))))
  (testing "a map literal that is an argument, or in a non-final body form, is not returned"
    (is (= 0 (:writes (usage-of {"a.clj" "(defn f [x] (log! {:target x}) (count x))"}
                                (writer "a.clj" {:returns-record :flight}) :writes))))
    (is (= 0 (:writes (usage-of {"a.clj" "(defn f [x] (assoc x :k {:target 1}))"}
                                (writer "a.clj" {:returns-record :flight}) :writes)))))
  (testing "a multi-arity defn: each arity's last form"
    (is (= 2 (:writes (usage-of {"a.clj" "(defn f ([] {:target 0}) ([x] {:target x}))"}
                                (writer "a.clj" {:returns-record :flight}) :writes))))))

(deftest the-unscoped-field-keeps-what-the-two-forms-do-not-take
  (testing "the same key elsewhere at the site stays the unscoped field's"
    (with-root {"a.clj" (str resolve-src "\n(defn g [issued] (:target issued))")}
      (fn [root]
        (let [spec {:spec/id :t
                    :boxes [(writer "a.clj" {:returns-record :flight})
                            {:box/id :other :box/kind :component :reads [:target] :writes []
                             :site {:file "a.clj"}}]}
              u (fn [id role] (:usage (first (filter #(and (= id (:box/id %)) (= role (:role %)))
                                                    (wiring/usage root spec)))))]
          (is (= 2 (:writes (u :resolve :writes))) "the scope's two returned-map writes")
          (is (= {:reads 1 :writes 0 :unclassified 0} (u :other :reads))
              "the unscoped :target keeps (:target issued) and loses the two attributed map keys"))))))

;; ---------------------------------------------------------------------------
;; WM-PROVER-NESTED-LITERAL-I: a let-bound literal returned by name, and a
;; nested literal under a record key.

(defn- writes-by-vertex
  "{vertex :writes-count} for the :writes entries of the boxes over FILES."
  [files boxes]
  (with-root files
    (fn [root]
      (into {} (for [u (wiring/usage root {:spec/id :t :boxes boxes}) :when (= :writes (:role u))]
                 [[(:box/id u) (:field u)] (:writes (:usage u))])))))

(defn- wbox [id writes & [extra]]
  (merge {:box/id id :box/kind :component :reads [] :writes writes :site {:file "a.clj"}} extra))

(deftest a-let-bound-literal-returned-by-name-is-in-return-position
  (let [w (fn [src] (get (writes-by-vertex {"a.clj" src}
                                           [(wbox :w [[:target {:record :flight}]] {:returns-record :flight})])
                         [:w [:target :flight]]))]
    (testing "the name itself, and each of assoc / merge / update / -> / cond-> threaded from it"
      (is (= 1 (w "(defn f [x] (let [m {:target x}] m))")))
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (assoc m :k 1)))")))
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (merge m {:z 1})))")))
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (update m :k inc)))")))
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (-> m (assoc :k 1) (dissoc :z))))")))
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (cond-> m x (assoc :k 1))))"))))
    (testing "through nested lets and a cond branch"
      (is (= 1 (w "(defn f [x] (let [m {:target x}] (let [n (count x)] (if n m nil))))"))))
    (testing "a let-bound literal whose name is not returned is not attributed"
      (is (= 0 (w "(defn f [x] (let [m {:target x}] (count m)))")))
      (is (= 0 (w "(defn f [x] (let [m {:target x} n {:k 1}] n))")) "another name is returned")
      (is (= 0 (w "(defn f [x] (let [m {:target x} m (inc 1)] m))")) "the name is rebound to a non-literal"))))

(def assemble-one-src
  (str "(defn assemble-one [sources target]\n"
       "  (let [want (get-in sources [:wants target])\n"
       "        base-problem {:facts 1\n"
       "                      :want (vec want)\n"
       "                      :cascade-spec {:want (set want) :lam 2}\n"
       "                      :other {:want 9}}]\n"
       "    (cond (nil? want) {:refused true}\n"
       "          :else {:target target\n"
       "                 :cascade-problem (assoc base-problem :precedences [])})))"))

(deftest the-assemble-one-shape-attributes-the-nested-want-to-cascade-spec
  (let [boxes [(wbox :w [[:want {:record :cascade-spec}] [:want {:record :cascade-problem}]
                         [:want {:record :result}]]
                     {:returns-record :result})]
        u (writes-by-vertex {"a.clj" assemble-one-src} boxes)]
    (is (= 1 (get u [:w [:want :cascade-spec]])) "the :want inside :cascade-spec {...}")
    (is (= 1 (get u [:w [:want :cascade-problem]]))
        "base-problem's own :want, reached as the value of :cascade-problem (assoc base-problem ...)")
    (is (= 0 (get u [:w [:want :result]])) "the returned literal has no :want key of its own")))

(deftest a-nested-literal-goes-to-its-key-record-and-only-that-one
  (let [both [(wbox :w [[:want {:record :cascade-spec}] [:want {:record :flight}]]
                    {:returns-record :flight})]]
    (testing "the nested :want is the inner record's, not the outer one's"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] {:outer 1 :cascade-spec {:want x}})"} both)]
        (is (= 1 (get u [:w [:want :cascade-spec]])))
        (is (= 0 (get u [:w [:want :flight]])))))
    (testing "the outer literal's own :want is the outer record's alone"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] {:want x :cascade-spec {:want x}})"} both)]
        (is (= 1 (get u [:w [:want :cascade-spec]])))
        (is (= 1 (get u [:w [:want :flight]])))))
    (testing "a nested literal under a key no box scopes is not attributed"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] {:other {:want x}})"}
                                [(wbox :w [[:want {:record :flight}]] {:returns-record :flight})])]
        (is (= 0 (get u [:w [:want :flight]])))))
    (testing "a nested literal outside any attributed literal is not attributed"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] (log! {:cascade-spec {:want x}}) (count x))"} both)]
        (is (= 0 (get u [:w [:want :cascade-spec]])))))
    (testing "without :returns-record nothing is attributed"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] {:cascade-spec {:want x}})"}
                                [(wbox :w [[:want {:record :cascade-spec}]])])]
        (is (= 0 (get u [:w [:want :cascade-spec]])))))
    (testing "a literal that could reach itself terminates"
      (let [u (writes-by-vertex {"a.clj" "(defn f [x] (let [m {:cascade-spec {:want x}}] (let [m {:cascade-spec m}] m)))"}
                                [(wbox :w [[:want {:record :cascade-spec}]] {:returns-record :flight})])]
        (is (number? (get u [:w [:want :cascade-spec]])))))))

;; ---------------------------------------------------------------------------
;; WM-PROVER-THREADED-RETURN-I: the threaded first argument of -> / cond-> is a
;; return position.

(def resolve-target-src
  (str "(defn resolve-target [given field-entry]\n"
       "  (let [given? (some? given)]\n"
       "    (cond-> (cond given? {:target given :why :given}\n"
       "                  :else {:target (default-for given) :why :default})\n"
       "      field-entry (assoc :field field-entry))))"))

(deftest the-threaded-first-argument-is-a-return-position
  (let [w (fn [src]
            (writes-by-vertex {"a.clj" src}
                              [(wbox :w [[:target {:record :flight}] [:field {:record :flight}]]
                                     {:returns-record :flight})]))]
    (testing "the resolve-target shape: both branches of the cond under cond-> are writes"
      (let [u (w resolve-target-src)]
        (is (= 2 (get u [:w [:target :flight]])))
        (is (= 0 (get u [:w [:field :flight]])) "the key a threaded step adds stays unattributed")))
    (testing "-> likewise, over cond, if, case, a literal and a let-bound name"
      (is (= 2 (get (w "(defn f [x] (-> (if x {:target 1} {:target 2}) (assoc :k 1)))") [:w [:target :flight]])))
      (is (= 3 (get (w "(defn f [x] (-> (case x 1 {:target 1} 2 {:target 2} {:target 0}) (assoc :k 1)))") [:w [:target :flight]])))
      (is (= 1 (get (w "(defn f [x] (-> {:target x} (assoc :k 1)))") [:w [:target :flight]])))
      (is (= 1 (get (w "(defn f [x] (let [m {:target x}] (-> m (assoc :k 1))))") [:w [:target :flight]])))
      (is (= 2 (get (w "(defn f [x] (-> (cond x {:target 1} :else {:target 2}) (dissoc :z) (assoc :k 1)))") [:w [:target :flight]]))
          "more than one threaded step"))
    (testing "the threaded form must itself be in return position"
      (is (= 0 (get (w "(defn f [x] (log! (cond-> (if x {:target 1} {:target 2}) x (assoc :k 1))) (count x))") [:w [:target :flight]]))))
    (testing "->> and cond->> thread last, so their first argument is not the value: not covered"
      (is (= 0 (get (w "(defn f [x] (->> (if x {:target 1} {:target 2}) (merge {:k 1})))") [:w [:target :flight]])))
      (is (= 0 (get (w "(defn f [x] (cond->> (if x {:target 1} {:target 2}) x (merge {:k 1})))") [:w [:target :flight]]))))))

;; ---------------------------------------------------------------------------
;; WM-PROVER-MERGE-LITERAL-I: a literal argument of a returned merge/into/conj, and the
;; key-value pairs of a returned assoc.

(def cpe-src
  (str "(defn compute-prediction-error [observed prediction opts]\n"
       "  (let [stamp (cond-> {:producer-contract :v1} (:channel opts) (assoc :channel 1))\n"
       "        offending (remove nil? [observed prediction])]\n"
       "    (cond\n"
       "      (seq offending)\n"
       "      (merge stamp {:status :refused :reason :malformed :offending offending})\n"
       "      (nil? observed)\n"
       "      (merge stamp {:status :absent :absent-member :observed}\n"
       "             (or (not-empty (select-keys opts [:reason])) {:reason :observation-absent}))\n"
       "      :else\n"
       "      (let [err (- observed prediction)]\n"
       "        (merge stamp {:status :present :observed observed :error err\n"
       "                      :weighted-error (* err 2)})))))"))

(defn- merge-box [writes]
  (wbox :w writes {:returns-record :prediction-error}))

(deftest a-literal-argument-of-a-returned-merge-is-returned
  (let [u (writes-by-vertex {"a.clj" cpe-src}
                            [(merge-box [[:error {:record :prediction-error}]
                                         [:weighted-error {:record :prediction-error}]
                                         [:status {:record :prediction-error}]
                                         [:reason {:record :prediction-error}]
                                         [:producer-contract {:record :prediction-error}]
                                         [:channel {:record :prediction-error}]])])
        n (fn [k] (get u [:w [k :prediction-error]]))]
    (testing "the two keys R3a and R7 declare, each in the branch that writes it"
      (is (= 1 (n :error)))
      (is (= 1 (n :weighted-error))))
    (testing "a key written by all three branches counts once per literal"
      (is (= 3 (n :status))))
    (testing ":reason is written by a literal inside an `or` call (not a direct argument) and by the first branch"
      (is (= 1 (n :reason)) "only the direct literal of the first branch"))
    (testing "the let-bound stamp literal resolves by the existing name rule (one occurrence in the text, three branches return it)"
      (is (= 1 (n :producer-contract))))
    (testing "a key an assoc thread step adds to stamp stays unattributed"
      (is (= 0 (n :channel))))))

(deftest merge-without-a-literal-and-not-in-return-position
  (let [w (fn [src] (get (writes-by-vertex {"a.clj" src}
                                           [(wbox :w [[:target {:record :flight}]] {:returns-record :flight})])
                         [:w [:target :flight]]))]
    (testing "(merge stamp x): no literal, nothing"
      (is (= 0 (w "(defn f [stamp x] (merge stamp x))"))))
    (testing "a literal inside a merge that is not in return position, nothing"
      (is (= 0 (w "(defn f [s x] (log! (merge s {:target x})) (count x))")))
      (is (= 0 (w "(defn f [s x] (let [m (merge s {:target x})] (count m)))")))
      (is (= 0 (w "(defn f [s x] (count (merge s {:target x})))"))))
    (testing "a literal nested in a call inside a merge argument is not a direct argument"
      (is (= 0 (w "(defn f [s x] (merge s (or x {:target 1})))"))))))

(deftest merge-into-conj-in-any-position-and-assoc-pairs
  (let [w (fn [src] (get (writes-by-vertex {"a.clj" src}
                                           [(wbox :w [[:target {:record :flight}]] {:returns-record :flight})])
                         [:w [:target :flight]]))]
    (testing "merge: any position, several literals"
      (is (= 1 (w "(defn f [s x] (merge {:target x} s))")))
      (is (= 2 (w "(defn f [s x] (merge {:target 1} s {:target 2}))"))))
    (testing "into and conj"
      (is (= 1 (w "(defn f [base x] (into base {:target x}))")))
      (is (= 1 (w "(defn f [base x] (conj base {:target x}))")))
      (is (= 1 (w "(defn f [x] (into {:target x} []))"))))
    (testing "assoc: the key-value pairs of a returned assoc, several pairs"
      (is (= 1 (w "(defn f [m x] (assoc m :target x))")))
      (is (= 1 (w "(defn f [m x] (assoc m :other 1 :target x))")))
      (is (= 0 (w "(defn f [m x] (assoc m :other x))"))))
    (testing "in a branch of a cond, an if, a case and a trailing let"
      (is (= 2 (w "(defn f [s x] (if x (merge s {:target 1}) (assoc s :target 2)))")))
      (is (= 1 (w "(defn f [s x] (let [y (inc x)] (merge s {:target y})))")))
      (is (= 2 (w "(defn f [s x] (case x 1 (merge s {:target 1}) (merge s {:target 0})))"))))
    (testing "an assoc that is not returned is not attributed"
      (is (= 0 (w "(defn f [m x] (count (assoc m :target x)))")))
      (is (= 0 (w "(defn f [m x] (log! (assoc m :target x)) (count m))"))))
    (testing "a nested scoped literal inside a returned merge goes to its key record"
      (let [u (writes-by-vertex {"a.clj" "(defn f [s x] (merge s {:cascade-spec {:want x}}))"}
                                [(wbox :w [[:want {:record :cascade-spec}] [:want {:record :flight}]]
                                       {:returns-record :flight})])]
        (is (= 1 (get u [:w [:want :cascade-spec]])))
        (is (= 0 (get u [:w [:want :flight]])))))))
