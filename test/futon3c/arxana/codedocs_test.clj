(ns futon3c.arxana.codedocs-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.codedocs :as codedocs]
            [futon3c.arxana.codedocs-relocation :as codedocs-relocation]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io ByteArrayOutputStream File]
           [java.nio.charset StandardCharsets]))

(def futon2-root "/home/joe/code/futon2")
(def futon4-root "/home/joe/code/futon4")
(def mathlib4-root "/home/joe/code/mathlib4")
(def fixture-path "test/resources/arxana/codedocs/fixture.edn")

(defn- fixture [] (edn/read-string (slurp fixture-path)))

(defn- git-bytes [repo sha path]
  (let [process (.start (ProcessBuilder. (into-array String ["git" "-C" repo "show" (str sha ":" path)])))
        out (ByteArrayOutputStream.)]
    (with-open [in (.getInputStream process)] (.transferTo in out))
    (let [status (.waitFor process)]
      (when-not (zero? status)
        (throw (ex-info "git show failed" {:repo repo :sha sha :path path :status status})))
      (.toByteArray out))))

(defn- text-at [sha path]
  (String. ^bytes (git-bytes futon2-root sha path) StandardCharsets/UTF_8))
(defn- text-at-repo [repo sha path]
  (String. ^bytes (git-bytes repo sha path) StandardCharsets/UTF_8))

(defn- codepoint-subs [^String text start end]
  (let [a (.offsetByCodePoints text 0 start)
        b (.offsetByCodePoints text 0 end)]
    (.substring text a b)))

(defn- request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "codedocs-test" :body body})
(defn- call [cfg mid op body] (service/dispatch cfg (request mid op body)))
(defn- record [cfg id]
  (get-in (call cfg (str "get:" id) :entity/get {:entity/id id}) [:ok :record]))
(defn- related [cfg role id]
  (let [response (call cfg (str "related:" (name role) ":" id) :graph/query
                       {:query/type :role-reverse :edge/type [:correspondence 1]
                        :endpoint/role role :endpoint/id id
                        :time/order :desc :query/limit 20})]
    (mapv #(record cfg %) (get-in response [:ok :result/edge-ids]))))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "codedocs-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              repo (corr/in-memory-repository)
              cfg {:graph-store
                   (store-http/http-store
                    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                    f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)
                   :correspondence-repo repo}]
          (try (f cfg repo) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(deftest fixture-digests-pin-git-bytes
  (let [f (fixture)]
    (doseq [version [:historical :current]
            [path expected] (get-in f [version :files])]
      (is (= expected (digest/sha256-hex
                       (git-bytes futon2-root (get-in f [version :repo/sha]) path)))
          (str version " " path)))
    (doseq [[path expected] (get-in f [:docbook :files])]
      (is (= expected (digest/sha256-hex
                       (git-bytes futon4-root (get-in f [:docbook :repo/sha]) path)))
          path))
    (is (= (get-in f [:lean :sha256])
           (digest/sha256-hex
            (git-bytes mathlib4-root (get-in f [:lean :repo/sha])
                       (get-in f [:lean :file])))))
    (is (false? (get-in f [:docbook :mojibake])))
    (is (not= "not-the-pinned-digest"
              (digest/sha256-hex
               (git-bytes futon2-root (get-in f [:historical :repo/sha])
                          "src/futon2/aif/full_loop_runtime.clj"))))))

(deftest shared-finder-covers-private-and-metadata-forms
  (let [text "(defn- ^:private hidden [x] x)\n(def ^:dynamic *port* 1)\n"
        defs (codedocs/definitions text #{"hidden" "*port*"})]
    (is (= :defn- (get-in defs ["hidden" :definition/form])))
    (is (= :def (get-in defs ["*port*" :definition/form])))
    (is (= "(defn-" (subs text 0 6)))
    (is (= [0 30] (get-in defs ["hidden" :codepoint-range])))))

(deftest pinned-code-definitions-are-stored-and-position-resolved
  (with-stack
    (fn [cfg repo]
      (let [f (fixture)
            old-sha (get-in f [:historical :repo/sha])
            new-sha (get-in f [:current :repo/sha])
            plans
            (for [[path symbols] (:symbols f)]
              {:artifact-id (str "codedocs:futon2/" path)
               :symbols symbols
               :historical {:sha old-sha :text (text-at old-sha path)}
               :current {:sha new-sha :text (text-at new-sha path)}})
            results (into {} (for [plan plans]
                               [(:artifact-id plan)
                                (codedocs/ingest-file! cfg repo plan)]))]
        (doseq [plan plans
                :let [artifact-id (:artifact-id plan)
                      versions (results artifact-id)
                      current-rid (:revision-id (second versions))]]
          (testing (str artifact-id " has both immutable revisions and current head")
            (is (= 2 (count (distinct (map :revision-id versions)))))
            (is (= current-rid
                   (get-in (call cfg (str "head:" artifact-id) :revision/head
                                 {:artifact/id artifact-id})
                           [:ok :head/revision-id]))))
          (doseq [{:keys [revision-id scopes]} versions
                  [symbol {:keys [scope attestation codepoint-range]}] scopes]
            (let [stored-scope (record cfg (:id scope))
                  stored-att (record cfg (:id attestation))
                  source (get-in (call cfg (str "revision:" revision-id) :revision/get
                                       {:artifact/revision-id revision-id})
                                 [:ok :revision/text])
                  [start end] codepoint-range
                  inside (call cfg (str "at:" (:id scope)) :scope/at-position
                               {:artifact/revision-id revision-id
                                :position/codepoint start})]
              (testing (str symbol " is an exact stored scope with range evidence")
                (is (= scope stored-scope))
                (is (= attestation stored-att))
                (is (= symbol (get-in stored-scope [:asserted-content :symbol/name])))
                ;; identity is exactly these fields: no range, offset or line
                (is (= #{:artifact/revision-id :scope/kind :symbol/name :definition/form}
                       (set (keys (:asserted-content stored-scope)))))
                (is (empty? (:endpoints stored-scope)))
                (is (= codepoint-range (get-in stored-att [:content :codepoint-range])))
                (is (str/starts-with? (codepoint-subs source start (min end (+ start 12)))
                                      (str "(" (name (get-in stored-scope
                                                              [:asserted-content :definition/form])))))
                (is (= (:id scope) (get-in inside [:ok :result/innermost :subject/id])))
                (testing "the range is the whole top-level form, no more"
                  (let [form-text (codepoint-subs source start end)
                        parsed (read-string form-text)
                        at (fn [pos] (get-in (call cfg (str "at:" (:id scope) ":" pos)
                                                   :scope/at-position
                                                   {:artifact/revision-id revision-id
                                                    :position/codepoint pos})
                                             [:ok :result/innermost :subject/id]))]
                    (is (str/ends-with? form-text ")"))
                    (is (= symbol (str (second parsed))))
                    (is (= (:id scope) (at (quot (+ start end) 2))))
                    (is (= (:id scope) (at (dec end))))
                    (is (not= (:id scope) (at end)))))))))
        (doseq [[_ versions] results
                {:keys [revision-id]} versions]
          (let [outside (call cfg (str "outside:" revision-id) :scope/at-position
                              {:artifact/revision-id revision-id :position/codepoint 0})]
            (is (= :arxana.scope/no-enclosing-scope
                   (get-in outside [:error :error/code])))))))))

(deftest pinned-doc-entries-and-code-doc-correspondences
  (with-stack
    (fn [cfg repo]
      (let [f (fixture)
            old-sha (get-in f [:historical :repo/sha])
            new-sha (get-in f [:current :repo/sha])
            code-plans (for [[path symbols] (:symbols f)]
                         {:artifact-id (str "codedocs:futon2/" path)
                          :path path :symbols symbols
                          :historical {:sha old-sha :text (text-at old-sha path)}
                          :current {:sha new-sha :text (text-at new-sha path)}})
            code-results (into {}
                               (for [plan code-plans]
                                 [(:path plan) (codedocs/ingest-file! cfg repo plan)]))
            current-code (into {}
                               (for [[path versions] code-results]
                                 [path (:scopes (second versions))]))
            doc-sha (get-in f [:docbook :repo/sha])
            doc-results
            (vec (for [[path expected-digest] (get-in f [:docbook :files])
                       :let [text (text-at-repo futon4-root doc-sha path)
                             source-path (second (re-find #"(?m)^:SOURCE_PATH:\s*(.*?)\s*$" text))
                             symbols (get-in f [:symbols source-path])]]
                   (do (is (= expected-digest (digest/sha256-hex (.getBytes text StandardCharsets/UTF_8))))
                       (codedocs/ingest-doc!
                        cfg {:artifact-id (str "codedocs:futon4/" path)
                             :sha doc-sha :text text :symbols symbols}))))
            links (codedocs/link-code-docs! cfg current-code doc-results)]
        (is (= 5 (count links)))
        (doseq [{:keys [revision-id source-path scopes]} doc-results
                [symbol {:keys [scope attestation codepoint-range]}] scopes]
          (let [stored-scope (record cfg (:id scope))
                stored-att (record cfg (:id attestation))
                text (get-in (call cfg (str "doc-revision:" revision-id) :revision/get
                                  {:artifact/revision-id revision-id})
                             [:ok :revision/text])
                [start end] codepoint-range
                span (codepoint-subs text start end)
                next-text (codepoint-subs text end (min (.codePointCount text 0 (.length text))
                                                        (+ end 2)))]
            (testing (str symbol " is one exact whole def-doc bullet")
              (is (= scope stored-scope))
              (is (= attestation stored-att))
              (is (= #{:artifact/revision-id :entry/id :symbol/name}
                     (set (keys (:asserted-content stored-scope)))))
              (is (empty? (:endpoints stored-scope)))
              (is (= source-path (get-in stored-att [:content :source/path])))
              (is (= codepoint-range (get-in stored-att [:content :codepoint-range])))
              (is (str/starts-with? span (str "- " symbol ":")))
              (is (or (= end (.codePointCount text 0 (.length text)))
                      (str/starts-with? next-text "- ")
                      (str/starts-with? next-text "* ")))
              ;; There is no unconsumed next bullet or heading inside the span.
              (is (nil? (re-find #"(?m)\n(?:- |\*+ )" span))))))
        (doseq [{:keys [symbol code-scope doc-scope correspondence attestation]} links]
          (let [stored (record cfg (:id correspondence))
                stored-evidence (record cfg (:id attestation))]
            (testing (str symbol " links the current code scope to its exact doc entry")
              (is (= correspondence stored))
              (is (= attestation stored-evidence))
              (is (= symbol (get-in (record cfg (:id code-scope))
                                    [:asserted-content :symbol/name])))
              (is (= symbol (get-in (record cfg (:id doc-scope))
                                    [:asserted-content :symbol/name])))
              (is (= [(:id correspondence)]
                     (mapv :id (related cfg :source (:id code-scope)))))
              (is (= [(:id correspondence)]
                     (mapv :id (related cfg :target (:id doc-scope)))))
              (is (= (get-in f [:current :repo/sha]) new-sha))
              (is (= (:revision-id (second (code-results
                                             (get-in stored-evidence [:content :source/path]))))
                     (get-in code-scope [:asserted-content :artifact/revision-id]))))))
        (testing "SOURCE_PATH, not stub filename, controls matching"
          (let [wrong-text (str ":PROPERTIES:\n:ENTRY_ID: wrong::def-doc\n"
                                ":SOURCE_PATH: src/futon2/aif/not-this-file.clj\n:END:\n\n"
                                "* Context\n- route!: wrong-path entry\n")
                wrong (codedocs/ingest-doc!
                       cfg {:artifact-id "codedocs:wrong-path" :sha "synthetic"
                            :text wrong-text :symbols ["route!"]})]
            (is (empty? (codedocs/link-code-docs! cfg current-code [wrong])))
            ;; same file name in another directory is a different source
            (let [twin (codedocs/ingest-doc!
                        cfg {:artifact-id "codedocs:same-basename" :sha "synthetic"
                             :text (str ":PROPERTIES:\n:ENTRY_ID: twin::def-doc\n"
                                        ":SOURCE_PATH: src/elsewhere/meta_self_heal_production.clj\n"
                                        ":END:\n\n* Context\n- route!: same basename, other dir\n")
                             :symbols ["route!"]})]
              (is (empty? (codedocs/link-code-docs! cfg current-code [twin]))))
            (is (empty? (related cfg :target
                                 (get-in wrong [:scopes "route!" :scope :id]))))))
        (testing "a symbol mentioned in another bullet is not an entry"
          (let [mentioned (str ":PROPERTIES:\n:ENTRY_ID: mention::def-doc\n"
                               ":SOURCE_PATH: src/futon2/aif/meta_self_heal_production.clj\n"
                               ":END:\n\n* Context\n- other: calls route! in prose\n")]
            (is (empty? (codedocs/doc-entries mentioned #{"route!"})))))
        (testing "a wrapped bullet remains one scope through the next boundary"
          (let [wrapped (str ":PROPERTIES:\n:ENTRY_ID: wrap::def-doc\n"
                             ":SOURCE_PATH: src/x.clj\n:END:\n\n* Context\n"
                             "- route!: first line\n  continuation naming prepare-diagnosis\n"
                             "- next: boundary\n")
                [start end] (get-in (codedocs/doc-entries wrapped #{"route!"})
                                    ["route!" :codepoint-range])]
            (is (= "- route!: first line\n  continuation naming prepare-diagnosis\n"
                   (codepoint-subs wrapped start end)))))))))

(defn- independent-lean-citation-counts [text symbols]
  ;; Deliberately independent of the lexer: only column-zero doc comments
  ;; immediately followed by a column-zero top-level declaration are counted.
  (let [attached #"(?ms)^(/--.*?-/)[ \t\r\n]*(?:theorem|lemma|def|abbrev|structure|inductive|instance)\s+[^\s:(]+"]
    (reduce
     (fn [counts [_ comment]]
       (reduce (fn [m symbol]
                 (if (str/includes? comment (str "`" symbol "`"))
                   (update m symbol (fnil inc 0)) m))
               counts symbols))
     (zipmap symbols (repeat 0))
     (re-seq attached text))))

(deftest lean-declarations-and-exact-code-citations
  (with-stack
    (fn [cfg repo]
      (let [f (fixture)
            old-sha (get-in f [:historical :repo/sha])
            new-sha (get-in f [:current :repo/sha])
            code-results
            (into {}
                  (for [[path symbols] (:symbols f)
                        :let [plan {:artifact-id (str "codedocs:futon2/" path)
                                    :symbols symbols
                                    :historical {:sha old-sha :text (text-at old-sha path)}
                                    :current {:sha new-sha :text (text-at new-sha path)}}]]
                    [path (codedocs/ingest-file! cfg repo plan)]))
            current-code (into {}
                               (for [[_ versions] code-results
                                     [symbol scope-info] (:scopes (second versions))]
                                 [symbol (:scope scope-info)]))
            symbols (set (mapcat val (:symbols f)))
            lean-sha (get-in f [:lean :repo/sha])
            lean-path (get-in f [:lean :file])
            lean-text (text-at-repo mathlib4-root lean-sha lean-path)
            independent (independent-lean-citation-counts lean-text symbols)
            result (codedocs/ingest-lean!
                    cfg {:artifact-id "codedocs:mathlib4/SelfHealRoute.lean"
                         :sha lean-sha :text lean-text :symbols symbols
                         :code-scopes current-code})
            by-symbol (merge (zipmap symbols (repeat 0))
                             (frequencies (map :symbol (:citations result))))]
        (is (= (get-in f [:lean :sha256])
               (digest/sha256-hex (.getBytes lean-text StandardCharsets/UTF_8))))
        ;; The doc-comment-only regex undercounts: the legacy rule also reads
        ;; `--` comments in the declaration body (review, claude-13).
        (is (= {"run-opportunity!" 0 "prepare-diagnosis" 0
                "production-readiness" 3 "route!" 8 "prepare-acceptance" 0}
               independent))
        ;; Oracle: the legacy browser's own `arxana-browser-code--lean-declarations`
        ;; + backtick rule run on this pinned file (futon4 dev/arxana-browser-code.el,
        ;; 2026-11-09): pairs per function.
        (is (= {"run-opportunity!" 0 "prepare-diagnosis" 1
                "production-readiness" 4 "route!" 10 "prepare-acceptance" 1}
               by-symbol))
        (is (= 16 (count (:citations result))))
        (is (some #(and (= "prepare-diagnosis" (:symbol %))
                        (str/ends-with? (:fqn %) ".RouteState"))
                  (:citations result))
            "a `--` comment in RouteState's constructor list cites prepare-diagnosis")
        (doseq [[fqn {:keys [scope attestation declaration]}] (:scopes result)]
          (let [stored-scope (record cfg (:id scope))
                stored-att (record cfg (:id attestation))
                [start end] (:codepoint-range declaration)
                [doc-start doc-end] (:doc-comment-range declaration)
                declaration-text (codepoint-subs lean-text start end)
                comment-text (codepoint-subs lean-text doc-start doc-end)]
            (testing (str fqn " is an exact namespace-qualified Lean scope")
              (is (= scope stored-scope))
              (is (= attestation stored-att))
              (is (= #{:artifact/revision-id :declaration/fqn}
                     (set (keys (:asserted-content stored-scope)))))
              (is (str/starts-with? fqn "DarkTower.WarMachine.SelfHealRoute."))
              (is (str/starts-with? declaration-text
                                    (str (name (:declaration/keyword declaration)) " "
                                         (:declaration/name declaration))))
              (is (and (str/starts-with? comment-text "/--")
                       (str/ends-with? comment-text "-/")))
              (is (= (:codepoint-range declaration)
                     (get-in stored-att [:content :codepoint-range])))
              (is (= (:doc-comment-range declaration)
                     (get-in stored-att [:content :doc-comment-range]))))))
        (doseq [{:keys [symbol lean-scope code-scope correspondence attestation]}
                (:citations result)]
          (let [stored-edge (record cfg (:id correspondence))
                stored-att (record cfg (:id attestation))
                [start end] (get-in stored-att [:content :token/codepoint-range])]
            (testing (str "exact backticked citation " symbol)
              (is (= correspondence stored-edge))
              (is (= attestation stored-att))
              (is (= :parser (:basis stored-att)))
              (is (= :unverified (:standing stored-att)))
              (is (not= :kernel (:basis stored-att)))
              (is (= (str "`" symbol "`") (codepoint-subs lean-text start end)))
              (is (= new-sha (get-in f [:current :repo/sha])))
              (is (= (get-in current-code [symbol :asserted-content :artifact/revision-id])
                     (get-in code-scope [:asserted-content :artifact/revision-id])))
              (is (some #{(:id correspondence)}
                        (map :id (filter #(= :cites-code
                                             (get-in % [:asserted-content :relation/kind]))
                                         (related cfg :source (:id lean-scope))))))
              (is (some #{(:id correspondence)}
                        (map :id (related cfg :target (:id code-scope))))))))
        (testing "only attached exact backticks cite, and namespaces qualify FQNs"
          (let [synthetic (str "/-! module `route!` -/\n"
                               "namespace Demo\n"
                               "/-- bare route! only -/\ndef bare : Nat := 0\n"
                               "/- ordinary `route!` -/\ndef ordinary : Nat := 0\n"
                               "def text : String := \"theorem Fake and `route!`\"\n"
                               "/- outer /- nested theorem Nope -/ comment -/\n"
                               "/-- exact `route!` -/\ntheorem good : True := by trivial\n"
                               "inductive Body where\n  -- stage (`route!`)\n  | a\n"
                               "end Demo\n")
                declarations (codedocs/lean-declarations synthetic #{"route!"})
                cited (filterv (comp seq :citations) declarations)]
            (is (= ["Demo.good" "Demo.Body"] (mapv :declaration/fqn cited)))
            ;; a module docstring directly above a declaration is not its doc
            (is (empty? (filterv (comp seq :citations)
                                 (codedocs/lean-declarations
                                  "/-! module `route!` -/\ndef m : Nat := 0\n" #{"route!"}))))
            (is (= ["route!"] (mapv :symbol/name (:citations (first cited)))))))))))

(defn- materialize-view-fixture! [cfg repo reverse-order?]
  (let [f (fixture)
        old-sha (get-in f [:historical :repo/sha])
        new-sha (get-in f [:current :repo/sha])
        ordered (cond->> (:symbols f) reverse-order? reverse)
        code-results
        (into {}
              (for [[path symbols] ordered
                    :let [plan {:artifact-id (str "codedocs:futon2/" path)
                                :symbols symbols
                                :historical {:sha old-sha :text (text-at old-sha path)}
                                :current {:sha new-sha :text (text-at new-sha path)}}]]
                [path (codedocs/ingest-file! cfg repo plan)]))
        current-by-path (into {} (for [[path versions] code-results]
                                   [path (:scopes (second versions))]))
        current-by-symbol (into {} (for [[_ scopes] current-by-path
                                         [symbol info] scopes]
                                     [symbol (:scope info)]))
        doc-sha (get-in f [:docbook :repo/sha])
        doc-files (cond->> (get-in f [:docbook :files]) reverse-order? reverse)
        doc-results
        (vec (for [[path _] doc-files
                   :let [text (text-at-repo futon4-root doc-sha path)
                         source-path (second (re-find #"(?m)^:SOURCE_PATH:\s*(.*?)\s*$" text))]]
               (codedocs/ingest-doc!
                cfg {:artifact-id (str "codedocs:futon4/" path) :sha doc-sha :text text
                     :symbols (get-in f [:symbols source-path])})))
        doc-links (codedocs/link-code-docs! cfg current-by-path doc-results)
        lean-sha (get-in f [:lean :repo/sha])
        lean-text (text-at-repo mathlib4-root lean-sha (get-in f [:lean :file]))
        lean-result (codedocs/ingest-lean!
                     cfg {:artifact-id "codedocs:mathlib4/SelfHealRoute.lean"
                          :sha lean-sha :text lean-text :symbols (set (keys current-by-symbol))
                          :code-scopes current-by-symbol})
        path "src/futon2/aif/meta_self_heal_production.clj"
        versions (code-results path)]
    {:historical (:revision-id (first versions)) :current (:revision-id (second versions))
     :code-results code-results :doc-links doc-links :lean-result lean-result}))

(defn- attestation-ids-for [cfg subject-id]
  (get-in (call cfg (str "attestations:" subject-id) :graph/query
                {:query/type :attestations :subject/id subject-id :query/limit 100})
          [:ok :result/attestation-ids]))

(deftest historical-code-scopes-relocate-honestly-and-ambiguity-is-preserved
  (with-stack
    (fn [cfg repo]
      (let [{:keys [code-results doc-links lean-result]}
            (materialize-view-fixture! cfg repo false)
            immutable-ids (vec (concat
                                (for [[_ versions] code-results
                                      version versions
                                      [_ info] (:scopes version)]
                                  (get-in info [:scope :id]))
                                (map #(get-in % [:correspondence :id]) doc-links)
                                (map #(get-in % [:correspondence :id])
                                     (:citations lean-result))))
            before (into {} (map (juxt identity #(record cfg %)) immutable-ids))
            rows
            (vec
             (for [[path versions] (sort-by key code-results)
                   :let [historical (first versions)
                         current (second versions)
                         source-text (get-in (call cfg (str "old-text:" path) :revision/get
                                                  {:artifact/revision-id (:revision-id historical)})
                                             [:ok :revision/text])]
                   [symbol old-info] (sort-by key (:scopes historical))
                   :let [current-info (get-in current [:scopes symbol])
                         result (codedocs-relocation/relocate-one
                                 (:artifact-provider cfg) source-text
                                 (:revision-id historical) (:revision-id current)
                                 (get-in old-info [:scope :id]) (:codepoint-range old-info))
                         {:keys [attestation]} (codedocs-relocation/append-one! cfg result)
                         stored (record cfg (:id attestation))
                         candidates (get-in stored [:content :candidates])
                         relocated-range (when (= 1 (count candidates))
                                           (let [r (get-in candidates [0 :location :range])]
                                             [(:start r) (:end r)]))
                         current-scope-id (get-in current-info [:scope :id])
                         doc-count (count (filter #(= current-scope-id
                                                       (get-in % [:code-scope :id])) doc-links))
                         lean-count (count (filter #(= current-scope-id
                                                        (get-in % [:code-scope :id]))
                                                  (:citations lean-result)))]]
               (do
                 (is (= attestation stored) (str symbol " relocation read back exactly"))
                 (is (some #{(:id attestation)}
                           (attestation-ids-for cfg (get-in old-info [:scope :id])))
                     (str symbol " relocation is publicly queryable"))
                 {:symbol symbol :path path
                  :historical true
                  :outcome (codedocs-relocation/outcome stored)
                  :relocated/range relocated-range
                  :current/range (:codepoint-range current-info)
                  :range-equals-current? (= relocated-range (:codepoint-range current-info))
                  :symbol-finder-would-recover? (boolean current-info)
                  :current-scope/id current-scope-id
                  :code-doc-links doc-count :lean-code-links lean-count
                  :code-doc-links-target-current? (pos? doc-count)
                  :lean-links-target-current? (pos? lean-count)
                  :links-reach-relocation? (and (= relocated-range (:codepoint-range current-info))
                                                (pos? doc-count))})))
            stored-report {:fixture {:historical (get-in (fixture) [:historical :repo/sha])
                                     :current (get-in (fixture) [:current :repo/sha])}
                           :counts {:historical (count rows)
                                    :current (count (filter :range-equals-current? rows))
                                    :both (count (filter #(and (:historical %)
                                                               (:range-equals-current? %)) rows))}
                           :rows rows}
            expected-report (edn/read-string
                             (slurp "test/resources/arxana/codedocs/survives-edits.edn"))]
        (is (= expected-report stored-report))
        (is (= before (into {} (map (juxt identity #(record cfg %)) immutable-ids)))
            "relocation appends evidence without changing scopes or correspondences")
        (testing "duplicated exact content remains ambiguous with both candidates"
          (let [row (first rows)
                path (:path row)
                historical (first (code-results path))
                old-info (get-in historical [:scopes (:symbol row)])
                source-text (get-in (call cfg "duplicate-source" :revision/get
                                          {:artifact/revision-id (:revision-id historical)})
                                    [:ok :revision/text])
                [start end] (:codepoint-range old-info)
                form (codepoint-subs source-text start end)
                duplicate-text (str "prefix\n" form "\nseparator\n" form "\nsuffix\n")
                ingested (artifact/ingest! (:artifact-provider cfg) "codedocs:duplicate"
                                           (.getBytes duplicate-text StandardCharsets/UTF_8)
                                           "text/x-clojure; charset=utf-8")
                target-id (get-in ingested [:ok :artifact/revision-id])
                result (codedocs-relocation/relocate-one
                        (:artifact-provider cfg) source-text (:revision-id historical)
                        target-id (get-in old-info [:scope :id]) (:codepoint-range old-info))]
            (is (= :ambiguous (get-in result [:resolution :result/status])))
            (is (= 2 (count (get-in result [:resolution :result/candidates]))))
            ;; the stored attestation, read back, says the same (review, claude-13)
            (let [{:keys [attestation response]} (codedocs-relocation/append-one! cfg result)
                  stored (record cfg (:id attestation))]
              (is (:ok response))
              (is (= attestation stored))
              (is (= :ambiguous (codedocs-relocation/outcome stored)))
              (is (= 2 (count (get-in stored [:content :candidates])))))))))))

(deftest registered-codedocs-view-is-public-bounded-and-deterministic
  (let [runs (atom [])]
    (doseq [reverse-order? [false true]]
      (with-stack
        (fn [cfg repo]
          (let [{:keys [historical current]} (materialize-view-fixture!
                                               cfg repo reverse-order?)
                current-view (call cfg (str "view-current:" reverse-order?) :codedocs/view
                                   {:artifact/revision-id current})
                historical-view (call cfg (str "view-old:" reverse-order?) :codedocs/view
                                      {:artifact/revision-id historical})
                missing-view (call cfg (str "view-missing:" reverse-order?) :codedocs/view
                                   {:artifact/revision-id "sha256:not-a-revision"})
                view (:ok current-view)
                definitions (:definitions view)
                by-symbol (into {} (map (juxt :symbol identity) definitions))]
            (testing "current view has source-ordered definitions and exact linked text"
              (is (= :complete (:result/status view)))
              (is (= :materialized (:links/status view)))
              (is (= current (:links/at-revision view)))
              (is (= ["prepare-acceptance" "prepare-diagnosis"
                      "production-readiness" "route!"]
                     (mapv :symbol definitions)))
              (doseq [{:keys [symbol docs lean]} definitions]
                (is (= 1 (count docs)))
                (is (str/starts-with? (:text (first docs)) (str "- " symbol ":")))
                (is (every? #(not (str/includes? (or (:doc/first-sentence %) "") "`"))
                            lean)))
              (is (= ["RouteState"]
                     (mapv #(last (str/split (:declaration/fqn %) #"\."))
                           (get-in by-symbol ["prepare-diagnosis" :lean]))))
              (is (= ["CoordinatorRefusal" "ReadinessTerm" "RouteState" "RouteTerm"
                      "click11PredictedPath" "click4RecordedSuffix"
                      "click4_runtimeOrderIsTabled" "coordination_66_reportedToday"
                      "fallthrough_5_today" "transitions"]
                     (mapv #(last (str/split (:declaration/fqn %) #"\."))
                           (get-in by-symbol ["route!" :lean]))))
              (doseq [id (:record/ids view)]
                (is (= id (:id (record cfg id))))))
            (testing "historical definitions explicitly report unmaterialized links"
              (let [old (:ok historical-view)]
                (is (= ["prepare-acceptance" "prepare-diagnosis"
                        "production-readiness" "route!"]
                       (mapv :symbol (:definitions old))))
                (is (= :not-materialized-for-revision (:links/status old)))
                (is (nil? (:links/at-revision old)))
                (is (every? #(and (empty? (:docs %)) (empty? (:lean %)))
                            (:definitions old)))))
            (is (= :arxana.artifact/revision-not-found
                   (get-in missing-view [:error :error/code])))
            (swap! runs conj {:current (:ok current-view)
                              :historical (:ok historical-view)})))))
    (testing "record write order does not affect either projection"
      (is (= (first @runs) (second @runs))))))
