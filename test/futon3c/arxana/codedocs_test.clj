(ns futon3c.arxana.codedocs-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.codedocs :as codedocs]
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
