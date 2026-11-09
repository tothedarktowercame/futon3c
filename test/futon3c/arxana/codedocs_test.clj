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
                (is (nil? (get-in stored-scope [:asserted-content :codepoint-range])))
                (is (= codepoint-range (get-in stored-att [:content :codepoint-range])))
                (is (str/starts-with? (codepoint-subs source start (min end (+ start 12)))
                                      (str "(" (name (get-in stored-scope
                                                              [:asserted-content :definition/form])))))
                (is (= (:id scope) (get-in inside [:ok :result/innermost :subject/id])))))))
        (doseq [[_ versions] results
                {:keys [revision-id]} versions]
          (let [outside (call cfg (str "outside:" revision-id) :scope/at-position
                              {:artifact/revision-id revision-id :position/codepoint 0})]
            (is (= :arxana.scope/no-enclosing-scope
                   (get-in outside [:error :error/code])))))))))
