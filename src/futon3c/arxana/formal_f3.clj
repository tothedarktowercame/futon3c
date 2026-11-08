(ns futon3c.arxana.formal-f3
  "F3a fixture, Lean-kernel evidence adapter, and stable declaration locators."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service])
  (:import [java.io ByteArrayOutputStream File]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]))

(def lean-toolchain "leanprover/lean4:v4.29.0-rc8")
(def historical-informal-commit "fe78c85d1a1b4267f6db16fda884ad403128e2df")
(def fixture-commit "2dd18b2c3547897cb07f8a35084f535b9da7f4c7")
(def informal-path "problems/a93J02/informal-solution.md")
(def lean-main-path "problems/a93J02/lean/Main.lean")

(def kernel-input-paths
  ["lean-toolchain" "lakefile.toml" "lake-manifest.json"
   lean-main-path "problems/a93J02/problem.md" "problems/a93J02/problem.tex"
   "problems/a93J02/proof-outline.md"])

(defn- sha256-bytes [^bytes bs] (digest/sha256-hex bs))
(defn- file-bytes [root path] (Files/readAllBytes (.toPath (io/file root path))))

(defn- run-process [root command]
  (let [builder (doto (ProcessBuilder. ^java.util.List command)
                  (.directory (io/file root))
                  (.redirectErrorStream true))
        process (.start builder)
        output (with-open [in (.getInputStream process)
                           out (ByteArrayOutputStream.)]
                 (.transferTo in out)
                 (.toByteArray out))
        status (.waitFor process)]
    {:exit-status status :output/bytes output
     :output (String. ^bytes output StandardCharsets/UTF_8)}))

(defn- git-output [root & args]
  (let [r (run-process root (into ["git"] args))]
    (when-not (zero? (:exit-status r))
      (throw (ex-info "git command failed" {:args args :result r})))
    (str/trim (:output r))))

(defn- dependency-metadata-digest [root]
  (let [dir (io/file root ".lake/packages/mathlib")]
    (when-not (.exists dir)
      (throw (ex-info "pinned Mathlib package is unavailable" {:path (.getPath dir)})))
    (->> (file-seq dir)
         (filter #(.isFile ^File %))
         (map (fn [^File f]
                [(.toString (.relativize (.toPath dir) (.toPath f)))
                 (.length f) (.lastModified f)]))
         sort vec digest/canonical-digest)))

(defn run-lean-gate
  "Run the pinned a93J02 Lean file and return a complete kernel-run value.
  The dependency metadata digest before/after makes writes through the
  worktree's package link an explicit failure."
  [apm-root]
  (let [actual-toolchain (str/trim (slurp (io/file apm-root "lean-toolchain")))]
    (when-not (= lean-toolchain actual-toolchain)
      (throw (ex-info "unexpected Lean toolchain"
                      {:expected lean-toolchain :actual actual-toolchain})))
    (let [inputs (mapv (fn [path]
                         {:path path :sha256 (sha256-bytes (file-bytes apm-root path))})
                       kernel-input-paths)
          dependency-before (dependency-metadata-digest apm-root)
          command ["lake" "env" "lean" lean-main-path]
          run (run-process apm-root command)
          dependency-after (dependency-metadata-digest apm-root)]
      {:record/type :lean-kernel-run
       :record/version 1
       :toolchain actual-toolchain
       :worktree/commit (git-output apm-root "rev-parse" "HEAD")
       :command command
       :exit-status (:exit-status run)
       :outcome (if (zero? (:exit-status run)) :passed :failed)
       :output (:output run)
       :output/sha256 (sha256-bytes (:output/bytes run))
       :inputs inputs
       :mathlib/metadata-before dependency-before
       :mathlib/metadata-after dependency-after
       :mathlib/unchanged? (= dependency-before dependency-after)})))

(defn kernel-claim [revision-id]
  (records/make-clusion
   {:type-revision [:kernel-builds 1]
    :endpoints [{:role :artifact :kind :artifact-revision
                 :target {:artifact/revision-id revision-id}}]
    :asserted-content {:language :lean}}))

(defn- kernel-attestation [claim run]
  (records/make-attestation
   {:subject-clusion-id (:id claim)
    :producer "futon3c.arxana/evidence-adapter"
    :basis :kernel
    :standing (if (= :passed (:outcome run)) :verified :contradicted)
    :coverage :complete
    :method {:id "lean-kernel" :version (:toolchain run)}
    :content {:outcome (:outcome run)
              :exit-status (:exit-status run)
              :command (:command run)
              :output (:output run)
              :output/sha256 (:output/sha256 run)
              :worktree/commit (:worktree/commit run)
              :mathlib/unchanged? (:mathlib/unchanged? run)}
    :source-refs (mapv #(select-keys % [:path :sha256]) (:inputs run))}))

(defn append-kernel-run!
  "The sole F3a path from a Lean run into Arxana: assert the checked-property
  claim, then attach the complete kernel result through evidence/append."
  [cfg revision-id run]
  (let [claim (kernel-claim revision-id)
        attestation (kernel-attestation claim run)
        dispatch (fn [suffix operation record]
                   (service/dispatch
                    cfg {:protocol/version "arxana.v1"
                         :message/id (str "formal-f3a:" suffix ":" (:id record))
                         :operation operation :principal/id "formal-f3a/evidence-adapter"
                         :body {:record record}}))]
    {:claim claim :attestation attestation
     :claim/write (dispatch "claim" :clusion/assert claim)
     :evidence/write (dispatch "evidence" :evidence/append attestation)}))

(defn historical-informal-bytes [apm-root]
  (let [result (run-process apm-root ["git" "show"
                                      (str historical-informal-commit ":" informal-path)])]
    (when-not (zero? (:exit-status result))
      (throw (ex-info "historical informal source is unavailable"
                      {:commit historical-informal-commit
                       :path informal-path
                       :result (dissoc result :output/bytes)})))
    (:output/bytes result)))

(defn pinned-bytes [apm-root commit path]
  (let [result (run-process apm-root ["git" "show" (str commit ":" path)])]
    (when-not (zero? (:exit-status result))
      (throw (ex-info "pinned fixture source is unavailable"
                      {:commit commit :path path
                       :result (dissoc result :output/bytes)})))
    (:output/bytes result)))

(defn manifestation-plans [futon3c-root apm-root]
  [{:artifact/id "formal-f3:a93J02/authored-informal"
    :role :authored-informal
    :media-type "text/markdown; charset=utf-8"
    :bytes (file-bytes futon3c-root "data/apm-informal-proofs/apm-a93J02.md")}
   {:artifact/id "formal-f3:a93J02/historical-informal-copy"
    :role :historical-authored-informal-copy
    :media-type "text/markdown; charset=utf-8"
    :source/commit historical-informal-commit
    :bytes (historical-informal-bytes apm-root)}
   {:artifact/id "formal-f3:a93J02/current-informal-latex"
    :role :authored-informal-latex
    :media-type "text/markdown; charset=utf-8"
    :source/commit fixture-commit
    :bytes (pinned-bytes apm-root fixture-commit informal-path)}
   {:artifact/id "formal-f3:a93J02/problem-md" :role :problem-markdown
    :media-type "text/markdown; charset=utf-8"
    :source/commit fixture-commit
    :bytes (pinned-bytes apm-root fixture-commit "problems/a93J02/problem.md")}
   {:artifact/id "formal-f3:a93J02/problem-tex" :role :problem-tex
    :media-type "text/x-tex; charset=utf-8"
    :source/commit fixture-commit
    :bytes (pinned-bytes apm-root fixture-commit "problems/a93J02/problem.tex")}
   {:artifact/id "formal-f3:a93J02/proof-outline" :role :proof-outline
    :media-type "text/markdown; charset=utf-8"
    :source/commit fixture-commit
    :bytes (pinned-bytes apm-root fixture-commit "problems/a93J02/proof-outline.md")}
   {:artifact/id "formal-f3:a93J02/lean-main" :role :readable-lean
    :media-type "text/x-lean; charset=utf-8"
    :source/commit fixture-commit
    :bytes (pinned-bytes apm-root fixture-commit lean-main-path)}
   {:artifact/id "formal-f3:a93J02/authored-tex" :role :authored-tex
    :media-type "text/x-tex; charset=utf-8"
    :bytes (file-bytes futon3c-root "resources/arxana/apm-demo/apm-a93J02.tex")}
   {:artifact/id "formal-f3:a93J02/generated-html" :role :generated-html
    :media-type "text/html; charset=utf-8"
    :producer {:tool "latexml_oxide" :version "0.7.5"
               :log/artifact-id "formal-f3:a93J02/latexml-log"}
    :bytes (file-bytes futon3c-root "resources/arxana/apm-demo/index.html")}
   {:artifact/id "formal-f3:a93J02/latexml-log" :role :producer-log
    :media-type "text/plain; charset=utf-8"
    :producer {:tool "latexml_oxide" :version "0.7.5"}
    :bytes (file-bytes futon3c-root
                       "resources/arxana/apm-demo/apm-a93J02.latexml.log")}])

(defn ingest-manifestations! [cfg plans]
  (mapv
   (fn [{:keys [artifact/id media-type bytes] :as plan}]
     (let [response
           (service/dispatch
            cfg {:protocol/version "arxana.v1"
                 :message/id (str "formal-f3a:ingest:" id)
                 :operation :revision/ingest :principal/id "formal-f3a/fixture"
                 ;; Text must enter as canonical UTF-8 text: this selects the
                 ;; futon1b conditional route, which atomically establishes
                 ;; the reserved-route artifact head.  Supplying byte[] is a
                 ;; legacy non-CAS compatibility path.
                 :body {:artifact/id id
                        :bytes (String. ^bytes bytes StandardCharsets/UTF_8)
                        :media-type media-type
                        :expected/revision-id :absent}})]
       (assoc (dissoc plan :bytes) :response response
              :artifact/revision-id (get-in response [:ok :artifact/revision-id]))))
   plans))

(defn declaration-id [fully-qualified-name]
  (str "arxana:lean-declaration:sha256:"
       (digest/canonical-digest {:declaration/name fully-qualified-name})))

(defn declaration-location [fully-qualified-name revision-id]
  (records/make-clusion
   {:type-revision [:declaration-location 1]
    :endpoints [{:role :declaration :kind :entity
                 :target {:declaration/id (declaration-id fully-qualified-name)
                          :declaration/name fully-qualified-name}}
                {:role :artifact :kind :artifact-revision
                 :target {:artifact/revision-id revision-id}}]
    :asserted-content {}}))

(defn declaration-range [^String source declaration-name]
  (let [pattern (re-pattern (str "(?m)^(?:lemma|theorem|def)\\s+"
                                 (java.util.regex.Pattern/quote declaration-name)
                                 "\\b"))
        matcher (re-matcher pattern source)]
    (when-not (.find matcher)
      (throw (ex-info "Lean declaration not found" {:declaration declaration-name})))
    [(.codePointCount source 0 (.start matcher))
     (.codePointCount source 0 (.end matcher))]))

(defn declaration-range-attestation [claim revision-id range]
  (records/make-attestation
   {:subject-clusion-id (:id claim) :producer "futon3c.arxana/lean-source-adapter"
    :basis :parser :standing :verified :coverage :complete
    :method {:id "lean-source/declaration-range" :version 1}
    :content {:artifact/revision-id revision-id :codepoint-range range}
    :source-refs [{:artifact/revision-id revision-id}]}))

(defn -main [& [mode apm-root output-path]]
  (when-not (= mode "lean-gate")
    (throw (ex-info "usage: formal-f3 lean-gate APM_ROOT OUTPUT.edn" {})))
  (let [record (run-lean-gate apm-root)]
    (spit output-path (str (pr-str record) "\n"))
    (println (pr-str (select-keys record [:outcome :exit-status :output/sha256
                                          :mathlib/unchanged?])))))
