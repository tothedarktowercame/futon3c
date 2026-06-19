(ns futon3c.peripheral.street-sweeper-test
  "Tests for the Street Sweeper peripheral.

   Coverage targets:
   - spike-check (envelope file exists + config valid)
   - INV-1 cg-id binding: emit + lookup + repo-match + files-allowed
   - INV-2 secret-pattern reject
   - INV-3 file-count cap on commit
   - INV-10 regenerable-artifact relocation proposal
   - INV-12/15-packet-size cap
   - INV-13 universal exclusion patterns
   - INV-14 cross-repo dep detection + accept-broken-cross-ref override
   - INV-15 auto-approve criteria (each check independently)
   - INV-17 candidate-invariant clustering at threshold
   - build-packets clustering algorithm
   - run-full-sweep orchestration smoke test

   Run via Drawbridge nREPL:
     (require '[clojure.test :as t]
              '[futon3c.peripheral.street-sweeper-test])
     (t/run-tests 'futon3c.peripheral.street-sweeper-test)"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.peripheral.street-sweeper :as ss]
            [futon3c.peripheral.street-sweeper-shapes :as sss]
            [futon3c.peripheral.street-sweeper-backend :as ssb]))

(defn- sh!
  [dir & args]
  (let [r (apply shell/sh (concat args [:dir dir]))]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "command failed: " (str/join " " args))
                      {:args args :dir dir :out (:out r) :err (:err r)})))
    r))

(defn- delete-tree!
  [f]
  (when (.exists (io/file f))
    (doseq [child (reverse (file-seq (io/file f)))]
      (io/delete-file child true))))

;; =============================================================================
;; Spike + config
;; =============================================================================

(deftest spike-check-returns-valid
  (let [r (ss/spike-check)]
    (is (:valid-config? r) "domain config passes cycle/valid-domain-config?")
    (is (:make-sweeper-ok? r) "make-sweeper returns without error")
    (is (nil? (:spike-error r)))
    (is (contains? (set (:verified-criteria r)) :C1-envelope-file-exists-and-config-valid))
    (is (contains? (set (:verified-criteria r)) :C2-make-sweeper-returns-without-error))))

(deftest shapes-loaded
  (testing "data-driven INV declarations are populated"
    (is (pos? (count sss/stage-invariants)))
    (is (pos? (count sss/auto-approve-checks)))
    (is (pos? (count sss/secret-patterns)))
    (is (pos? (count sss/exclusion-patterns)))
    (is (pos? (count sss/defer-reasons)))
    (is (contains? sss/substantive-tools :repo-stage))
    (is (contains? sss/substantive-tools :repo-commit))
    (is (not (contains? sss/substantive-tools :write-defer-manifest))
        "write-defer-manifest is NOT substantive (writes to operator-archive only)")))

;; =============================================================================
;; INV-2: secret-pattern reject
;; =============================================================================

(deftest inv-2-secret-pattern-reject
  (let [r (ssb/apply-stage-invariants "futon0"
                                      [".env" ".admintoken" "credentials.json"
                                       "id_rsa" "secret-tokens.edn"
                                       "src/safe.clj"])]
    (testing ".env / .admintoken / credentials / id_rsa / secret patterns rejected"
      (is (= 5 (count (:rejected r))))
      (is (every? (fn [hit]
                    (= :secret-pattern-match (-> hit :hits first :reason)))
                  (:rejected r))))
    (testing "non-secret path stays in :ok"
      (is (contains? (set (:ok r)) "src/safe.clj")))))

;; =============================================================================
;; INV-13: universal exclusion
;; =============================================================================

(deftest inv-13-universal-exclusion-reject
  (let [r (ssb/apply-stage-invariants "futon3c"
                                      ["data/proof-state/x.edn"
                                       "logs/server.log"
                                       ".shadow-cljs/foo"
                                       "node_modules/lib.js"
                                       "__pycache__/x.pyc"
                                       "target/build.jar"
                                       "x.swp"
                                       "src/futon3c/foo.clj"])]
    (is (= 7 (count (:rejected r))))
    (is (every? (fn [hit]
                  (= :universal-exclusion (-> hit :hits first :reason)))
                (:rejected r)))
    (is (= ["src/futon3c/foo.clj"] (:ok r)))))

;; =============================================================================
;; INV-10: regenerable-artifact relocation proposal
;; =============================================================================

(deftest inv-10-generated-extension-proposes-relocation
  (let [r (ssb/apply-stage-invariants "futon3a"
                                      ["resources/notions/minilm_corpus_embeddings.json"
                                       "README.md"])]
    (testing "JSON proposed for relocation (extension-based)"
      (is (= 1 (count (:proposed r))))
      (is (= "resources/notions/minilm_corpus_embeddings.json"
             (-> r :proposed first :path)))
      (is (= :regenerable-artifact (-> r :proposed first :hits first :reason))))
    (testing "Markdown stays in :ok (hand-authored)"
      (is (contains? (set (:ok r)) "README.md")))))

;; =============================================================================
;; INV-14: cross-repo dep detection
;; =============================================================================

(deftest inv-14-extract-cross-repo-refs
  (testing "regex captures /home/joe/code/<repo>/<rel-path>"
    (let [refs (ssb/extract-cross-repo-refs
                "source /home/joe/code/futon3c/scripts/emacs-socket-lib.sh\n")]
      (is (= 1 (count refs)))
      (is (= "futon3c" (-> refs first :sister-repo)))
      (is (= "scripts/emacs-socket-lib.sh" (-> refs first :rel-path)))))

  (testing "regex captures ~/code/<repo>/<rel-path>"
    (let [refs (ssb/extract-cross-repo-refs
                "cat ~/code/futon2/scripts/foo.bb")]
      (is (= 1 (count refs)))
      (is (= "futon2" (-> refs first :sister-repo)))
      (is (= "scripts/foo.bb" (-> refs first :rel-path)))))

  (testing "multiple distinct refs deduplicate"
    (let [refs (ssb/extract-cross-repo-refs
                "/home/joe/code/futon0/x.el\n/home/joe/code/futon0/x.el\n/home/joe/code/futon2/y.clj")]
      (is (= 2 (count refs))))))

(deftest inv-14-accept-broken-override
  (testing "with :accept-broken-cross-ref true the check returns nil"
    (is (nil? (ssb/check-cross-repo-deps "futon0" ["scripts/cr"]
                                         {:accept-broken-cross-ref true})))))

;; =============================================================================
;; INV-1: cg-id binding registry
;; =============================================================================

(deftest inv-1-emit-binds-cg-id
  (let [r (ssb/consent-gate-emit {:repo "futon0"
                                  :intent :test
                                  :files ["scripts/cr"]})]
    (is (:ok r))
    (is (true? (-> r :result :bound?)))
    (is (str/starts-with? (-> r :result :consent-gate-event-id) "cg-"))
    (is (= 3600000 (-> r :result :ttl-ms)))))

(deftest inv-1-stage-rejects-wrong-repo
  (let [emit (ssb/consent-gate-emit {:repo "futon0" :intent :test
                                     :files ["scripts/cr"]})
        cg-id (-> emit :result :consent-gate-event-id)
        bad   (ssb/repo-stage {:repo "futon3c"
                               :files ["src/futon3c/foo.clj"]
                               :consent-gate-event-id cg-id})]
    (is (false? (:ok bad)))
    (is (= :INV-1 (:pilot-invariant bad)))
    (is (str/includes? (:error bad) "bound to repo futon0"))))

(deftest inv-1-stage-rejects-files-outside-allowlist
  (let [emit (ssb/consent-gate-emit {:repo "futon0" :intent :test
                                     :files ["scripts/cr"]})
        cg-id (-> emit :result :consent-gate-event-id)
        bad   (ssb/repo-stage {:repo "futon0"
                               :files ["scripts/not-in-allowlist"]
                               :consent-gate-event-id cg-id})]
    (is (false? (:ok bad)))
    (is (= :INV-1 (:pilot-invariant bad)))))

(deftest inv-1-stage-rejects-unknown-cg-id
  (let [bad (ssb/repo-stage {:repo "futon0"
                             :files ["scripts/cr"]
                             :consent-gate-event-id "cg-this-id-does-not-exist"})]
    (is (false? (:ok bad)))
    (is (= :INV-1 (:pilot-invariant bad)))
    (is (str/includes? (:error bad) "has no live binding"))))

(deftest pilot-i1-substantive-tool-without-cg-id
  (let [bad (ssb/repo-stage {:repo "futon0" :files ["scripts/cr"]})]
    (is (false? (:ok bad)))
    (is (= :Pilot-I1 (:pilot-invariant bad)))
    (is (str/includes? (:error bad) ":consent-gate-event-id"))))

(deftest repo-commit-commits-only-consent-bound-files
  (let [repo (str "futon-sweeper-test-" (System/currentTimeMillis))
        root (str "/home/joe/code/" repo)]
    (try
      (.mkdirs (io/file root))
      (sh! root "git" "init" "-q")
      (sh! root "git" "config" "user.email" "street-sweeper-test@example.invalid")
      (sh! root "git" "config" "user.name" "Street Sweeper Test")
      (spit (str root "/README.md") "base\n")
      (sh! root "git" "add" "README.md")
      (sh! root "git" "commit" "-q" "-m" "base")
      (doseq [i (range 11)]
        (spit (str root "/extra-" i ".txt") (str "extra " i "\n")))
      (spit (str root "/target.txt") "target\n")
      (sh! root "git" "add" ".")
      (let [emit (ssb/consent-gate-emit {:repo repo
                                          :intent :test
                                          :files ["target.txt"]})
            cg-id (-> emit :result :consent-gate-event-id)
            r (ssb/repo-commit {:repo repo
                                 :message "target: add file"
                                 :files ["target.txt"]
                                 :consent-gate-event-id cg-id})
            committed (-> (:out (sh! root "git" "show" "--name-only" "--format=" "HEAD"))
                          str/split-lines
                          set)
            staged (-> (:out (sh! root "git" "diff" "--staged" "--name-only"))
                       str/split-lines
                       set)]
        (is (:ok r))
        (is (= 1 (-> r :result :file-count)))
        (is (= ["target.txt"] (-> r :result :files)))
        (is (= #{"target.txt"} committed))
        (is (= (set (map #(str "extra-" % ".txt") (range 11))) staged)
            "unrelated staged files remain staged for their own packets"))
      (finally
        (delete-tree! root)))))

;; =============================================================================
;; INV-15: auto-approve checks (each independently)
;; =============================================================================

(deftest inv-15-packet-size-cap
  (testing "small packet passes"
    (let [c (ssb/classify-packet {:repo "x" :files ["a"] :diff-text "" :loc 5})]
      (is (:auto-approve? c))))
  (testing "too many files defers"
    (let [files (vec (for [i (range 15)] (str "f" i)))
          c (ssb/classify-packet {:repo "x" :files files :diff-text "" :loc 5})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :exceeded-packet-cap))))
  (testing "too much LoC defers"
    (let [c (ssb/classify-packet {:repo "x" :files ["a"] :diff-text "" :loc 500})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :exceeded-packet-cap)))))

(deftest inv-15-security-sensitive-defers
  (testing "INV-19: clear security compound in diff-text defers"
    (let [c (ssb/classify-packet {:repo "x" :files ["a.clj"]
                                  :diff-text "+ (defn authenticate-user [token] ...)"
                                  :loc 5})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :security-sensitive-diff))))
  (testing "INV-19: api-key compound in filename defers"
    (let [c (ssb/classify-packet {:repo "x" :files ["api-key-loader.clj"]
                                  :diff-text "" :loc 5})]
      (is (false? (:auto-approve? c)))))
  (testing "INV-19 narrowing: bare 'secret' mention without compound does NOT defer"
    (let [c (ssb/classify-packet {:repo "x" :files ["secret-handler.clj"]
                                  :diff-text "+ ;; this handles open secrets"
                                  :loc 5})]
      (is (:auto-approve? c)
          "INV-19 only fires on real security compounds; bare 'secret' is too loose"))))

(deftest inv-18-test-context-paths-skip-security-check
  (testing "INV-18: *-verify.mjs files pass even with security keywords"
    (let [c (ssb/classify-packet {:repo "x"
                                  :files ["web/war-machine/wm-anchor-0001-verify.mjs"]
                                  :diff-text "+ async function verify() { /* exec assertions */ }"
                                  :loc 30})]
      (is (:auto-approve? c)
          "INV-18: verification scripts are test-context — security regex skipped")))
  (testing "INV-18: files under test/ skip security check"
    (let [c (ssb/classify-packet {:repo "x"
                                  :files ["test/sweeper_test.clj"]
                                  :diff-text "+ (defn test-authenticate-with-key [])"
                                  :loc 10})]
      (is (:auto-approve? c)
          "INV-18: test/ paths are test-context"))))

(deftest inv-20-retired-both-creation-and-modification-auto-approve
  (testing "INV-20 retired: new mission doc CREATION auto-approves"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["holes/missions/E-some-excursion.md"]
              :file-statuses {"holes/missions/E-some-excursion.md" :untracked}
              :diff-text "" :loc 5})]
      (is (:auto-approve? c))))
  (testing "MODIFICATION of existing mission doc still auto-approves"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["holes/missions/M-some-mission.md"]
              :file-statuses {"holes/missions/M-some-mission.md" :modified}
              :diff-text "+ ### Checkpoint N — 2026-05-26" :loc 5})]
      (is (:auto-approve? c)))))

(deftest inv-21-additive-only-loc-cap-relaxation
  (testing "INV-21: pure-additive packet at 400 LoC auto-approves (cap=500)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["library/aif/new.flexiarg"]
              :file-statuses {"library/aif/new.flexiarg" :untracked}
              :diff-text "" :loc 400})]
      (is (:auto-approve? c))))
  (testing "mixed packet at 250 LoC defers (cap=200 for mixed)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["a.clj"]
              :file-statuses {"a.clj" :modified}
              :diff-text "" :loc 250})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :exceeded-packet-cap)))))

(deftest inv-10-package-json-is-hand-authored
  (testing "INV-10 (2026-05-26 fix): package.json is hand-authored, not regenerable"
    (let [r (ssb/apply-stage-invariants "futon4"
                                        ["dev/web/webarxana/package.json"])]
      (is (zero? (count (:proposed r)))
          "package.json filename overrides .json extension → no relocation proposal")
      (is (contains? (set (:ok r)) "dev/web/webarxana/package.json"))))
  (testing "INV-10: other .json files still flagged for relocation"
    (let [r (ssb/apply-stage-invariants "futon3a"
                                        ["resources/notions/minilm_corpus_embeddings.json"])]
      (is (= 1 (count (:proposed r)))))))

(deftest inv-23-prose-loc-cap-relaxation
  (testing "INV-23: pure-prose packet at 700 LoC auto-approves (cap=1000)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["library/aif/foo.flexiarg"
                      "library/aif/bar.flexiarg"]
              :file-statuses {"library/aif/foo.flexiarg" :modified
                              "library/aif/bar.flexiarg" :modified}
              :diff-text "" :loc 700})]
      (is (:auto-approve? c)
          "prose extensions get a 1000 LoC cap, not 200")))
  (testing "INV-23: mixed code+prose packet at 700 LoC still defers (cap=200 base)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["library/aif/foo.flexiarg"
                      "src/code.clj"]
              :file-statuses {"library/aif/foo.flexiarg" :modified
                              "src/code.clj" :modified}
              :diff-text "" :loc 700})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :exceeded-packet-cap)))))

(deftest inv-24-intent-marker-skip-in-prose
  (testing "INV-24: TODO in .md / .flexiarg packet auto-approves"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["docs/notes.md"]
              :diff-text "+ TODO: think about this later" :loc 5})]
      (is (:auto-approve? c)
          "TODO in markdown is deliberate documentation, not deferred")))
  (testing "INV-24: TODO in .clj still defers"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["src/foo.clj"]
              :diff-text "+ ;; TODO refactor this" :loc 5})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :intent-marker-in-diff)))))

(deftest inv-25-local-root-deps-auto-approve
  (testing "INV-25: deps.edn diff that adds only :local/root entries auto-approves"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["deps.edn"]
              :diff-text "+ futon2/futon2 {:local/root \"../futon2\"}\n+ futon3a {:local/root \"../futon3a\"}"
              :loc 2})]
      (is (:auto-approve? c)
          "local-root additions don't introduce external deps — safe")))
  (testing "INV-25: deps.edn diff with :mvn/version still defers"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["deps.edn"]
              :diff-text "+ org.clojure/data.json {:mvn/version \"2.5.0\"}"
              :loc 1})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :new-external-dep))))
  (testing "INV-25: Leiningen-vector form with version string still defers"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["deps.edn"]
              :diff-text "+ [reagent/reagent \"1.2.0\"]"
              :loc 1})]
      (is (false? (:auto-approve? c)))))
  (testing "INV-25: npm semver in package.json diff still defers"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["package.json"]
              :diff-text "+ \"react\": \"^18.2.0\""
              :loc 1})]
      (is (false? (:auto-approve? c))))))

(deftest inv-26-repo-policy-loc-multiplier
  (testing "INV-26: repo-policy :loc-cap-multiplier scales the base cap"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["a.clj"]
              :file-statuses {"a.clj" :modified}
              :diff-text "" :loc 350
              :repo-policy {:loc-cap-multiplier 2.0}})]
      (is (:auto-approve? c)
          "base cap 200 × policy 2.0 = 400; 350 < 400 → auto-approve"))
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["a.clj"]
              :file-statuses {"a.clj" :modified}
              :diff-text "" :loc 350
              :repo-policy nil})]
      (is (false? (:auto-approve? c))
          "no policy → base cap 200; 350 > 200 → defer"))))

(deftest inv-26-repo-policy-hand-authored-paths
  (testing "INV-26: :additional-hand-authored-paths exempt INV-10 relocation"
    (let [r (ssb/apply-stage-invariants
             "futon7a"
             ["vsatarcs.html"]
             {:additional-hand-authored-paths #{"vsatarcs.html"}})]
      (is (zero? (count (:proposed r)))
          "vsatarcs.html allowlisted by repo policy → not flagged for relocation")
      (is (contains? (set (:ok r)) "vsatarcs.html"))))
  (testing "INV-26: without policy, vsatarcs.html is still flagged"
    (let [r (ssb/apply-stage-invariants "futon7a" ["vsatarcs.html"] nil)]
      (is (= 1 (count (:proposed r)))
          "no policy → .html in generated-extensions → relocation proposal"))))

(deftest inv-28-mission-docs-free-pass-on-loc-cap
  (testing "INV-28: all-holes/-packet auto-approves regardless of LoC"
    (let [c (ssb/classify-packet
             {:repo "futon7"
              :files ["holes/M-foo.md" "holes/M-bar.md"]
              :diff-text "" :loc 9999})]
      (is (:auto-approve? c)
          "mission-doc free pass — no LoC cap when all files under holes/")))
  (testing "INV-28: mixed packet (some holes/, some src/) still caps"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["holes/M-foo.md" "src/code.clj"]
              :diff-text "" :loc 9999})]
      (is (false? (:auto-approve? c))
          "mixed packet doesn't qualify for the free pass"))))

(deftest inv-28-mission-docs-skip-cross-repo-dep-check
  (testing "INV-28: check-cross-repo-deps returns nil for all-holes/ packets"
    (is (nil? (ssb/check-cross-repo-deps "futon7"
                                         ["holes/M-some-mission.md"]
                                         {}))
        "mission-doc packet bypasses INV-14 even without explicit override")))

(deftest inv-29-per-file-loc-scaling
  (testing "INV-29: 5-file packet at 600 LoC auto-approves (cap=750)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files (vec (for [i (range 5)] (str "src/f" i ".clj")))
              :file-statuses (into {} (for [i (range 5)] [(str "src/f" i ".clj") :modified]))
              :diff-text "" :loc 600})]
      (is (:auto-approve? c)
          "5 × 150 = 750; 600 < 750 → auto-approve")))
  (testing "INV-29: 1-file packet at 1000 LoC still defers (no per-file scaling lift)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["src/foo.clj"]
              :file-statuses {"src/foo.clj" :modified}
              :diff-text "" :loc 1000})]
      (is (false? (:auto-approve? c))
          "1 × 150 = 150 < 200 base; effective cap caps at base × uniformity 1.5 = 300; 1000 > 300 → defer")))
  (testing "INV-29: megapack still defers even with high file-count"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files (vec (for [i (range 10)] (str "src/f" i ".clj")))
              :file-statuses (into {} (for [i (range 10)] [(str "src/f" i ".clj") :modified]))
              :diff-text "" :loc 3000})]
      (is (false? (:auto-approve? c))
          "10 × 150 = 1500; 3000 > 1500 → defer (genuine megapack)"))))

(deftest inv-27-prose-skips-security-check
  (testing "INV-27: security keyword in prose-only packet diff does NOT defer"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["docs/auth-notes.md"]
              :diff-text "+ This pattern uses authenticate() to verify the token"
              :loc 5})]
      (is (:auto-approve? c)
          "markdown mentioning auth keywords is documentation, not auth code")))
  (testing "INV-27: security keyword in code-file diff STILL defers"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["src/auth.clj"]
              :diff-text "+ (defn authenticate [token] ...)"
              :loc 5})]
      (is (false? (:auto-approve? c)))
      (is (contains? (set (:defer-reasons c)) :security-sensitive-diff))))
  (testing "INV-27: mixed prose+code packet still applies the check"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["docs/notes.md" "src/auth.clj"]
              :diff-text "+ authenticate function added"
              :loc 5})]
      (is (false? (:auto-approve? c))
          "mixed-extension packets don't qualify for prose exemption"))))

(deftest inv-22-uniform-packet-bonus
  (testing "INV-22: uniform packet (same ext + shared prefix) at 250 LoC auto-approves
                   (uniform = 1.5× cap)"
    (let [c (ssb/classify-packet
             {:repo "x"
              :files ["library/aif/a.flexiarg"
                      "library/aif/b.flexiarg"
                      "library/aif/c.flexiarg"]
              :file-statuses {"library/aif/a.flexiarg" :modified
                              "library/aif/b.flexiarg" :modified
                              "library/aif/c.flexiarg" :modified}
              :diff-text "" :loc 280})]
      (is (:auto-approve? c)
          "INV-22: uniform packet gets 200 × 1.5 = 300 LoC cap"))))

(deftest inv-15-new-external-dep-defers
  (let [c (ssb/classify-packet {:repo "x" :files ["deps.edn"]
                                :diff-text "+ [some/dep \"1.0\"]" :loc 5})]
    (is (false? (:auto-approve? c)))
    (is (contains? (set (:defer-reasons c)) :new-external-dep))))

(deftest inv-15-todo-marker-defers
  (let [c (ssb/classify-packet {:repo "x" :files ["foo.clj"]
                                :diff-text "+ ;; TODO refine this" :loc 5})]
    (is (false? (:auto-approve? c)))
    (is (contains? (set (:defer-reasons c)) :intent-marker-in-diff))))

(deftest inv-20-retired-mission-doc-always-auto-approves
  (testing "INV-20 retired 2026-05-26: mission doc CREATION no longer defers"
    (let [c (ssb/classify-packet {:repo "x"
                                  :files ["holes/missions/E-some-excursion.md"]
                                  :diff-text "" :loc 5})]
      (is (:auto-approve? c)
          "intent-doc-creation defer retired; mission docs auto-approve")
      (is (not (contains? (set (:defer-reasons c)) :intent-doc-creation))))))

(deftest inv-15-clean-packet-auto-approves
  (let [c (ssb/classify-packet {:repo "x"
                                :files ["library/aif/foo.flexiarg"
                                        "library/aif/bar.flexiarg"]
                                :diff-text "+ ;; some pattern content"
                                :loc 20})]
    (is (:auto-approve? c))
    (is (empty? (:failed-checks c)))))

;; =============================================================================
;; build-packets clustering
;; =============================================================================

(deftest build-packets-small-set
  (testing "≤ max-size files = single packet"
    (let [packets (ss/build-packets ["a/x" "a/y" "b/z"] 10)]
      (is (= 1 (count packets)))
      (is (= 3 (count (:files (first packets))))))))

(deftest build-packets-splits-by-top-level-dir
  (testing "files in distinct top-level dirs split per dir"
    (let [files (concat (for [i (range 12)] (str "dirA/" i))
                        (for [i (range 8)] (str "dirB/" i)))
          packets (ss/build-packets files 10)]
      (is (every? #(<= (count (:files %)) 10) packets))
      (is (= 20 (apply + (map (comp count :files) packets)))))))

(deftest build-packets-recurses-into-large-dir
  (testing "single top-level dir with > max-size files recurses"
    (let [files (concat (for [i (range 12)] (str "deep/x/" i))
                        (for [i (range 5)] (str "deep/y/" i)))
          packets (ss/build-packets files 10)]
      (is (every? #(<= (count (:files %)) 10) packets))
      (is (= 17 (apply + (map (comp count :files) packets)))))))

(deftest build-packets-root-files-each-own-packet
  (testing "files at root (no leading dir) each become single-file packets"
    (let [files (concat ["README.md" "CLAUDE.md" "AGENTS.md"]
                        (for [i (range 12)] (str "src/" i)))
          packets (ss/build-packets files 10)]
      ;; 3 root files = 3 single-file packets
      ;; 12 src/ files = 2 packets (10 + 2 OR similar split)
      (is (every? #(<= (count (:files %)) 10) packets))
      (is (= 15 (apply + (map (comp count :files) packets)))))))

;; =============================================================================
;; INV-17 reflection: candidate clustering
;; =============================================================================

(deftest inv-17-write-defer-manifest-and-cluster
  (testing "defer manifest writes + candidate-invariants when cluster ≥ 3"
    (let [ts (str "test-" (System/currentTimeMillis))
          packets (concat (for [i (range 4)]
                            {:repo "x" :files [(str "f" i)]
                             :defer-reason :security-sensitive-diff
                             :diff-text "+ test"})
                          (for [i (range 2)]
                            {:repo "x" :files [(str "g" i)]
                             :defer-reason :intent-doc-creation
                             :diff-text "+ test"}))
          r (ssb/write-defer-manifest {:sweep-ts ts :deferred-packets packets})
          manifest-path (-> r :result :manifest-path)
          root (subs manifest-path 0 (.lastIndexOf manifest-path "/"))
          candidate-path (str root "/candidate-invariants.edn")]
      (is (:ok r))
      (is (.exists (java.io.File. manifest-path)))
      (testing "cluster ≥ 3 (security-sensitive-diff = 4) generates candidate"
        (is (.exists (java.io.File. candidate-path))))
      ;; cleanup
      (clojure.java.io/delete-file candidate-path true)
      (clojure.java.io/delete-file manifest-path true)
      (clojure.java.io/delete-file (java.io.File. root) true))))

;; =============================================================================
;; Orchestration smoke
;; =============================================================================

(deftest run-full-sweep-empty-repos-list
  (testing "explicit empty repo list = no-op clean structure"
    (let [r (ss/run-full-sweep {:dry-run? true :repos []})]
      (is (zero? (count (:commits-landed r))))
      (is (zero? (count (:would-commit r))))
      (is (zero? (count (:deferred-packets r))))
      (is (zero? (count (:errors r))))
      (is (true? (:dry-run? r))))))

(deftest run-full-sweep-summary-shape
  (testing "sweep-summary preserves the keys"
    (let [r (ss/run-full-sweep {:dry-run? true :repos []})
          s (ss/sweep-summary r)]
      (is (contains? s :sweep-ts))
      (is (contains? s :dry-run?))
      (is (contains? s :commits-landed-count))
      (is (contains? s :would-commit-count))
      (is (contains? s :deferred-count))
      (is (contains? s :defer-reason-frequencies)))))
