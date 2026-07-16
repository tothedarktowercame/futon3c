(ns futon3c.logic.locus-test
  "Tests for `futon3c.logic.locus` — single-locus check-fn factories.

   Mission: M-single-locus (futon3c/holes/missions/)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [futon3c.evidence.backend :as backend]
            [futon3c.logic.locus :as locus]
            [futon3c.logic.probe :as probe]))

(def ^:dynamic *evidence-backend* nil)

(use-fixtures
  :each
  (fn [f]
    (binding [*evidence-backend*
              (backend/->AtomBackend (atom {:entries {} :order []}))]
      (f))))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- write-file! [^java.io.File dir relative-path content]
  (let [f (io/file dir relative-path)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (.getPath f)))

(defn- write-mission-file! [^java.io.File dir basename content]
  (.mkdirs dir)
  (let [f (io/file dir basename)]
    (spit f content)
    (.getPath f)))

(defn- make-temp-repo! [prefix]
  (let [tmp-repo (java.io.File/createTempFile prefix "")]
    (.delete tmp-repo)
    (.mkdirs tmp-repo)
    (.mkdirs (io/file tmp-repo ".git"))
    (.deleteOnExit tmp-repo)
    tmp-repo))

;; -----------------------------------------------------------------------------
;; parse-home-repo-annotations
;; -----------------------------------------------------------------------------

(deftest parse-extracts-explicit-annotation
  (testing "Home-repo: tag in first 10 lines is captured"
    (let [tmp (java.io.File/createTempFile "M-test-" ".md")
          _ (.deleteOnExit tmp)
          _ (spit tmp "**Status:** active\n**Home-repo:** futon3c\n\n# Body\n")
          annotations (locus/parse-home-repo-annotations (.getPath tmp))]
      (is (= ["futon3c"] annotations))
      (.delete tmp))))

(deftest parse-case-insensitive
  (testing "case-insensitive match for tag and value"
    (let [tmp (java.io.File/createTempFile "M-test-ci-" ".md")
          _ (.deleteOnExit tmp)
          _ (spit tmp "**HOME-REPO:** Futon4\nbody\n")]
      (is (= ["futon4"] (locus/parse-home-repo-annotations (.getPath tmp))))
      (.delete tmp))))

(deftest parse-missing-tag-empty
  (testing "no Home-repo line → empty vector"
    (let [tmp (java.io.File/createTempFile "M-test-noattr-" ".md")
          _ (.deleteOnExit tmp)
          _ (spit tmp "**Status:** active\n# Body without annotation\n")]
      (is (= [] (locus/parse-home-repo-annotations (.getPath tmp))))
      (.delete tmp))))

(deftest parse-multiple-annotations-collected
  (testing "two Home-repo lines yield a two-element vector"
    (let [tmp (java.io.File/createTempFile "M-test-multi-" ".md")
          _ (.deleteOnExit tmp)
          _ (spit tmp (str "**Status:** active\n"
                           "**Home-repo:** futon3c\n"
                           "**Home-repo:** futon4\n"
                           "# body\n"))]
      (is (= ["futon3c" "futon4"]
             (locus/parse-home-repo-annotations (.getPath tmp))))
      (.delete tmp))))

(deftest parse-only-first-10-lines-scanned
  (testing "annotation past line 10 is ignored"
    (let [tmp (java.io.File/createTempFile "M-test-late-" ".md")
          _ (.deleteOnExit tmp)
          _ (spit tmp (str (apply str (repeat 12 "padding\n"))
                           "**Home-repo:** futon4\n"))]
      (is (= [] (locus/parse-home-repo-annotations (.getPath tmp))))
      (.delete tmp))))

;; -----------------------------------------------------------------------------
;; check-mission-home-locus
;; -----------------------------------------------------------------------------

(deftest mission-home-empty-repo-list-is-ok
  (testing "no repos → :ok"
    (let [check (locus/check-mission-home-locus [])
          r (check *evidence-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :total-missions]))))))

(deftest mission-home-real-stack-is-ok
  (testing "live futon3c repo: many missions, none with contradictory annotations"
    (let [check (locus/check-mission-home-locus
                 ["/home/joe/code/futon3c"])
          r (check *evidence-backend*)]
      (is (= :ok (:outcome r))
          (str "expected :ok against live futon3c, got "
               (pr-str (select-keys (:detail r)
                                    [:total-missions :explicit-home-count
                                     :implicit-home-count])))))))

(deftest mission-home-detects-explicit-and-implicit
  (testing "explicit-home-count >= 1 once we annotate, implicit fills the rest"
    (let [check (locus/check-mission-home-locus
                 ["/home/joe/code/futon3c"])
          r (check *evidence-backend*)
          d (:detail r)]
      (is (= :ok (:outcome r)))
      (is (>= (:explicit-home-count d) 1)
          "at least one mission carries Home-repo: annotation now"))))

(deftest mission-home-flags-contradiction-in-tmp
  (testing "synthesized missions dir with two distinct annotations → :violation"
    (let [tmp-repo (java.io.File/createTempFile "fake-repo-" "")
          _ (.delete tmp-repo)
          _ (.mkdirs tmp-repo)
          _ (.mkdirs (io/file tmp-repo ".git"))
          missions-dir (io/file tmp-repo "holes" "missions")
          _ (write-mission-file!
             missions-dir "M-bad.md"
             "**Status:** open\n**Home-repo:** futon3c\n**Home-repo:** futon4\n# body\n")
          check (locus/check-mission-home-locus [(.getPath tmp-repo)])
          r (check *evidence-backend*)]
      (is (= :violation (:outcome r))
          (str "expected violation, got " (pr-str r)))
      (is (= 1 (count (get-in r [:detail :violations]))))
      (is (= ["futon3c" "futon4"]
             (-> (get-in r [:detail :violations])
                 first :annotations))))))

;; -----------------------------------------------------------------------------
;; check-agent-routing-locus
;; -----------------------------------------------------------------------------

(deftest agent-routing-empty-registry-is-ok
  (let [check (locus/check-agent-routing-locus (atom {}))
        r (check *evidence-backend*)]
    (is (= :ok (:outcome r)))
    (is (= 0 (get-in r [:detail :total-agent-records])))))

(deftest agent-routing-single-entry-is-ok
  (let [check (locus/check-agent-routing-locus
               (atom {"codex-1" {:agent/id {:id/value "codex-1"
                                           :id/type :continuity}
                                 :agent/type :codex
                                 :agent/session-id "s-1"}}))
        r (check *evidence-backend*)]
    (is (= :ok (:outcome r)))
    (is (= 1 (get-in r [:detail :scanned-identities])))))

(deftest agent-routing-detects-duplicate
  (let [check (locus/check-agent-routing-locus
               (atom {"registry-key-1" {:agent/id {:id/value "codex-1"
                                                  :id/type :continuity}
                                        :agent/session-id "s-1"}
                      "registry-key-2" {:agent/id {:id/value "codex-1"
                                                  :id/type :continuity}
                                        :agent/session-id "s-2"}}))
        r (check *evidence-backend*)]
    (is (= :violation (:outcome r))
        (str "expected violation, got " (pr-str r)))
    (is (= 1 (count (get-in r [:detail :violations]))))
    (is (= "codex-1"
           (-> (get-in r [:detail :violations])
               first :identity)))))

;; -----------------------------------------------------------------------------
;; check-artifact-live-copy-locus
;; -----------------------------------------------------------------------------

(deftest artifact-live-copy-empty-repos-is-ok
  (let [check (locus/check-artifact-live-copy-locus [])
        r (check *evidence-backend*)]
    (is (= :ok (:outcome r)))
    (is (= 0 (get-in r [:detail :total-artifacts])))))

(deftest artifact-live-copy-traversal-is-bounded-to-supported-glob-roots
  (let [repo (make-temp-repo! "artifact-bounded-roots-")
        _ (write-file! repo "library/example.flexiarg" "! conclusion: example\n")
        _ (write-file! repo "scripts/example.clj" "(println :example)\n")
        _ (write-file! repo "unrelated/large-tree/file.txt" "not an artifact\n")
        traversed-roots (atom [])]
    (with-redefs [clojure.core/file-seq
                  (fn [root]
                    (swap! traversed-roots conj (.getCanonicalPath root))
                    [])]
      (let [check (locus/check-artifact-live-copy-locus [(.getPath repo)])]
        (check *evidence-backend*)))
    (is (= #{(.getCanonicalPath (io/file repo "library"))
             (.getCanonicalPath (io/file repo "scripts"))}
           (set @traversed-roots)))))

(deftest artifact-live-copy-detects-multi-repo-path
  (testing "same relative-path in two repos without canonical marker → violation"
    (let [repo-a (make-temp-repo! "artifact-repo-a-")
          repo-b (make-temp-repo! "artifact-repo-b-")
          _ (write-file! repo-a "scripts/shared-tool.clj" "(println :a)\n")
          _ (write-file! repo-b "scripts/shared-tool.clj" "(println :b)\n")
          check (locus/check-artifact-live-copy-locus
                 [(.getPath repo-a) (.getPath repo-b)])
          r (check *evidence-backend*)]
      (is (= :violation (:outcome r))
          (str "expected violation, got " (pr-str r)))
      (is (= ["scripts/shared-tool.clj"]
             (mapv :identity (get-in r [:detail :violations]))))))
  (testing "same basename at different relative-paths → not flagged"
    (let [repo-a (make-temp-repo! "artifact-repo-e-")
          repo-b (make-temp-repo! "artifact-repo-f-")
          _ (write-file! repo-a "library/coordination/ARGUMENT.flexiarg"
                         "! conclusion: a\n")
          _ (write-file! repo-b "library/social/ARGUMENT.flexiarg"
                         "! conclusion: b\n")
          check (locus/check-artifact-live-copy-locus
                 [(.getPath repo-a) (.getPath repo-b)])
          r (check *evidence-backend*)]
      (is (= :ok (:outcome r))
          (str "expected ok, got " (pr-str r)))))
  (testing "canonical-repo marker on one matching file suppresses violation"
    (let [repo-a (make-temp-repo! "artifact-repo-c-")
          repo-b (make-temp-repo! "artifact-repo-d-")
          repo-a-name (.getName repo-a)
          _ (write-file! repo-a "scripts/shared-tool.clj"
                         (str ";; canonical-repo: " repo-a-name "\n(println :a)\n"))
          _ (write-file! repo-b "scripts/shared-tool.clj" "(println :b)\n")
          check (locus/check-artifact-live-copy-locus
                 [(.getPath repo-a) (.getPath repo-b)])
          r (check *evidence-backend*)]
      (is (= :ok (:outcome r))
          (str "expected ok, got " (pr-str r)))))) 

;; -----------------------------------------------------------------------------
;; Canonical statement is grep-verifiable
;; -----------------------------------------------------------------------------

(deftest canonical-statement-grep-verifiable
  (is (string? locus/I-single-locus))
  (is (re-find #"I-single-locus" locus/I-single-locus)))

;; -----------------------------------------------------------------------------
;; on-load wrappers
;; -----------------------------------------------------------------------------

(deftest mission-home-on-load-emits-family-fired-evidence
  (testing "mission-home on-load fires family-fired with proper outcome and emit-receipt"
    (let [r (locus/check-mission-home-locus-on-load!
             *evidence-backend*
             {:print? false :repo-paths ["/home/joe/code/futon3c"]})]
      (is (#{:ok :violation :inactive} (:outcome r))
          (str "expected one of #{:ok :violation :inactive}, got "
               (pr-str (:outcome r))))
      (is (:emit-receipt r))
      (is (:ok (:emit-receipt r))))))

(deftest mission-home-on-load-respects-emit-flag
  (testing "mission-home emit? false → no emit-receipt"
    (let [r (locus/check-mission-home-locus-on-load!
             *evidence-backend*
             {:print? false :emit? false
              :repo-paths ["/home/joe/code/futon3c"]})]
      (is (nil? (:emit-receipt r))))))

(deftest agent-routing-on-load-emits-family-fired-evidence
  (let [r (locus/check-agent-routing-locus-on-load!
           *evidence-backend*
           {:print? false
            :state-source (atom {})})]
    (is (#{:ok :violation :inactive} (:outcome r)))
    (is (:emit-receipt r))
    (is (:ok (:emit-receipt r)))))

(deftest agent-routing-on-load-respects-emit-flag
  (let [r (locus/check-agent-routing-locus-on-load!
           *evidence-backend*
           {:print? false
            :emit? false
            :state-source (atom {})})]
    (is (nil? (:emit-receipt r)))))

(deftest artifact-live-copy-on-load-emits-family-fired-evidence
  (let [repo-a (make-temp-repo! "artifact-load-a-")
        repo-b (make-temp-repo! "artifact-load-b-")
        _ (write-file! repo-a "library/example.flexiarg" "! conclusion: a\n")
        _ (write-file! repo-b "library/other.flexiarg" "! conclusion: b\n")
        r (locus/check-artifact-live-copy-locus-on-load!
           *evidence-backend*
           {:print? false
            :repo-paths [(.getPath repo-a) (.getPath repo-b)]})]
    (is (#{:ok :violation :inactive} (:outcome r)))
    (is (:emit-receipt r))
    (is (:ok (:emit-receipt r)))))

(deftest artifact-live-copy-on-load-respects-emit-flag
  (let [repo-a (make-temp-repo! "artifact-load-c-")
        repo-b (make-temp-repo! "artifact-load-d-")
        _ (write-file! repo-a "scripts/left.clj" "(println :left)\n")
        _ (write-file! repo-b "scripts/right.clj" "(println :right)\n")
        r (locus/check-artifact-live-copy-locus-on-load!
           *evidence-backend*
           {:print? false
            :emit? false
            :repo-paths [(.getPath repo-a) (.getPath repo-b)]})]
    (is (nil? (:emit-receipt r)))))

;; -----------------------------------------------------------------------------
;; register-locus-taps!
;; -----------------------------------------------------------------------------

(deftest register-installs-three-locus-taps
  (testing "registrar installs all three single-locus taps"
    (let [old @probe/family-check-fns]
      (try
        (reset! probe/family-check-fns {})
        (let [registered (locus/register-locus-taps!
                          {:repo-paths []
                           :state-source (atom {})})]
          (is (= #{:single-locus/mission-home
                   :single-locus/agent-routing
                   :single-locus/artifact-live-copy}
                 registered))
          (is (= registered (set (keys @probe/family-check-fns)))))
        (finally
          (reset! probe/family-check-fns old))))))
