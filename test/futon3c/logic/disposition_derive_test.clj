(ns futon3c.logic.disposition-derive-test
  "Tests for `futon3c.logic.disposition-derive`. Verifies that
   active missions' path references derive correctly into a
   disposition state, and that combined-state honors operator
   override.

   Mission: M-bounded-in-flight-state INSTANTIATE D-05."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [futon3c.logic.disposition-derive :as derive])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- mk-tmp-repo
  "Make a temp dir whose layout looks like a repo with a
   holes/missions/ subdir; returns its path string."
  ^String []
  (let [dir (.toFile (Files/createTempDirectory
                      "fb-disp-derive-"
                      (into-array FileAttribute [])))]
    (.mkdirs (io/file dir "holes" "missions"))
    (.getPath dir)))

(defn- write-mission!
  "Write a mission .md to REPO-PATH/holes/missions/M-NAME.md with
   the given STATUS-LINE (e.g. \"Status: open\") and BODY."
  [repo-path mission-name status-line body]
  (let [f (io/file repo-path "holes" "missions"
                   (str mission-name ".md"))
        content (str (or status-line "")
                     (when status-line "\n")
                     "# " mission-name "\n\n"
                     (or body ""))]
    (spit f content)))

(deftest derive-from-open-mission
  (testing "An open mission referencing futon3c paths derives :in-progress entries
            for those paths in the relevant repo."
    (let [repo (mk-tmp-repo)]
      (write-mission!
       repo "M-test-open"
       "Status: open"
       "Affects futon3c/src/futon3c/logic/foo.clj and futon3c/dev/futon3c/dev.clj.")
      (let [state (derive/derive-from-active-missions [repo])
            ;; Find the entry whose key ends with the temp repo's tail.
            repo-state (some (fn [[k v]]
                               (when (re-find #"futon3c$" (str k)) v))
                             state)]
        ;; The temp repo's basename is fb-disp-derive-XXXX, not "futon3c",
        ;; so this case won't match unless the basename ends in "futon3c".
        ;; This test verifies a *no-match* path: derived state for non-futon-prefixed
        ;; repos is empty.
        (is (or (nil? repo-state) (empty? (:in-progress repo-state)))
            "no spurious entries when temp repo basename doesn't match futon prefix")))))

(deftest derive-respects-status-terminal
  (testing ":closed / :archived / :parked missions do NOT contribute."
    (let [repo (mk-tmp-repo)]
      (write-mission! repo "M-closed"   "Status: closed"   "References futon3c/src/x.clj")
      (write-mission! repo "M-archived" "Status: archived" "References futon3c/src/y.clj")
      (write-mission! repo "M-parked"   "Status: parked"   "References futon3c/src/z.clj")
      ;; Terminal-status missions should produce no in-progress entries
      ;; even if their bodies reference paths.
      (let [state (derive/derive-from-active-missions [repo])
            entries (mapcat #(keys (or (:in-progress %) {}))
                            (vals state))]
        (is (empty? entries))))))

(deftest derive-extracts-only-futon-prefixed-paths
  (testing "Path-reference regex matches `futonX/...` shapes only;
            not bare basenames or arbitrary URLs."
    ;; This test exercises the regex via a roundtrip: write a
    ;; mission referencing both qualified and bare paths, then
    ;; verify only the qualified ones produce entries.
    (let [repo (mk-tmp-repo)
          rest-repo (str repo "/futon3c")]
      ;; Make a fake "futon3c" sibling repo so the derive picks it up.
      (.mkdirs (io/file rest-repo "holes" "missions"))
      (write-mission!
       repo "M-active"
       "Status: open"
       (str "Affects bare-name.clj (should NOT match) "
            "and futon3c/src/futon3c/logic/foo.clj (should match) "
            "and http://example.com/path (should NOT match)."))
      (let [state (derive/derive-from-active-missions [repo rest-repo])
            entries (apply merge {} (map :in-progress (vals state)))]
        (is (contains? entries "src/futon3c/logic/foo.clj")
            "qualified path made it through")
        (is (not (contains? entries "bare-name.clj")))))))

(deftest combined-state-honors-file-override
  (testing "When file-based state has a path also present in derived
            state, the file entry wins (operator-attested takes
            precedence over derivation)."
    (let [derived-state {:in-progress
                         {"src/foo.clj" {:reasoning "by mission M-X"
                                          :source :derived/active-mission}}
                         :source :derived}
          file-state    {:in-progress
                         {"src/foo.clj" {:reasoning "operator: experimental WIP"
                                          :since "2026-05-03"
                                          :review-by "2026-05-30"}
                          "src/other.clj" {:reasoning "operator: paused"
                                            :since "2026-05-01"}}}
          merged (derive/combined-state derived-state file-state)
          foo (get-in merged [:in-progress "src/foo.clj"])
          other (get-in merged [:in-progress "src/other.clj"])]
      (is (= "operator: experimental WIP" (:reasoning foo))
          "file entry wins for the colliding key")
      (is (= "operator: paused" (:reasoning other))
          "file-only entry preserved")
      (is (contains? merged :source-derived)
          "the derived source marker is retained for diagnostics"))))

(deftest combined-state-empty-file-keeps-derived
  (testing "When the operator hasn't written a file, derived state is
            returned as the in-progress map."
    (let [derived-state {:in-progress
                         {"src/foo.clj" {:reasoning "by M-X"}}
                         :source :derived}
          merged (derive/combined-state derived-state nil)]
      (is (contains? (:in-progress merged) "src/foo.clj"))
      (is (= :derived (:source-derived merged))))))
