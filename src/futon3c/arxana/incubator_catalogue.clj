(ns futon3c.arxana.incubator-catalogue
  "Offline Demo A records and isolated ERT probes for the incubator."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.design-catalogue :as design]
            [futon3c.arxana.together-catalogue :as together-catalogue]))

(def ^:private main-test "test/futon4a-incubator-test.el")

(def features
  [{:feature :edit-is-one-act :kind :ruling
    :statement "Committing an edit adds one edit act with its text and provenance; an unchanged commit or a cancel adds none."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator.el" :section "futon4a-incubator-edit, futon4a-incubator-edit-commit, futon4a-incubator-edit-cancel"
    :test-path main-test :tests ["incubator-edit-flows-back-to-the-slot"]}
   {:feature :layers-on-a-main-line :kind :ruling
    :statement "Versions are layers on a main line; a layer shows the acts of its lineage only."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator-layers.el" :section "futon4a-incubator--lineage, futon4a-incubator--layer-view"
    :test-path main-test :tests ["incubator-revision-layer-above-the-call" "incubator-proposals-sit-above-joes-pass"]}
   {:feature :forks-invisible-from-head :kind :ruling
    :statement "A fork branches from a layer; its acts never show on the head of the main line."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator-layers.el" :section "futon4a-incubator--fork-p, futon4a-incubator--lineage, futon4a-incubator--layer-view"
    :test-path main-test :tests ["incubator-murch-layer-is-a-teammates-not-an-agents"]}
   {:feature :proposals-are-not-the-answer :kind :constraint
    :statement "An edit on a proposal layer or a fork is not counted as Joe's answer unless that layer is shown."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator.el" :section "futon4a-incubator--latest-edit, futon4a-incubator--slot-text"
    :test-path main-test :tests ["incubator-proposals-sit-above-joes-pass" "incubator-murch-layer-is-a-teammates-not-an-agents"]}
   {:feature :proposer-is-never-joe :kind :constraint
    :statement "Proposing a revision as Joe, or with no actor, is refused."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator-layers.el" :section "futon4a-incubator-propose-revision"}
   {:feature :editorial-save :kind :decision
    :statement "In an editorial pass, saving writes one edit act by Joe per changed answer; the placeholder of an unanswered question is not saved; leaving without saving writes nothing."
    :repo "futon4a" :revision "8d8b09ebbed0dc3f8b71d9f525c15bf25f2ad456" :path "lisp/futon4a-incubator-editorial.el" :section "futon4a-incubator-editorial-save, futon4a-incubator-editorial-abandon"
    :test-path main-test :tests ["incubator-editorial-pass-tracks-typing-acceptance-and-cut-and-paste"]}
   {:feature :character-provenance :kind :decision
    :statement "On save each character is classed typed, accepted, agent, kept or moved; text pasted from outside counts as typed."
    :repo "futon4a" :revision "8d8b09ebbed0dc3f8b71d9f525c15bf25f2ad456" :path "lisp/futon4a-incubator-editorial.el" :section "futon4a-incubator--editorial-classify, futon4a-incubator--editorial-segments"
    :test-path main-test :tests ["incubator-editorial-pass-tracks-typing-acceptance-and-cut-and-paste"]}
   {:feature :sources-in-colour :kind :ruling
    :statement "Each source has its own face; agent text is bracketed, a teammate's is not."
    :repo "futon4a" :revision "4f6e80ac3738050c5a4bd5ee9901a26fd816118a" :path "lisp/futon4a-incubator-provenance.el" :section "futon4a-incubator--teammate-face, futon4a-incubator--insert-provenance"
    :test-path main-test :tests ["incubator-revision-provenance-colours-brackets-and-hud" "incubator-expansion-segments-carry-their-own-source" "incubator-ret-edit-colours-only-what-changed"]}
   {:feature :layer-view-is-read-only :kind :constraint
    :statement "Viewing a layer or fork renders a copy; the form's session, act count and layer are unchanged."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator-layers.el" :section "futon4a-incubator-snapshot"
    :test-path main-test :tests ["incubator-snapshot-shows-a-fork-without-touching-the-form"]}
   {:feature :copy-keeps-source :kind :decision
    :statement "Text copied from a teammate's view and pasted in an editorial pass is saved as accepted from that teammate."
    :repo "futon4a" :revision "8d8b09ebbed0dc3f8b71d9f525c15bf25f2ad456" :path "lisp/futon4a-incubator-editorial.el" :section "futon4a-incubator--editorial-segments, futon4a-incubator-editorial-save"
    :test-path main-test :tests ["incubator-copy-from-murchs-view-keeps-his-colour"]}
   {:feature :numbered-versions :kind :ruling
    :statement "When the session's content changes, an idle timer writes it as a numbered version named by its hash; a state seen before keeps its old number."
    :repo "futon4a" :revision "83a1934e7294e2f7569ae1a3c67d9d693ebf250b" :path "lisp/futon4a-incubator-autosave.el" :section "futon4a-incubator--autosave-write, futon4a-incubator--autosave-tick"
    :test-path main-test :tests ["incubator-autosave-numbers-versions-and-sees-in-place-edits" "incubator-autosave-round-trip"]}
   {:feature :anchors-by-words :kind :ruling
    :statement "A mark is located by its words and their context: a unique exact match, else a unique best context match, else nowhere; never by file and line."
    :repo "futon4a" :revision "c2915467c6c30885d778bcb00ca93b88c21faf34" :path "lisp/futon4a-incubator-holes.el" :section "futon4a-incubator-holes--anchor, futon4a-incubator-holes--resolve"
    :test-path "test/futon4a-incubator-holes-test.el" :tests ["futon4a-incubator-holes-anchors-survive-edits-and-moves" "futon4a-incubator-holes-never-marks-the-wrong-word"]}
   {:feature :fuzzy-match-proposes :kind :ruling
    :statement "A fuzzy match proposes a derivation and never decides one; a speaker passes only through a confirmed derivation."
    :repo "futon4a" :revision "24c20149c4ebe96e3955ae47e3381043d858609b" :path "lisp/futon4a-clip-derivation.el" :section "futon4a-clip-add-edge, futon4a-clip-decide-edge, futon4a-clip--speaker"
    :test-path "test/futon4a-call-notes-test.el" :tests ["clip-derivation-paraphrase-merge-reject-and-cycle"]}
   {:feature :first-cut-once :kind :ruling
    :statement "An over-limit answer is cut once automatically, and never again after anyone has cut or restored."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator.el" :section "futon4a-incubator--first-cuts, futon4a-incubator-toggle-cut"
    :test-path main-test :tests ["incubator-first-cut-strikes-to-fit-and-x-restores"]}
   {:feature :load-refused-whole :kind :constraint
    :statement "A turn-clip load with any text mismatch is refused whole and leaves the session unchanged."
    :repo "futon4a" :revision "610d4985a22b2c1473ff349fb6665c0a56c9db23" :path "lisp/futon4a-incubator-turns.el" :section "futon4a-incubator-turns--mismatches, futon4a-incubator-load-turn-clips"
    :test-path "test/futon4a-incubator-turns-test.el" :tests ["turn-clips-mismatch-refuses-whole-load"]}
   {:feature :export-is-explicit :kind :constraint
    :statement "Export goes only to a file the user chooses, and quitting with unsaved acts asks first."
    :repo "futon4a" :revision "b317cc976b77ebfb0081627c8638e00edecd7268" :path "lisp/futon4a-incubator.el" :section "futon4a-incubator-export, futon4a-incubator-quit"
    :test-path main-test :tests ["incubator-explicit-export-and-unsaved-warning"]}
   {:feature :tests-do-not-persist :kind :constraint
    :statement "The test harness fails any store request or file write, and autosave never starts in batch."
    :repo "futon4a" :revision "274acce071c1766a2ecb9026150ba9a58d2a1f09" :path "test/futon4a-incubator-test.el" :section "with-incubator, incubator-test-harness-refuses-writes-and-store"
    :test-path main-test :tests ["incubator-test-harness-refuses-writes-and-store"]}
   {:feature :shared-answer-saves-as-usual :kind :decision
    :statement "A shared answer, once detached, is saved by the ordinary editorial save with the same provenance classes."
    :repo "futon4a" :revision "6b6c71579c8569ae986eda3cf90a8445a3475bab" :path "lisp/futon4a-incubator-together.el" :section "futon4a-incubator-together-detach, futon4a-incubator-together-save"
    :test-path "test/futon4a-incubator-together-test.el" :tests ["incubator-together-seed-and-save-attribution"]}])

(defn- tail [text] (apply str (take-last 800 text)))

(defn- selector [names]
  (str "\\`\\(" (str/join "\\|" names) "\\)\\'"))

(defn- eval-form [names]
  (str "(progn (dolist (name '(" (str/join " " names)
       ")) (unless (ert-test-boundp name) (princ (format \"MISSING:%s\\n\" name)))) "
       "(ert-run-tests-batch-and-exit " (pr-str (selector names)) "))"))

(defn probe [feature]
  (try
    (let [{:keys [test-path tests]} (some #(when (= feature (:feature %)) %) features)]
      (when-not (seq tests)
        (throw (ex-info "Feature has no executable check" {:feature feature})))
      (let [futon3c (io/file (System/getProperty "user.dir"))
            futon4a (io/file futon3c ".." "futon4a")
            {:keys [exit out]}
            (together-catalogue/run-process
             futon4a ["emacs" "-Q" "--batch" "-L" (str (io/file futon3c "emacs"))
                      "-L" "lisp" "-L" "test" "-l" test-path "--eval" (eval-form tests)])
            missing (mapv second (re-seq #"(?m)^MISSING:([^\n]+)$" out))
            [_ ran expected] (re-find #"Ran (\d+) tests?, (\d+) results? as expected" out)
            ran (some-> ran parse-long)
            expected (some-> expected parse-long)
            violations (cond-> []
                         (seq missing) (conj {:reason :missing-tests :tests missing})
                         (not (zero? exit)) (conj {:reason :ert-exit :exit exit :output (tail out)})
                         (not= (count tests) ran) (conj {:reason :wrong-test-count
                                                        :expected (count tests) :actual ran
                                                        :missing (vec (remove (set missing) tests))})
                         (not= ran expected) (conj {:reason :unexpected-results :ran ran :expected expected}))]
        {:result (if (seq violations) :violation :pass) :violations violations}))
    (catch Exception e
      {:result :error :violations [{:reason :probe-exception :message (ex-message e)}]})))

(defn build [{:keys [pins at implementation-digest]}]
  (let [groups
        (mapv
         (fn [{:keys [feature kind statement tests] :as specification}]
           (let [choice (design/choice
                         {:choice/key (keyword "incubator" (name feature)) :kind kind
                          :statement statement
                          :sources [(select-keys specification [:repo :revision :path :section])]
                          :lifecycle :accepted :applies-to [feature]
                          :rationale "Expose the incubator editing contract and its isolated ERT evidence in Demo A."
                          :alternatives []})
                 check (when (seq tests)
                         (design/check-record
                          {:check/key (keyword "incubator-ert" (name feature))
                           :runtime :clojure
                           :entry-point "futon3c.arxana.incubator-catalogue/probe"
                           :input-contract {:feature feature :checkout :futon4a :tests tests}
                           :output-contract {:result [:pass :violation :error] :coverage :partial}
                           :implementation-digest implementation-digest
                           :fixtures [{:path (:test-path specification) :tests tests}]}))
                 link (when check (design/link :design/checked-by choice check))
                 run (when check (design/check-run choice check pins at (probe feature)))]
             {:choice choice :check check :link link :run run})) features)
        values #(vec (keep % groups))
        choices (values :choice) checks (values :check) links (values :link) runs (values :run)]
    {:choices choices :checks checks :links links :runs runs
     :records (vec (mapcat #(keep % [:choice :check :link :run]) groups))}))
