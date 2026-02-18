(ns scripts.update-alleycat-scorecard
  "Update `holes/missions/alleycat-scorecard.md` from a discipline live-gate artifact.

   Usage:
     clojure -M scripts/update_alleycat_scorecard.clj <artifact.edn>"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def scorecard-path "holes/missions/alleycat-scorecard.md")
(def section-start "<!-- discipline-live-gate:start -->")
(def section-end "<!-- discipline-live-gate:end -->")
(def signed-gate-line "  - Discipline live gate (Evidence Landscape + :discipline)")

(defn- ensure! [pred message data]
  (when-not pred
    (throw (ex-info message data))))

(defn- date-from [inst-str]
  (subs (str inst-str) 0 10))

(defn- repo-relative [repo-root path]
  (let [root (.toPath (io/file repo-root))
        file-path (.toPath (io/file path))]
    (try
      (str (.relativize root file-path))
      (catch Exception _
        path))))

(defn- build-section [artifact artifact-rel]
  (let [run-date (date-from (:ran-at artifact))
        sid (:session-id artifact)
        pattern-id (:pattern-id artifact)
        evidence-count (get-in artifact [:evidence :entry-count])
        thread-count (get-in artifact [:evidence :thread-entry-count])
        type-counts (get-in artifact [:evidence :type-counts])
        receipt (:receipt artifact)]
    (str section-start "\n"
         "## Discipline Live Gate: Evidence Landscape + :discipline\n\n"
         "Date: " run-date "\n"
         "Session: `" sid "`\n"
         "Invariant focus: **P-4 explicit transition** + **P-6 transport-backed discipline routing**\n\n"
         "### What Was Proven\n\n"
         "1. Codex and Claude both completed live WS readiness on Agency.\n"
         "2. Codex action dispatch routed through `peripheral/run-chain` with `peripheral_id=discipline`.\n"
         "3. A real-backend discipline cycle executed PSR->PUR->PAR for pattern `" pattern-id "`.\n"
         "4. Chain transitioned explicitly from `:discipline` to `:reflect` with stable session continuity.\n\n"
         "### Evidence\n\n"
         "- Artifact: `" artifact-rel "`\n"
         "- WS receipt route: `" (:route receipt) "` (`peripheral_id=" (:peripheral_id receipt) "`)\n"
         "- Session continuity: receipt session `" sid "` equals chain final context session.\n"
         "- Evidence entries for session: " evidence-count "\n"
         "- Pattern thread entry count: " thread-count "\n"
         "- Evidence type counts: `" (pr-str type-counts) "`\n\n"
         "### Grading\n\n"
         "- [x] Live WS readiness and discipline route receipt observed\n"
         "- [x] PSR/PUR round-trip produced typed `:pattern-selection` + `:pattern-outcome`\n"
         "- [x] PAR punctuation produced typed `:reflection`\n"
         "- [x] Explicit hop `:discipline -> :reflect` succeeded\n"
         "- [x] Session continuity preserved through hop\n\n"
         "### Status: **PASS**\n"
         section-end "\n")))

(defn- upsert-signed-gate [content]
  (if (str/includes? content signed-gate-line)
    content
    (let [anchor "  - Three-way chat gate (Joe + Claude + Codex, @-mention gated)\n"]
      (if (str/includes? content anchor)
        (str/replace content anchor (str anchor signed-gate-line "\n"))
        content))))

(defn- upsert-section [content section]
  (let [start-idx (str/index-of content section-start)
        end-idx (str/index-of content section-end)]
    (cond
      (and start-idx end-idx (> end-idx start-idx))
      (let [after-end (+ end-idx (count section-end))
            prefix (subs content 0 start-idx)
            suffix (subs content after-end)]
        (str (str/trimr prefix) "\n\n" section "\n" (str/triml suffix)))

      :else
      (str (str/trimr content) "\n\n---\n\n" section))))

(defn- run-update! [artifact-path]
  (let [repo-root (.getCanonicalPath (io/file "."))
        artifact-file (io/file artifact-path)
        _ (ensure! (.exists artifact-file)
                   "Artifact file not found"
                   {:artifact artifact-path})
        artifact (edn/read-string (slurp artifact-file))
        _ (ensure! (= :pass (:status artifact))
                   "Artifact status is not :pass"
                   {:artifact artifact-path :status (:status artifact)})
        scorecard-file (io/file scorecard-path)
        _ (ensure! (.exists scorecard-file)
                   "Scorecard file not found"
                   {:scorecard scorecard-path})
        artifact-rel (repo-relative repo-root (.getCanonicalPath artifact-file))
        section (build-section artifact artifact-rel)
        before (slurp scorecard-file)
        after (-> before
                  upsert-signed-gate
                  (upsert-section section))]
    (spit scorecard-file after)
    (println (str "Updated " scorecard-path))
    (println (str "Using artifact " artifact-rel))))

(let [artifact-path (first *command-line-args*)]
  (ensure! (some? artifact-path)
           "Usage: clojure -M scripts/update_alleycat_scorecard.clj <artifact.edn>"
           {})
  (run-update! artifact-path))
