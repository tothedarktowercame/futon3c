(ns futon3c.vsatarcs.feeder-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.vsatarcs.feeder :as feeder]))

(defn- temp-dir-path
  []
  (doto (io/file (str (System/getProperty "java.io.tmpdir")
                      "/vsatarcs-feeder-test-"
                      (System/nanoTime)))
    .mkdirs
    .getAbsolutePath))

(defn- write-file!
  [path body]
  (io/make-parents path)
  (spit path body)
  path)

(defn- fixture-config
  [dir]
  {:vsatarcs-aif-path (str dir "/vsatarcs.aif.edn")
   :pilot-inhabitations-path (str dir "/pilot-inhabitations.edn")
   :anchors-path (str dir "/wm-ui-anchors.edn")
   :vocabulary-path (str dir "/war-machine-strategic-vocabulary.edn")
   :wm-core-cljs-path (str dir "/core.cljs")
   :wm-judge-clj-path (str dir "/war_machine.clj")
   :missions-dir (str dir "/missions")
   :poll-ms 60000})

(defn- seed-fixtures!
  [config]
  (write-file! (:vsatarcs-aif-path config)
               "{:schema-version 1\n ;; keep this comment\n :bilateral-evidence []}\n")
  (write-file! (:pilot-inhabitations-path config)
               (str
                "{:events ["
                "{:id \"inhab/claude-9/cycle/408e44ed\""
                " :at \"2026-05-25T12:00:00Z\""
                " :event :cycle-complete"
                " :pilot-agent \"claude-9\""
                " :cycle-id \"408e44ed\""
                " :tool :coherence-row-author"
                " :cited-consent-gate-event-id \"cg-1\""
                " :target {:anchor \"wm-ui-anchor:0020\" :new-sub-kind :symptom-to-root-traceback}"
                " :note \"first substantive pilot cycle\"}"
                "{:id \"inhab/claude-1/bootstrap\""
                " :at \"2026-05-25T15:45:37Z\""
                " :event :substrate-creation"
                " :pilot-agent \"claude-1\""
                " :note \"created pilot-inhabitations substrate\"}"
                "{:id \"inhab/claude-1/in-flight\""
                " :at \"2026-05-25T16:00:00Z\""
                " :event :cycle-in-flight"
                " :pilot-agent \"claude-1\"}]}\n"))
  (write-file! (:anchors-path config)
               "{:anchors [] :coherence-evidence []}\n")
  (write-file! (:vocabulary-path config)
               "{:μ/override-modes {:stop-the-line {:landed \"2026-05-25\"}}}\n")
  (write-file! (:wm-core-cljs-path config)
               (str ";; replaces the static Pilot Contract\n"
                    "(defn demo [] [:div [:h3 \"Inhabitation Log\"]])\n"
                    "(:pilot-inhabitations data)\n"
                    "(defn stop-the-line-banner [data] [:div {:data-testid \"stop-the-line-banner\"}])\n"
                    "[:judgement :mode]\n"))
  (write-file! (:wm-judge-clj-path config)
               (str ";; OVERRIDE-MODE check\n"
                    ";; mode to :stop-the-line\n"))
  (write-file! (str (:missions-dir config) "/E-street-sweeper.md")
               "# E-street-sweeper\n\nFirst E-prefix excursion.\n")
  (write-file! (str (:missions-dir config) "/E-night-shift.md")
               "# E-night-shift\n\nBranch-isolated code repair excursion.\n"))

(deftest candidate-entries-derives-pilot-backfill-shapes
  (testing "candidate scan derives entries from pilot events plus code/vocab probes"
    (let [dir (temp-dir-path)
          config (fixture-config dir)]
      (seed-fixtures! config)
      (let [entries (feeder/candidate-entries config)
            ids (set (map :vsatarcs-id entries))
            kinds (set (map :evidence-kind entries))]
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-event-inhab-claude-9-cycle-408e44ed"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-event-inhab-claude-1-bootstrap"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-stop-the-line-vocabulary"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-inhabitation-log-ui-swap"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-stop-the-line-implementation"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-e-street-sweeper-excursion"))
        (is (contains? ids "hx:vsatarcs-align:auto:pilot-e-night-shift-excursion"))
        (is (contains? kinds :consent-gated-writer-event))
        (is (contains? kinds :one-sided-extension))
        (is (every? :auto-generated entries))
        (is (every? #(= :pending (:review-status %)) entries))))))

(deftest ingest-is-idempotent-by-id-and-content-hash
  (testing "second ingest pass appends nothing new"
    (let [dir (temp-dir-path)
          config (fixture-config dir)]
      (seed-fixtures! config)
      (let [first-pass (feeder/ingest! config)
            after-first (edn/read-string (slurp (:vsatarcs-aif-path config)))
            second-pass (feeder/ingest! config)
            after-second (edn/read-string (slurp (:vsatarcs-aif-path config)))]
        (is (pos? (:appended-count first-pass)))
        (is (= 0 (:appended-count second-pass)))
        (is (= (count (:bilateral-evidence after-first))
               (count (:bilateral-evidence after-second))))
        (is (= (:bilateral-evidence after-first)
               (:bilateral-evidence after-second)))))))
