(ns futon3c.giraffe.missions-test
  "M-giraffe stage 1 packet 6 acceptance tests: the mission-docs
  source adapter.  Fixtures are tiny fake repos built in a temp dir;
  the real-repos check with an empty edges file reads the canonical
  checkouts (local file reads only, no shell)."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.missions :as gm]
            [futon3c.giraffe.pass :as pass]
            [futon3c.giraffe.run :as run]
            [futon2.aif.mission-registry :as registry]))

(defn- temp-dir [tag]
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "giraffe-missions-" tag "-"
                        (System/currentTimeMillis) "-" (rand-int 100000)))]
    (.mkdirs d)
    (str d)))

(defn- spit-mission [root repo id body]
  (let [d (io/file root repo "holes/missions")]
    (.mkdirs d)
    (.mkdirs (io/file root repo ".git"))
    (spit (io/file d (str id ".md")) body)))

(defn- status-of [body]
  (some #(second (re-matches #"\*\*Status:\*\*(.*)" %)) (str/split-lines body)))

(def repo-a-bodies
  {"M-alpha" "**Status:** OPEN — IDENTIFY: stage 1 built; not done in stage 2\n\nwork\n"
   "M-beta" "**Status:** CLOSED 2026-06-10 — operator close via pilot\n"
   "M-gamma" "**Status:** Ready for handoff\n\nNeeds M-beta before it can run.\n"
   "M-delta" "**Status:** SPECIFIED, NOT YET IMPLEMENTED\n"
   "M-eps" "**Status:** READY TO EXECUTE\n"
   "M-zeta" "**Status:** Blocked on Parts II-V\n"
   "M-eta" "**Status:** DELIVERED (v1) - with named residue\n"
   "M-theta" "**Status:** :review\n"
   "M-kappa" "**Status:** HEAD. The substrate exists.\n"
   "M-lambda" "**Status:** SUPERSEDED by M-alpha\n"
   "M-omicron" "**Status:** PARTIAL (Phase 1 shipped; Phases 2-4 deferred)\n"
   ;; M-table carries the Missing-column invariant fixtures: the same
   ;; sentence inside the Ready|Missing table and outside it.
   "M-table" (str/join "\n" ["**Status:** OPEN"
                             ""
                             "Sentence needing M-beta sits twice."
                             ""
                             "| Ready | Missing |"
                             "|---|---|"
                             "| yes | Sentence needing M-beta sits twice. |"
                             ""
                             "Plain line: Sentence needing M-beta sits twice."
                             ""])})

(def repo-b-bodies
  {"M-alpha" "**Status:** OPEN — duplicate across repos\n"
   "M-mu" "**Status:** ANSWERED (option (b); (a) deferred)\n"})

(defn- fixture-root []
  (let [root (temp-dir "repos")]
    (doseq [[id body] repo-a-bodies] (spit-mission root "futonX" id body))
    (doseq [[id body] repo-b-bodies] (spit-mission root "futonY" id body))
    root))

(defn- line-of [root repo id needle]
  (let [lines (-> (io/file root repo "holes/missions" (str id ".md"))
                  (slurp) (str/split-lines))]
    (->> lines (keep-indexed (fn [i l] (when (str/includes? l needle) (inc i))))
         first)))

(defn- edges-file [root edges]
  (let [f (io/file root "edges.json")]
    (spit f (json/generate-string {:edges edges :rejected []}))
    (str f)))

(deftest status-mapping-covers-the-m-giraffe-list
  (let [expect {"M-alpha" :started "M-beta" :completed "M-gamma" :unstarted
                "M-delta" :unstarted "M-eps" :unstarted "M-zeta" :unstarted
                "M-eta" :completed "M-theta" :started "M-kappa" :started
                "M-lambda" :cancelled "M-omicron" :started
                "M-mu" :completed}]
    (doseq [[id group] expect
            :let [body (or (get repo-a-bodies id) (get repo-b-bodies id))
                  status (status-of body)]]
      (is (= group (gm/status->group status))
          (str id " (" (pr-str status) ") maps to " group))))
  ;; one unmapped example: neither packet-5 rules nor the M-giraffe list
  (is (nil? (gm/status->group "weird flux state")))
  (is (nil? (gm/status->group nil)))
  ;; "not done" prose after the marker must not flip OPEN to completed
  (is (= :started (gm/status->group
                   "OPEN — stage 1 built; not done in stage 2")))
  ;; the two real statuses that motivated the token fix: bare phase names
  (is (= :started (gm/status->group "DERIVE pending; MAP complete (2026-09-27)")))
  (is (= :started (gm/status->group "IDENTIFY (2026-05-05, draft 1)"))))

(deftest items-duplicates-and-edges
  (let [root (fixture-root)
        repos [(str root "/futonX") (str root "/futonY")]
        gamma-line (line-of root "futonX" "M-gamma" "Needs M-beta")
        tbl (line-of root "futonX" "M-table" "| yes |")
        plain (line-of root "futonX" "M-table" "Plain line:")
        quote "Sentence needing M-beta sits twice."
        efile (edges-file
               root
               [{:kind "needs" :from "M-gamma" :to "M-beta" :to_kind "cluster"
                 :class "closure" :file "futonX/holes/missions/M-gamma.md"
                 :line gamma-line :quote "Needs M-beta before it can run."}
                {:kind "needs" :from "M-table" :to "M-beta" :to_kind "cluster"
                 :class "closure" :file "futonX/holes/missions/M-table.md"
                 :line tbl :quote quote}
                {:kind "needs" :from "M-table" :to "M-beta" :to_kind "cluster"
                 :class "closure" :file "futonX/holes/missions/M-table.md"
                 :line plain :quote quote}
                {:kind "needs" :from "M-gamma" :to "M-beta" :to_kind "cluster"
                 :class "closure" :file "futonX/holes/missions/M-gamma.md"
                 :line gamma-line :quote ""}
                {:kind "needs" :from "M-eps" :to "T-car3-queue-routing-design-note"
                 :to_kind "external" :class "artifact"
                 :file "futonX/holes/missions/M-eps.md"
                 :line 1 :quote "**Status:** READY TO EXECUTE"}
                {:kind "needs" :from "M-alpha" :to "M-beta" :to_kind "cluster"
                 :class "closure" :file "futonX/holes/missions/M-gamma.md"
                 :line gamma-line :quote "Needs M-beta before it can run."}])
        {:keys [input rejected unmapped-statuses status-mapping]}
        (gm/missions->input {:repos repos :edges-file efile})
        items (:items input)
        by-id (into {} (map (juxt :id identity) items))
        rejects (frequencies (map :reject rejected))]
    (testing "duplicate resolution is the shared registry decision"
      (is (= "futonX/holes/missions/M-alpha.md" (:file (by-id :M-alpha))))
      (is (nil? (get rejects :item/duplicate-id))))
    (testing "item shape and order"
      (is (= "futonX" (:repo (by-id :M-gamma))))
      (is (= "futonX/holes/missions/M-gamma.md" (:file (by-id :M-gamma))))
      (is (= "Ready for handoff" (:declared-status (by-id :M-gamma))))
      (is (= :unstarted (:state-group (by-id :M-gamma))))
      (is (= (map :id (sort-by :source-order items))
             (map :id (sort-by (juxt :repo :file) items)))))
    (testing "edges"
      (let [gamma-edges (filter #(= :M-gamma (:from %)) (:edges input))]
        (is (= [{:from :M-gamma :to :M-beta :kind :item}]
               (map #(select-keys % [:from :to :kind]) gamma-edges))))
      (is (= 1 (get rejects :edge/own-missing-column))
          "quote cited inside the Ready|Missing table rejected")
      (is (= 1 (count (filter #(and (= :M-table (:from %)) (= :M-beta (:to %)))
                              (:edges input))))
          "the same quote outside the table admitted")
      (is (= 1 (get rejects :edge/quote-not-at-line)) "blank quote rejected")
      (let [ext (first (filter #(= :external (:kind %)) (:edges input)))]
        (is (= "T-car3-queue-routing-design-note" (:external-target ext)))
        (is (false? (:met ext))))
      (is (nil? (get rejects :edge/dangling-source))
          "M-alpha is retained by the shared registry"))
    (is (vector? unmapped-statuses))
    (is (seq status-mapping))
    (is (every? #(contains? % :mapped?) status-mapping))))

(deftest real-repos-empty-edges
  (let [efile (edges-file (temp-dir "real") [])
        {:keys [input rejected unmapped-statuses]}
        (gm/missions->input {:edges-file efile})
        items (:items input)
        groups (frequencies (map :state-group items))]
    (is (pos? (count items)))
    (is (= [] (:edges input)))
    (println "real repos: items=" (count items)
             "| groups=" groups
             "| unmapped=" (count unmapped-statuses)
             "| duplicate-id rejects="
             (count (filter #(= :item/duplicate-id (:reject %)) rejected)))))

;; Review (claude-13, 2026-10-28): cases from the real repos that the first
;; version got wrong.
(deftest review-middle-dot-separates-status-phrases
  ;; M-joe-told-me-about-futon read "MAP closed" as completed;
  ;; M-landscape-positioning read "MAP not started" as unstarted.
  (is (= :started (gm/status->group "HEAD and IDENTIFY accepted 2026-09-26 · **MAP closed** 2026-09-26")))
  (is (= :started (gm/status->group "OPEN · HEAD drafted 2026-10-10 · MAP not started (a first-pass"))))

(deftest unknown-status-is-retained-as-the-registry-requires
  (let [root (temp-dir "nostatus")]
    (spit-mission root "futonZ" "M-no-status" "# M-no-status\n\nNo status line here.\n")
    (let [{:keys [input unmapped-statuses]}
          (gm/missions->input {:repos [(str root "/futonZ")]
                               :edges-file (edges-file root [])})]
      (is (= [:M-no-status] (map :id (:items input))))
      (is (= [{:id :M-no-status :status-unmapped :status/missing}] unmapped-statuses)))))

(deftest real-omissions-and-exclusions-use-registry-authority
  (let [adapter (gm/missions->input {:edges-file "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json"})
        items (into {} (map (juxt :id identity)) (get-in adapter [:input :items]))
        actual (set (map (comp name :id) (:order (pass/pass (:input adapter)))))
        expected (set (map :id (registry/open-missions
                               (registry/load-missions-from-files "/home/joe/code"))))
        flat (items :M-G-over-cascades)
        plain (items :M-象-cascade)
        written (run/write-run! {:root (temp-dir "output") :adapter-result adapter
                                 :pass-output (pass/pass (:input adapter))})]
    (is (= "futon2/holes/M-G-over-cascades.md" (:file flat)))
    (is (contains? actual "M-G-over-cascades"))
    (is (contains? actual "M-象-cascade"))
    (is (= :identify (:registry-status plain)))
    (is (= expected actual) "independent registry census; no count fixed to a moving registry")
    (is (seq (:excluded adapter)))
    (is (not-any? #(contains? actual (:id %)) (:excluded adapter)))
    (is (.isFile (io/file (:dir written) "excluded.edn")))
    (is (:ok? (run/verify-run (:dir written))))))
