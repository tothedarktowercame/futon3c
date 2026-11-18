(ns futon3c.circus.chain-certificate
  "Generate and check the fixed Lean certificate for one Circus chain."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.circus.chain-record :as chain]))

(def mathlib-dir "/home/joe/code/mathlib4")
(def animals [:驴 :馬 :狗 :象 :mentions :猴])

(defn- lean-string [x] (json/generate-string (str x)))
(defn- lean-list [f xs] (str "[" (str/join ", " (map f xs)) "]"))
(defn- lean-bool [x] (if x "true" "false"))
(defn- name-string [x] (if (nil? x) "" (name x)))

(defn- animal-row [record animal]
  (let [{:keys [status clusion-ids reason]} (get-in record [:gcc animal])]
    (str "⟨" (lean-string (name animal)) ", "
         (lean-string (name-string status)) ", "
         (lean-list lean-string clusion-ids) ", "
         (lean-string (name-string reason)) "⟩")))

(defn- priority-row [{:keys [item-id status cutoff-status]}]
  (str "⟨" (lean-string item-id) ", "
       (lean-string (name-string status)) ", "
       (lean-string (name-string cutoff-status)) "⟩"))

(defn render
  "Render RECORD and the run's independently supplied PINNED-IDS. The theorem
  suite is invariant: malformed or incomplete data remains in the definitions
  and makes Lean reject the corresponding theorem."
  [record pinned-ids]
  (let [turn-id (get-in record [:turn :id])
        run-id (get-in record [:run :run-id])
        ma-reading (:ma-reading record)
        ma-proposed (:proposed-pattern-ids ma-reading)
        ma-completed (keep #(when (= :completed (:status %)) (:pattern %))
                           (:patterns ma-reading))
        ma-absence (name-string (:legitimate-absence-reason ma-reading))
        ma-commit-state (or (:commit-state ma-reading) :unspecified)
        gcc-ids (mapcat #(get-in record [:gcc % :clusion-ids] []) animals)]
    (str "import Mathlib\n\n"
         "/-! GENERATED FILE — DO NOT EDIT.\n"
         "Record SHA-256: " (:record/sha256 record) "\n"
         "Turn: " turn-id "\n"
         "Run: " run-id "\n-/\n\n"
         "-- Ma read commit state: " (name-string ma-commit-state) "\n\n"
         "namespace DarkTower.Circus.ChainCertificate\n\n"
         "structure AnimalRecord where\n"
         "  animal : String\n  status : String\n  ids : List String\n  reason : String\n"
         "  deriving DecidableEq, Repr\n\n"
         "structure PriorityRecord where\n"
         "  item : String\n  status : String\n  cutoffStatus : String\n"
         "  deriving DecidableEq, Repr\n\n"
         "def animals : List AnimalRecord := "
         (lean-list #(animal-row record %) animals) "\n"
         "def turnGccIds : List String := " (lean-list lean-string gcc-ids) "\n"
         "def pinnedIds : List String := " (lean-list lean-string pinned-ids) "\n"
         "def focusLink : List String := "
         (lean-list lean-string (:focus-link record)) "\n"
         "def turnItems : List String := " (lean-list lean-string (:items record)) "\n"
         "def priorities : List PriorityRecord := "
         (lean-list priority-row (get-in record [:priority :items])) "\n"
         "def runVerified : Bool := "
         (lean-bool (true? (get-in record [:run :verification :ok?]))) "\n"
         "def focusVersionPresent : Bool := "
         (lean-bool (some? (get-in record [:run :focus/version]))) "\n\n"
         "def maProposedPatterns : List String := "
         (lean-list lean-string ma-proposed) "\n"
         "def maCompletedPatterns : List String := "
         (lean-list lean-string ma-completed) "\n"
         "def maLegitimateAbsence : String := " (lean-string ma-absence) "\n\n"
         "def validAnimal (a : AnimalRecord) : Bool :=\n"
         "  (a.status == \"present\" && !a.ids.isEmpty) ||\n"
         "  (a.status == \"absent\" && !a.reason.isEmpty)\n\n"
         "theorem every_animal_present_or_typed_absent :\n"
         "    animals.all validAnimal = true := by decide\n\n"
         "theorem ma_read_every_proposed_pattern :\n"
         "    (!maLegitimateAbsence.isEmpty || maProposedPatterns.all maCompletedPatterns.contains) = true := by\n"
         "  decide\n\n"
         "theorem focus_link_in_turn_gcc_and_pin :\n"
         "    (!focusLink.isEmpty && focusLink.all (fun id => turnGccIds.contains id && pinnedIds.contains id)) = true := by\n"
         "  decide\n\n"
         "theorem turn_items_ranked_within_cutoff :\n"
         "    (!turnItems.isEmpty && turnItems.all (fun item => priorities.any (fun p =>\n"
         "      p.item == item && p.status == \"ranked\" && p.cutoffStatus == \"within-cutoff\"))) = true := by\n"
         "  decide\n\n"
         "theorem run_verified : runVerified && focusVersionPresent = true := by\n"
         "  decide\n\n"
         "#print axioms every_animal_present_or_typed_absent\n"
         "#print axioms ma_read_every_proposed_pattern\n"
         "#print axioms focus_link_in_turn_gcc_and_pin\n"
         "#print axioms turn_items_ranked_within_cutoff\n"
         "#print axioms run_verified\n"
         "end DarkTower.Circus.ChainCertificate\n")))

(defn pinned-ids [run-dir]
  (let [manifest (edn/read-string (slurp (io/file run-dir "manifest.edn")))]
    (->> (get-in manifest [:focus-relation-input :evidence-pin :evidence-pin])
         (keep #(when (sequential? %) (first %))) distinct sort vec)))

(defn lean-check [path]
  (shell/sh "lake" "env" "lean" (.getAbsolutePath (io/file path)) :dir mathlib-dir))

(defn write-and-check!
  [{:keys [turn-id run-dir output-path commit-state]}]
  (let [record (cond-> (chain/chain-record {:turn-id turn-id :run-dir run-dir})
                 commit-state (assoc-in [:ma-reading :commit-state] commit-state))
        output (io/file output-path)]
    (io/make-parents output)
    (spit output (render record (pinned-ids run-dir)))
    (let [result (lean-check output)]
      (spit (str (.getAbsolutePath output) ".check.json")
            (json/generate-string
             {"exit" (:exit result) "ok" (zero? (:exit result))
              "at" (str (java.time.Instant/now))
              "record_sha256" (:record/sha256 record)}))
      {:ok? (zero? (:exit result)) :exit (:exit result)
       :path (.getAbsolutePath output) :record-sha (:record/sha256 record)})))

(defn -main [& [turn-id run-dir output-path]]
  (when-not (every? some? [turn-id run-dir output-path])
    (throw (ex-info "usage: chain-certificate TURN-ID RUN-DIR OUTPUT.lean" {})))
  (prn (write-and-check! {:turn-id turn-id :run-dir run-dir
                          :output-path output-path})))
