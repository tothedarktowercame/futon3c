(ns futon3c.logic.mission-doc-invariants-test
  "M-repl-circus document invariants (Joe, 2026-11-14): one document per
  id, and a readable status. Invented fixtures in temp dirs: a duplicate
  name across two roots is caught; a document with no status line is
  caught; a clean set yields no violations. The futon1b entity half is
  tested against a synthetic SQLite db."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.logic.mission-doc-invariants :as inv])
  (:import [java.nio.file Files]
           [java.sql DriverManager]))

(def ^:dynamic *tmp* nil)

(use-fixtures :each
  (fn [f]
    (let [tmp (str (Files/createTempDirectory "mission-doc-inv" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (binding [*tmp* tmp]
        (f)))))

(defn- spit-in [rel content]
  (let [f (io/file *tmp* rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (str f)))

(def mission-md
  "# Mission: Fixture

**Status:** in-progress

Body.
")

(defn- doc-file [rel _kind content]
  (spit-in rel content))

;; ---------------------------------------------------------------------------
;; Discovery: the scanner sees what the watcher sees
;; ---------------------------------------------------------------------------

(deftest scan-finds-documents-and-honours-nesting
  (let [_ (doc-file "repo-a/holes/missions/M-alpha.md" :mission mission-md)
        _ (doc-file "repo-a/holes/excursions/E-beta.md" :excursion
                    "# Excursion\n\n**Requisition:** completed — x\n")
        _ (doc-file "repo-a/holes/tickets/T-gamma.md" :ticket
                    "# Ticket\n\n**Status:** open\n")
        _ (doc-file "repo-a/holes/campaigns/C-delta.md" :campaign
                    "# Campaign\n\n**Status:** active\n")
        ;; a nested holes copy is NOT a document
        _ (doc-file "repo-a/holes/labs/run1/authority/holes/missions/M-alpha.md"
                    :mission mission-md)
        ;; a plain markdown file is not a document
        _ (doc-file "repo-a/notes/README.md" nil "hi")]
    (is (= [[:campaign "C-delta.md"]
              [:excursion "E-beta.md"]
              [:mission "M-alpha.md"]
              [:ticket "T-gamma.md"]]
           (sort (map (juxt :kind :external-id)
                      (inv/scan-docs [{:path (str (io/file *tmp* "repo-a")) :label "repo-a"}] [])))))))

;; ---------------------------------------------------------------------------
;; Invariant 1
;; ---------------------------------------------------------------------------

(deftest duplicate-name-across-two-roots-is-caught
  (spit-in "repo-a/holes/missions/M-dup.md" mission-md)
  (spit-in "repo-b/holes/missions/M-dup.md" mission-md)
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo-a")) :label "a"}
                             {:path (str (io/file *tmp* "repo-b")) :label "b"}]
                            [])
        vs (inv/duplicate-file-name-violations docs)]
    (is (= 1 (count vs)))
    (is (= "M-dup.md" (:external-id (first vs))))
    (is (= 2 (count (:paths (first vs)))))))

(deftest ledger-duplicate-is-caught
  (spit-in "repo/holes/excursions/E-kimi-task-1.md"
           "# E-kimi-task-1\n\n**Requisition:** completed — x\n")
  (spit-in "ledger/holes/excursions/E-kimi-task-1.md"
           "# E-kimi-task-1\n\n**Requisition:** completed — x\n")
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo")) :label "repo"}]
                            [(str (io/file *tmp* "ledger"))])]
    (is (= 1 (count (inv/duplicate-file-name-violations docs))))))

(deftest only-and-exclude-are-honoured
  ;; same name, but one copy lives outside :only and one inside :exclude
  (spit-in "repo/src/holes/missions/M-x.md" mission-md)
  (spit-in "repo/included/holes/missions/M-x.md" mission-md)
  (spit-in "repo/excluded/holes/missions/M-x.md" mission-md)
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo"))
                              :label "repo"
                              :only [(str (io/file *tmp* "repo/included"))]
                              :exclude [(str (io/file *tmp* "repo/excluded"))]}]
                            [])]
    (is (= 1 (count docs)))
    (is (empty? (inv/duplicate-file-name-violations docs)))))

(defn- synthetic-db []
  (let [db (str (io/file *tmp* "ent.db"))
        conn (DriverManager/getConnection (str "jdbc:sqlite:" db))]
    (with-open [s (.createStatement conn)]
      (.executeUpdate s "create table ent_node (id TEXT PRIMARY KEY, type TEXT NOT NULL, doc BLOB NOT NULL, name TEXT, external_id TEXT)")
      (.executeUpdate s "insert into ent_node values ('v1', ':mission/doc', x'00', 'v1', 'M-one')")
      (.executeUpdate s "insert into ent_node values ('v2', ':mission/doc', x'00', 'v2', 'M-one')")
      (.executeUpdate s "insert into ent_node values ('v3', ':excursion/doc', x'00', 'v3', 'E-one')"))
    (.close conn)
    db))

(deftest duplicate-futon1b-external-id-is-caught
  (let [vs (inv/duplicate-entity-violations (synthetic-db))]
    (is (= 1 (count vs)))
    (is (= "M-one" (:external-id (first vs))))
    (is (= 2 (:entity-count (first vs)))))

  (testing "a missing db is not a violation"
    (is (empty? (inv/duplicate-entity-violations
                 (str (io/file *tmp* "absent.db")))))))

;; ---------------------------------------------------------------------------
;; Invariant 2
;; ---------------------------------------------------------------------------

(deftest missing-status-line-is-caught
  (let [p (spit-in "repo/holes/missions/M-nostatus.md"
                   "# Mission: No Status\n\nJust a body.\n")
        docs (inv/scan-docs [{:path (str (io/file *tmp* "repo")) :label "repo"}] [])]
    (is (= 1 (count docs)))
    (let [vs (inv/unreadable-status-violations docs)]
      (is (= 1 (count vs)))
      (is (= p (:path (first vs))))
      (is (nil? (:raw-status (first vs))))
      (is (contains? #{:unknown nil} (:status (first vs)))))))

(deftest unrecognized-status-words-are-caught-with-raw-line
  (let [p (spit-in "repo/holes/tickets/T-odd.md"
                   "# Ticket\n\n**Status:** wibbling\n\nBody\n")
        docs (inv/scan-docs [{:path (str (io/file *tmp* "repo")) :label "repo"}] [])
        [v] (inv/unreadable-status-violations docs)]
    (is (= p (:path v)))
    (is (= "wibbling" (:raw-status v)))
    (is (= :unknown (:status v)))))

(deftest excursion-requisition-fallback-counts-as-readable
  (spit-in "repo/holes/excursions/E-req.md"
           "# E-req\n\n**Requisition:** in-progress | stuff\n\nBody\n")
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo")) :label "repo"}] [])]
    (is (= [] (inv/unreadable-status-violations docs)))))

(deftest clean-set-yields-no-violations
  (spit-in "repo-a/holes/missions/M-clean.md" mission-md)
  (spit-in "repo-a/holes/excursions/E-clean.md"
           "# E-clean\n\n**Status:** complete\n\nBody\n")
  (spit-in "repo-a/holes/tickets/T-clean.md"
           "# Ticket\n\n**Status:** closed\n\nBody\n")
  (spit-in "repo-a/holes/campaigns/C-clean.md"
           "# Campaign\n\n**Status:** active\n\nBody\n")
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo-a")) :label "repo-a"}] [])
        v (inv/violations {:docs docs :db-path (str (io/file *tmp* "absent.db"))})]
    (is (= [] (get-in v [:one-document-per-id :files])))
    (is (= [] (get-in v [:one-document-per-id :futon1b-entities])))
    (is (= [] (:readable-status v)))))

(deftest check-is-invariant-runner-shaped
  ;; the violations map is a plain {k [v]} shape an invariant-runner
  ;; domain spec can use directly as :check
  (spit-in "repo/holes/missions/M-nostatus.md" "# Mission\n\nBody\n")
  (let [docs (inv/scan-docs [{:path (str (io/file *tmp* "repo")) :label "repo"}] [])
        v (inv/check {:docs docs :db-path nil})]
    (is (map? v))
    (is (every? (fn [xs] (and (sequential? xs) (every? map? xs)))
                (concat [(:readable-status v)]
                        (vals (:one-document-per-id v)))))))
