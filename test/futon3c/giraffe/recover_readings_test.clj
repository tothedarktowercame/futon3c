(ns futon3c.giraffe.recover-readings-test
  "GIRAFFE-RECOVERY (2026-11-15): invented fixtures only — a prose-prefixed
  stored answer lands through the normal checks; a refused requisition (job
  failed, no work done) requeues as pending; the seat failure-share stop
  fires at the threshold and lifts when failures age out of the window."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-worker :as worker]
            [futon3c.giraffe.recover-readings :as recover])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture
  "Temp corpus (pointer symlink + generation base + toy checker), ledger dir
  and queue store with one FAILED request for M-one (attempts exhausted,
  :reader-record-invalid-json, the prose-prefix bug's terminal row)."
  []
  (let [root (.toFile (Files/createTempDirectory "recover-readings-"
                                                 (make-array FileAttribute 0)))
        source (io/file root "M-one.md")
        generations (io/file root "generations")
        base (io/file generations "base")
        pointer (io/file root "corpus")
        ledger (io/file root "ledger")
        db (io/file root "requests.sqlite")]
    (.mkdirs base)
    (.mkdirs ledger)
    (spit source "new source\n")
    (let [new-sha (generation/file-sha source)
          old-sha (apply str (repeat 64 "0"))
          new-pin {:path (.getAbsolutePath source) :sha256 new-sha}
          old-pin {:path (.getAbsolutePath source) :sha256 old-sha}
          record {:id "M-one" :source old-pin :status_as_read "open"
                  :purpose "old" :edges [] :unresolved []}]
      (spit (io/file base "members.json")
            (json/generate-string [(assoc old-pin :id "M-one")]))
      (spit (io/file base "batch-00.jsonl") (str (json/generate-string record) "\n"))
      (spit (io/file base "check_edges.py")
            (str "#!/usr/bin/env python3\n\"\"\"\n"
                 "EDGE = {\"to\": \"M-...|E-...|T-...\", \"kind\": KIND, \"basis\": \"declared|inferred\", \"evidence\": EVIDENCE, \"note\": \"...\"}\n"
                 "UNRESOLVED = {\"raw_target\": \"text as written\", \"kind\": KIND, \"basis\": ..., \"evidence\": EVIDENCE, \"reason\": \"why it does not resolve\"}\n"
                 "EVIDENCE = {\"path\": absolute file path, \"sha256\": sha256 of that file, \"line\": 1-based line where the quote starts, \"quote\": verbatim text, >= 12 chars, within lines line..line+5}\n"
                 "\"\"\"\n"
                 "KINDS = {\"depends-on\", \"unblocks\", \"part-of\", \"advances\", \"relates-to\", \"duplicates\", \"superseded-by\"}\n"
                 "BASES = {\"declared\", \"inferred\"}\n"
                 "STATUS = {\"open\", \"done-unmarked\", \"abandoned\", \"unclear\"}\n"
                 "import json,pathlib,sys\n"
                 "m={x['id']:x for x in json.loads((pathlib.Path(__file__).parent/'members.json').read_text())}\n"
                 "r=json.loads(pathlib.Path(sys.argv[1]).read_text())\n"
                 "sys.exit(0 if r.get('source') == {'path':m[r['id']]['path'],'sha256':m[r['id']]['sha256']} else 1)\n"))
      (Files/createSymbolicLink (.toPath pointer) (.toPath (.getAbsoluteFile base))
                                (make-array FileAttribute 0))
      (let [store (queue/open-store db)]
        (queue/reconcile! store [{:id "M-one" :source-pin new-pin}]
                          {:members [{:id "M-one" :status :read :source old-pin}]} 10)
        (queue/claim! store 20 1000 1 "zai-2")
        (queue/settle! store (first (queue/requests store))
                       {:status :failed :reason :reader-record-invalid-json
                        :answer-text "0 problems — pin matches the live file."}
                       30 1)
        {:root root :source source :generations generations :pointer pointer
         :ledger ledger :store store :new-pin new-pin :old-pin old-pin}))))

(defn- ledger-file!
  [{:keys [ledger new-pin]} job state]
  (spit (io/file ledger "E-giraffe-read-1.md")
        (str "# E-giraffe-read-1 — Giraffe reading M-one\n\n"
             "**Requisition:** completed — 2026-11-15T00:00:00Z, job " job
             ", state " state "\n\n## Packet\n\n"
             "Target: M-one\nSource path: " (:path new-pin) "\n"
             "Required source sha256: " (:sha256 new-pin) "\n")))

(defn- recover-opts [f & {:as extra}]
  (merge {:store (:store f) :ledger-dir (str (:ledger f))
          :pointer (str (:pointer f)) :generations-dir (str (:generations f))
          :now-ms 1000 :dry-run? false}
         extra))

(deftest prose-prefixed-stored-answer-lands
  (let [f (fixture)
        answer (str "0 problems — pin matches the live file.\n\n"
                    (json/generate-string
                     {:id "M-one" :source (:new-pin f) :status_as_read "open"
                      :purpose "new" :edges [] :unresolved []}))]
    (ledger-file! f "invoke-1-2-abc" "done")
    (let [report (recover/recover!
                  (recover-opts f :fetch-job (fn [_ _] {:state "done" :result answer})))
          row (first (queue/requests (:store f)))]
      (is (= [] (:still-failed report)))
      (is (= ["M-one"] (mapv :target (:recovered report))))
      (is (= :landed (:state row)))
      (is (string? (:landed_digest row)))
      ;; the landing is a normal generation publish: the pointer now serves the
      ;; new reading (new sha), not the old one
      (is (not= (slurp (io/file (:pointer f) "batch-00.jsonl"))
                (str (json/generate-string
                      {:id "M-one" :source (:old-pin f) :status_as_read "open"
                       :purpose "old" :edges [] :unresolved []}) "\n"))))))

(deftest refused-requisition-requeues-as-pending
  (let [f (fixture)]
    (ledger-file! f "invoke-1-3-bcd" "failed")
    ;; the refusal shape: terminal row with :agency-job-failed
    (queue/requeue! (:store f) (first (queue/requests (:store f))) 2000)
    (queue/claim! (:store f) 2100 1000 1 "kimi-1")
    (queue/settle! (:store f) (first (queue/requests (:store f)))
                   {:status :failed :reason :agency-job-failed}
                   2200 1)
    (let [report (recover/recover!
                  (recover-opts f :now-ms 3000
                                :fetch-job (fn [_ _] (throw (ex-info "must not fetch a refused job" {})))))
          row (first (queue/requests (:store f)))]
      (is (= ["M-one"] (mapv :target (:requeued report))))
      (is (= :pending (:state row)))
      (is (zero? (:attempts row)))
      (is (nil? (:last_error row))))))

(deftest settled-failure-keeps-raw-answer
  ;; the guard: any parse failure persists the reader's raw answer (last_answer)
  (let [f (fixture)
        row (first (queue/requests (:store f)))]
    (is (= :failed (:state row)))
    (is (= "0 problems — pin matches the live file." (:last_answer row)))))

(deftest seat-failure-share-fires-and-lifts
  (let [window 60000
        rows (concat (repeat 6 {:claimed_by "zai-2" :state :landed :last_error nil :updated_at_ms 20000})
                     (repeat 2 {:claimed_by "zai-2" :state :failed :last_error :reader-record-invalid-json :updated_at_ms 20000})
                     (repeat 1 {:claimed_by "kimi-1" :state :failed :last_error :agency-job-failed :updated_at_ms 20000})
                     ;; old failures: outside the window, must not count
                     (repeat 4 {:claimed_by "kimi-1" :state :failed :last_error :agency-job-failed :updated_at_ms 0}))]
    (let [health (queue/seat-health rows 70000 window)]
      (is (= 8 (:total (get health "zai-2"))))
      (is (= 1/4 (:failure-share (get health "zai-2"))))
      (is (true? (:stop-dispatch? (get health "zai-2"))))     ; 2/8 = 25% of >= 5 outcomes
      (is (false? (:stop-dispatch? (get health "kimi-1")))))  ; only 1 in-window outcome
    ;; lift: an hour later everything has aged out and dispatch resumes
    (is (empty? (filter :stop-dispatch? (vals (queue/seat-health rows (+ 70000 window) window)))))))

(deftest drain-once-skips-a-stopped-seat-and-lifts
  (let [f (fixture)
        store (:store f)
        calls (atom [])
        ;; invent terminal outcomes in-window: 6 landed + 1 failed for zai-2
        ;; (7 outcomes, 1/7 ~ 14%: below), then 2 more zai-2 failures -> 3/9 = 33%
        invent (fn [i seat outcome]
                 (let [target (str "M-seat-" i)]
                   (queue/reconcile! store [{:id (keyword target)
                                             :source-pin {:path (str (:source f))
                                                          :sha256 (str "sha-" i)}}]
                                     {:members []} (+ 5000 i))
                   (queue/claim! store (+ 5000 i) 1000 1 seat)
                   (queue/settle! store (first (queue/requests store {:target-id target}))
                                  (if (= :landed outcome)
                                    {:status :landed :digest "d"}
                                    {:status :failed :reason :reader-record-invalid-json})
                                  (+ 5000 i) 1)))
        run (fn [now-ms]
              (reset! calls [])
              (cli/drain-once! {:store store :reader-seats ["zai-2" "kimi-1"]
                                :pointer (str (:pointer f))
                                :generations-dir (str (:generations f))
                                :contact-fns (into {} (for [seat ["zai-2" "kimi-1"]]
                                                        [seat (fn [req _]
                                                                (swap! calls conj [seat (:target_id req)])
                                                                {:ok false :reason :reader-contact-failed})]))
                                :now-ms now-ms :new-item-cap 1 :mint-window-ms 600000
                                :mint-fn (fn [_] {:status :idle})
                                :parallel? false}))]
    (dotimes [i 6] (invent i "zai-2" :landed))
    (is (= [] (:seats-stopped (run 60000))) "6/6 landed: zai-2 healthy, and M-seat rows are terminal so nothing is claimed")
    (dotimes [i 2] (invent (+ 6 i) "zai-2" :failed))
    ;; one pending request, so a seat that is NOT stopped has something to read
    ;; (claude-13 review: without it, "not dispatched" held for both seats)
    (queue/reconcile! store [{:id :M-pending
                              :source-pin {:path (str (:source f)) :sha256 "sha-pending"}}]
                      {:members []} 69000)
    (let [stopped (run 70000)]
      (is (= ["zai-2"] (:seats-stopped stopped)))
      (is (true? (get-in stopped [:reader-health "zai-2" :stop-dispatch?])))
      (is (= [["kimi-1" "M-pending"]] @calls)
          "the stopped seat is not dispatched; the healthy seat reads the pending request"))
    ;; lift: window (one hour in drain-once!) moves past every invented outcome
    (let [lifted (run (+ 60000 3600000 1000))]
      (is (= [] (:seats-stopped lifted))))))

(deftest scratch-file-lands-when-the-job-transcript-is-gone
  ;; the job endpoint 404s after compaction; the reader's scratch file still
  ;; holds the self-checked record (this is how the 2026-11-15 batch recovered)
  (let [f (fixture)
        scratch (io/file (:root f) "giraffe-reread-scratch")]
    (.mkdirs scratch)
    (ledger-file! f "invoke-1-4-cde" "done")
    (let [path (.getAbsolutePath
                (io/file scratch
                         (str "M-one-" (subs (get-in f [:new-pin :sha256]) 0 8) ".jsonl")))]
      (with-redefs [worker/scratch-path (constantly path)]
        (spit path
              (str "0 problems — pin matches the live file.\n"
                   (json/generate-string
                    {:id "M-one" :source (:new-pin f) :status_as_read "open"
                     :purpose "new" :edges [] :unresolved []}) "\n"))
        (let [report (recover/recover!
                      (recover-opts f :fetch-job (fn [_ _] nil))) ; transcript gone
              row (first (queue/requests (:store f)))]
          (is (= ["M-one"] (mapv :target (:recovered report))))
          (is (= :landed (:state row))))))))

(deftest operator-withdrawn-row-is-never-landed
  ;; claude-13 review: the withdrawn filter compared (str keyword) and never
  ;; matched, so a withdrawn row with a stored answer would have been landed
  ;; against the operator's decision.
  (let [f (fixture)
        answer (json/generate-string
                {:id "M-one" :source (:new-pin f) :status_as_read "open"
                 :purpose "new" :edges [] :unresolved []})]
    (ledger-file! f "invoke-1-4-cde" "done")
    (queue/requeue! (:store f) (first (queue/requests (:store f))) 2000)
    (queue/claim! (:store f) 2100 1000 1 "zai-2")
    (queue/settle! (:store f) (first (queue/requests (:store f)))
                   {:status :failed
                    :reason (keyword "operator-withdrawn: invented bulk triage")}
                   2200 1)
    (let [report (recover/recover!
                  (recover-opts f :now-ms 3000
                                :fetch-job (fn [_ _] {:state "done" :result answer})))
          row (first (queue/requests (:store f)))]
      (is (= [] (:recovered report)))
      (is (= :failed (:state row))))))
