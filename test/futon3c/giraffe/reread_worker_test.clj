(ns futon3c.giraffe.reread-worker-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-worker :as worker])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture []
  (let [root (.toFile (Files/createTempDirectory "reread-worker-"
                                                 (make-array FileAttribute 0)))
        source (io/file root "M-one.md")
        generations (io/file root "generations")
        base (io/file generations "base")
        pointer (io/file root "corpus")
        db (io/file root "requests.sqlite")]
    (.mkdirs base)
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
      (let [store (queue/open-store db)
            open [{:id "M-one" :source-pin new-pin}]
            coverage {:members [{:id "M-one" :status :read :source old-pin}]}
            replacement {:id "M-one" :source new-pin :status_as_read "open"
                         :purpose "new" :edges [] :unresolved []}]
        (queue/reconcile! store open coverage 10)
        {:root root :source source :generations generations :pointer pointer
         :store store :open open :coverage coverage :replacement replacement}))))

(defn- opts [f contact]
  {:store (:store f) :pointer (:pointer f) :generations-dir (:generations f)
   :contact-fn contact :now-ms 20 :lease-ms 100 :max-attempts 2
   :new-item-cap 1 :reader-id "reader-1"})

(deftest one-request-dispatches-validates-publishes-and-lands
  (let [f (fixture)
        seen (atom nil)
        result (worker/run-once!
                (opts f (fn [request prompt]
                          (reset! seen {:request request :prompt prompt})
                          {:ok true :record (:replacement f)})))]
    (is (= :landed (:status result)))
    (is (= :landed (:state (first (queue/requests (:store f))))))
    (is (= (get-in f [:replacement :source :sha256])
           (get-in @seen [:request :source_sha])))
    (is (re-find #"six-line window" (:prompt @seen)))
    (is (re-find #"Return ONLY one JSON object" (:prompt @seen)))
    (is (re-find #"do not bell or whistle anyone" (:prompt @seen)))
    (is (str/includes? (:prompt @seen)
                       (str "python3 -I '" (.getAbsolutePath
                                             (io/file (:pointer f) "check_edges.py")) "'")))
    (is (str/includes? (:prompt @seen)
                       (.getAbsolutePath (io/file (:pointer f) "members.json"))))
    (is (str/includes? (:prompt @seen) "never coin an id"))
    (is (str/includes? (:prompt @seen) "grep -n"))
    (is (str/includes? (:prompt @seen) "sha256sum"))
    (let [prompt (:prompt @seen)]
      (doseq [key-name ["to" "kind" "basis" "evidence" "note"
                        "raw_target" "reason" "path" "sha256" "line" "quote"]]
        (is (str/includes? prompt key-name) (str "prompt names key " key-name)))
      (doseq [value ["depends-on" "unblocks" "part-of" "advances" "relates-to"
                     "duplicates" "superseded-by" "declared" "inferred"
                     "open" "done-unmarked" "abandoned" "unclear"]]
        (is (str/includes? prompt value) (str "prompt enumerates " value))))))

(deftest reader-self-check-scratch-is-durable-not-temporary
  (let [path (worker/scratch-path
              {:target_id "M-one" :source_sha (apply str (repeat 64 "a"))})]
    (is (= (str (System/getProperty "user.home")
                "/code/storage/giraffe-reread-scratch/M-one-aaaaaaaa.jsonl")
           path))
    (is (not (str/starts-with? path "/tmp/")))))

(deftest failed-contact-retries-to-bound-then-stays-visible
  (let [f (fixture)
        contact (fn [_ _] {:ok false :reason :reader-unavailable})]
    (is (= :failed (:status (worker/run-once! (opts f contact)))))
    (is (= [:pending 1] ((juxt :state :attempts)
                          (first (queue/requests (:store f))))))
    (is (= :failed (:status (worker/run-once! (assoc (opts f contact) :now-ms 30)))))
    (is (= [:failed 2 :reader-unavailable]
           ((juxt :state :attempts :last_error)
            (first (queue/requests (:store f))))))))

(deftest source-change-after-reader-return-supersedes-request
  (let [f (fixture)
        result (worker/run-once!
                (opts f (fn [_ _]
                          (spit (:source f) "changed again\n")
                          {:ok true :record (:replacement f)})))]
    (is (= :superseded (:status result)))
    (is (= :superseded (:state (first (queue/requests (:store f))))))
    (is (= :giraffe-generation/source-changed
           (:superseded_reason (first (queue/requests (:store f))))))))

(deftest queue-supersession-while-job-runs-prevents-publication
  (let [f (fixture)
        before (:path (generation/resolve-generation (:pointer f)))
        result (worker/run-once!
                (opts f (fn [_ _]
                          (spit (:source f) "newest\n")
                          (let [pin {:path (.getAbsolutePath (:source f))
                                     :sha256 (generation/file-sha (:source f))}]
                            (queue/reconcile! (:store f)
                                              [{:id "M-one" :source-pin pin}]
                                              (:coverage f) 21))
                          {:ok true :record (:replacement f)})))]
    (is (= :superseded (:status result)))
    (is (= before (:path (generation/resolve-generation (:pointer f)))))))

(deftest kimi-contact-uses-requisition-lifecycle
  (let [completed (atom nil)
        source {:path "/code/M-one.md" :sha256 "sha"}
        record {:id "M-one" :source source :status_as_read "open"
                :purpose "read" :edges [] :unresolved []}
        result (worker/kimi-contact
                {:reader-seat "kimi-1" :kimi-task-repo "/dedicated/repo"
                 :poll-ms 0
                 :task-runner (fn [_] {:ok true :task-id "E-kimi-task-1"
                                       :job-id "invoke-1-2-abc"})
                 :job-reader (fn [_] {:state "done"
                                      :result (json/generate-string record)})
                 :completer (fn [task job] (reset! completed [task job]))}
                {:target_id "M-one" :source_path (:path source)
                 :source_sha (:sha256 source)}
                "packet")]
    (is (:ok result))
    (is (= record (:record result)))
    (is (= ["E-kimi-task-1" "invoke-1-2-abc"] @completed))))
