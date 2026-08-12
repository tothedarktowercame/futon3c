#!/usr/bin/env bb

(ns frames
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def apm-root "/home/joe/code/apm-lean")
(def worktrees-root "/home/joe/code/apm-frames")
(def frames-root "/home/joe/code/futon3c/data/experiment-frames")

(def setup-slots
  [:frame/id :problem :base-revision :checkout :branch :seat :session
   :memory-channel :recall-system :resources :budget])

(def closure-slots
  [:commit-or-obstruction :receipts :lake-result :axioms :interview :twin-diff])

(defn die [message]
  (binding [*out* *err*] (println (str "ERROR: " message)))
  (System/exit 2))

(defn parse-options [args]
  (loop [xs args, result {}]
    (if (empty? xs)
      result
      (let [[flag value & more] xs]
        (when-not (and (string? flag) (str/starts-with? flag "--") value)
          (die (str "expected --option VALUE, got " (pr-str xs))))
        (recur more (assoc result (keyword (subs flag 2)) value))))))

(defn require-options! [options names]
  (doseq [name names]
    (when (str/blank? (get options name))
      (die (str "missing required option --" (clojure.core/name name)))))
  options)

(defn run! [& command]
  (let [{:keys [exit out err]} (apply process/sh command)]
    (when-not (zero? exit)
      (die (str "command failed (" exit "): " (str/join " " command)
                "\n" err)))
    (str/trim out)))

(defn sha256 [text]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes (str text) "UTF-8"))
    (format "%064x" (java.math.BigInteger. 1 (.digest digest)))))

(defn write-edn! [path value]
  (fs/create-dirs (fs/parent path))
  (spit (str path) (str (pr-str value) "\n")))

(defn read-edn [path]
  (edn/read-string (slurp (str path))))

(defn present? [value]
  (and (some? value)
       (not (and (string? value) (str/blank? value)))))

(defn setup-missing [frame]
  (vec (remove #(present? (get frame %)) setup-slots)))

(defn closure-missing [frame]
  (let [base (remove #(present? (get frame %)) closure-slots)
        session-pending (when (= :recorded-at-close (:session frame))
                          [:session/runner-session])
        coo (:commit-or-obstruction frame)
        receipts (:receipts frame)
        lake (:lake-result frame)
        interview (:interview frame)]
    (vec
      (concat base session-pending
              (when (and (map? coo)
                         (not (or (present? (:commit coo))
                                  (present? (:obstruction coo)))))
                [:commit-or-obstruction/value])
              (when (and (map? receipts) (not (present? (:offered receipts))))
                [:receipts/offered])
              (when (and (map? receipts) (not (present? (:outcome receipts))))
                [:receipts/outcome])
              (when (and (map? lake) (not (integer? (:exit lake))))
                [:lake-result/exit])
              (when (and (map? lake) (not (present? (:output-hash lake))))
                [:lake-result/output-hash])
              (when (and (map? interview)
                         (not (or (present? (:ref interview))
                                  (present? (:skipped interview)))))
                [:interview/value])))))

(defn frame-missing [frame]
  (vec (concat (setup-missing frame) (closure-missing frame))))

(defn none->nil [value]
  (when (and value (not= value "none")) value))

(defn open-frame! [args]
  (let [o (require-options! (parse-options args)
                            [:problem :arm :base-rev :seat :memory-channel
                             :recall-system :batch])
        ;; "case" = M-case-studies single-arm frame (no twin; branch
        ;; must be batch-qualified via --branch since exp/<pid>-<arm>
        ;; names collide with closed batch frames).
        _ (when-not (#{"mem" "ctl" "case"} (:arm o))
            (die "--arm must be mem, ctl, or case"))
        _ (when-not (#{"push" "none"} (:memory-channel o))
            (die "--memory-channel must be push or none"))
        frame-id (str (:batch o) "-" (:problem o) "-" (:arm o))
        checkout (str (fs/path worktrees-root frame-id))
        branch (or (:branch o) (str "exp/" (:problem o) "-" (:arm o)))
        record-path (fs/path frames-root (:batch o) (str frame-id ".edn"))
        revision (run! "git" "-C" apm-root "rev-parse"
                       (str (:base-rev o) "^{commit}"))]
    (when (fs/exists? record-path) (die (str "frame already exists: " record-path)))
    (when (fs/exists? checkout) (die (str "checkout already exists: " checkout)))
    (run! "git" "-C" apm-root "worktree" "add" "-b" branch checkout revision)
    (try
      (run! "cp" "-al" (str (fs/path apm-root ".lake"))
            (str (fs/path checkout ".lake")))
      (let [frame {:frame/id frame-id
                   :problem (:problem o)
                   :arm (keyword (:arm o))
                   :base-revision revision
                   :checkout checkout
                   :branch branch
                   :seat (:seat o)
                   ;; The runner's ACTUAL session id is knowable only after
                   ;; dispatch (job record). A minted UUID here asserted isolation
                   ;; that did not exist (claude-3, batch-2 session escalation).
                   :session :recorded-at-close
                   :memory-channel (keyword (:memory-channel o))
                   :recall-system (:recall-system o)
                   :batch (:batch o)
                   :resources {:systemd-slice "futon-agents.slice"
                               :memory-max "16G" :cpu-quota "400%"}
                   :budget {:tokens :campaign-default
                            :wall-clock :campaign-default}
                   :frame/status :open}
            missing (setup-missing frame)]
        (when (seq missing) (die (str "internal error: missing setup slots " missing)))
        (write-edn! record-path frame)
        (println "OPEN" frame-id)
        (println "checkout:" checkout)
        (println "record:" (str record-path))
        (println "session:" (:session frame))
        (println "resources:" (pr-str (:resources frame)))
        (println "budget:" (pr-str (:budget frame))))
      (catch Exception e
        (process/sh "git" "-C" apm-root "worktree" "remove" "--force" checkout)
        (throw e)))))

(defn locate-frame [frame-id]
  (let [matches (vec (filter #(= (str frame-id ".edn") (str (fs/file-name %)))
                             (fs/glob frames-root "**/*.edn")))]
    (case (count matches)
      0 (die (str "unknown frame: " frame-id))
      1 (first matches)
      (die (str "ambiguous frame id " frame-id ": " matches)))))

(defn close-frame! [args]
  (let [o (parse-options args)
        _ (when (str/blank? (:frame o)) (die "missing required option --frame"))
        path (locate-frame (:frame o))
        frame (read-edn path)
        axioms-path (:axioms-file o)
        axioms (when (and axioms-path (fs/regular-file? axioms-path))
                 (slurp axioms-path))
        exit (when-let [value (:lake-exit o)]
               (try (parse-long value) (catch Exception _ nil)))
        runner-session (:runner-session o)
        interview (when-let [value (:interview o)]
                    (if (str/starts-with? value "skip:")
                      {:skipped (subs value 5)}
                      {:ref value}))
        _ (doseq [[k v] {:offered-receipt (:offered-receipt o)
                         :outcome-receipt (:outcome-receipt o)}]
            (when (= v "MISSING")
              (die (str "sentinel MISSING passed as " (name k)
                        " — look up the real receipt id (claude-3 shakedown D1)"))))
        updated (cond-> frame
                  runner-session (assoc :session runner-session)
                  true (assoc
                  :commit-or-obstruction
                  {:commit (none->nil (:commit o))
                   :obstruction (none->nil (:obstruction o))}
                  :receipts {:offered (:offered-receipt o)
                             :outcome (:outcome-receipt o)}
                  :lake-result {:exit exit
                                :output-hash (when axioms (sha256 axioms))}
                  :axioms axioms
                  :interview interview
                  :twin-diff (if (= :case (:arm frame))
                               {:not-applicable :single-arm-case}
                               :pending-pair)))
        missing (frame-missing updated)
        status (if (seq missing) :incomplete :closed)
        final (assoc updated :frame/status status)]
    (write-edn! path final)
    ;; Close-time auto-commit (amendment 9 / case-studies hardening):
    ;; the frame corpus is versioned; a record that only exists in the
    ;; working tree has no past.
    (run! "git" "-C" "/home/joe/code/futon3c" "add" (str path))
    (run! "git" "-C" "/home/joe/code/futon3c" "commit" "-m"
          (str "frame close: " (:frame/id final) " (" (name status) ")")
          "--" (str path))
    (println (str/upper-case (name status)) (:frame/id final))
    (println "lake output hash source: --axioms-file")
    (println (str "twin diff: " (pr-str (:twin-diff final))))
    (when (seq missing) (println "MISSING CLOSURE SLOTS:" (str/join ", " missing)))))

(defn validate-batch! [args]
  (let [o (require-options! (parse-options args) [:batch])
        dir (fs/path frames-root (:batch o))
        paths (if (fs/directory? dir) (sort (fs/glob dir "*.edn")) [])]
    (println (format "%-42s %-12s %s" "FRAME" "STATUS" "MISSING"))
    (let [bad? (atom (empty? paths))]
      (doseq [path paths]
        (let [frame (read-edn path)
              missing (frame-missing frame)
              invalid? (or (= :incomplete (:frame/status frame)) (seq missing))]
          (when invalid? (reset! bad? true))
          (println (format "%-42s %-12s %s"
                           (:frame/id frame)
                           (name (:frame/status frame))
                           (if (seq missing) (str/join "," missing) "-")))))
      (when @bad? (System/exit 1)))))

(defn usage []
  (println "usage: frames.bb open|close|validate [--option value ...]"))

(defn twin! [args]
  (let [o (require-options! (parse-options args) [:frame-a :frame-b :diff])
        _ (when-not (fs/regular-file? (:diff o))
            (die (str "no such diff file: " (:diff o))))
        content (slurp (:diff o))
        ;; str/split-lines on "" yields [""] -> :lines 1 for a byte-identical
        ;; pair, indistinguishable from one-line divergence (claude-3,
        ;; batch-2 pair 1). Identical twins are the dataset's strongest
        ;; label; record them as such.
        lines (if (str/blank? content) 0 (count (str/split-lines content)))]
    (doseq [fid [(:frame-a o) (:frame-b o)]]
      (let [path (locate-frame fid)
            f (read-edn path)]
        (write-edn! path (assoc f :twin-diff
                                 (cond-> {:path (str (:diff o)) :lines lines}
                                   (zero? lines) (assoc :identical true))))
        (println "TWIN" fid "->" (str (:diff o)) "(" lines "lines"
                 (if (zero? lines) "— BYTE-IDENTICAL )" ")"))))))

(let [[command & args] *command-line-args*]
  (case command
    "open" (open-frame! args)
    "close" (close-frame! args)
    "twin" (twin! args)
    "validate" (validate-batch! args)
    (do (usage) (System/exit 2))))
