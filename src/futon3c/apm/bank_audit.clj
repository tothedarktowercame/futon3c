(ns futon3c.apm.bank-audit
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def ^:private proof-path-template "problems/%s/lean/Main.lean")
(def allowed-proof-axioms ["propext" "Classical.choice" "Quot.sound"])

(defn- default-read-at-rev
  [rev path]
  (let [repo (io/file (System/getProperty "user.home") "code" "apm-lean")
        {:keys [exit out]} (shell/sh "git" "show" (str rev ":" path)
                                     :dir (.getPath repo))]
    (when (zero? exit)
      out)))

(defn- sha256
  [content]
  (format "%064x"
          (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                                  (.getBytes content StandardCharsets/UTF_8)))))

(defn- terminal-receipts
  [campaign-dir]
  (->> (or (.listFiles (io/file campaign-dir)) (make-array java.io.File 0))
       (filter #(.isDirectory ^java.io.File %))
       (map #(io/file % "terminal" "frame-terminal.edn"))
       (filter #(.isFile ^java.io.File %))
       (sort-by #(.getPath ^java.io.File %))
       (map #(edn/read-string (slurp %)))))

(defn unbanked-solved
  "Classify terminal solver heads by comparing their proof content to master.

  Frame outcome is deliberately not a predicate here: a frame can be voided by
  an apparatus failure after its solver produced a complete proof. The axiom
  gate in `verify-and-pin!` decides whether an unbanked head is safe to pin."
  [{:keys [campaign-dir read-at-rev master-rev]
    ;; origin/master, not master. bank-sweep branches from origin/master and
    ;; pushes there, and never advances the local branch — so comparing against
    ;; local master reported f44 :unbanked immediately after the sweep had
    ;; banked and pushed it. Two tools disagreeing about which ref is "master"
    ;; reads as work lost when it is not. Overridable for tests and for a
    ;; repo with no remote.
    :or {read-at-rev default-read-at-rev master-rev "origin/master"}}]
  (->> (terminal-receipts campaign-dir)
       (mapv (fn [receipt]
               (let [frame (:frame/id receipt)
                     problem-id (:problem/id receipt)
                     head (get-in receipt [:workspace/terminal-heads :solver])
                     path (format proof-path-template problem-id)
                     head-content (read-at-rev head path)
                     master-content (when (some? head-content)
                                      (read-at-rev master-rev path))
                     status (cond
                              (nil? head-content) :head-unresolvable
                              (and (some? master-content)
                                   (= (sha256 head-content) (sha256 master-content))) :banked
                              :else :unbanked)]
                 {:frame frame
                  :problem-id problem-id
                  :head head
                  :status status})))))

(defn- default-git
  [repo & args]
  (apply shell/sh (concat ["git" "-C" repo] args)))

(defn- default-run-lean
  [repo proof-file]
  (shell/sh "lake" "env" "lean" (.getAbsolutePath ^java.io.File proof-file)
            :dir repo))

(defn axiom-list
  [{:keys [out err]}]
  (let [text (str out "\n" err)]
    (cond
      ;; A declaration proved without any axiom prints this instead of a list.
      ;; That is the STRONGEST result, not a missing one.
      (re-find #"does not depend on any axioms" text) []
      :else
      (when-let [[_ axioms]
                 (re-find #"(?s)depends on axioms:\s*\[([^]]*)\]" text)]
        (->> (str/split axioms #",")
             (mapv str/trim))))))

(defn proof-standard-for-source!
  "Elaborate SOURCE and return its typed proof-standard observation. ARTIFACT-ID
  identifies the immutable source revision or captured source object."
  [{:keys [problem-id artifact-id repo source run-lean]
    :or {run-lean default-run-lean}}]
  (let [theorem (or (last (map second
                               (re-seq #"(?m)^(?:theorem|lemma)\s+(apm_[A-Za-z0-9_']+)"
                                       (str source))))
                    (str "apm_" (str/lower-case problem-id)))
            temp-path (java.nio.file.Files/createTempDirectory
                       "apm-proof-standard-"
                       (make-array java.nio.file.attribute.FileAttribute 0))
            temp-dir (.toFile temp-path)
            proof-file (io/file temp-dir "Main.lean")]
    (try
      (spit proof-file (str source "\n#print axioms " theorem "\n"))
      (let [elaboration (run-lean repo proof-file)
            axioms (axiom-list elaboration)
            observation {:artifact-id artifact-id
                         :declaration-name theorem
                         :solved-claim? true
                         :axiom-names axioms
                         :allowed-axiom-names allowed-proof-axioms}]
        (cond
          (not (zero? (:exit elaboration)))
          {:ok false :error/code :apm-proof-standard-elaboration-failed
           :problem/id problem-id :artifact-id artifact-id
           :exit (:exit elaboration) :stderr (:err elaboration)}
          ;; No parse at all: elaboration succeeded but produced no axiom
          ;; verdict. Inconclusive is not clean.
          (nil? axioms)
          {:ok false :error/code :apm-proof-standard-observation-missing
           :problem/id problem-id :artifact-id artifact-id :declaration theorem
           :allowed-axioms allowed-proof-axioms
           :trace/proof-standard-observation observation}
          ;; Reject only axioms OUTSIDE the standard. A proof depending on
          ;; fewer axioms than allowed -- or none -- is stronger, not invalid.
          (seq (remove (set allowed-proof-axioms) axioms))
          {:ok false :error/code :apm-proof-standard-axioms-invalid
           :problem/id problem-id :artifact-id artifact-id :declaration theorem
           :axioms axioms :allowed-axioms allowed-proof-axioms
           :disallowed-axioms (vec (remove (set allowed-proof-axioms) axioms))
           :trace/proof-standard-observation observation}
          :else
          {:ok true :trace/proof-standard-observation observation}))
      (finally
        (doseq [file (reverse (file-seq temp-dir))]
          (io/delete-file file true))))))

(defn proof-standard-observation!
  "Read the target declaration at HEAD, then elaborate it into durable trace
  evidence. A nonstandard axiom set is a typed proof-standard failure, not a
  compilation failure. Effects are injectable for focused tests."
  [{:keys [problem-id head repo run-lean git]
    :or {run-lean default-run-lean git default-git}}]
  (let [proof-path (format proof-path-template problem-id)
        shown (git repo "show" (str head ":" proof-path))]
    (if-not (zero? (:exit shown))
      {:ok false :error/code :apm-proof-standard-source-unresolvable
       :problem/id problem-id :head head}
      (proof-standard-for-source!
       {:problem-id problem-id :artifact-id head :repo repo :source (:out shown)
        :run-lean run-lean}))))

(defn- safe-ref-component?
  [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z0-9._-]+" value))))

(defn verify-and-pin!
  "Elaborate an unbanked solver head, require the exact clean axiom set, and
  pin the verified commit under refs/apm/banked-solves. Effects are injectable
  for deterministic tests."
  [{:keys [frame problem-id head status repo run-lean git]
    :or {run-lean default-run-lean git default-git}}]
  (if (not= :unbanked status)
    {:status :skipped :reason status}
    (let [proof-path (format proof-path-template problem-id)
          project-mathlib (io/file repo ".lake" "build" "lib" "Mathlib.olean")]
      (cond
        (not (and (safe-ref-component? frame)
                  (safe-ref-component? problem-id)
                  (string? head)
                  (re-matches #"[0-9a-f]{40}" head)
                  (string? repo)))
        {:status :refused :reason :input-invalid}

        (.exists project-mathlib)
        {:status :refused :reason :mathlib-project-olean-present}

        :else
        (let [shown (git repo "show" (str head ":" proof-path))]
          (if-not (zero? (:exit shown))
            {:status :refused :reason :head-unresolvable}
            (let [temp-path (java.nio.file.Files/createTempDirectory
                             "apm-bank-verify-"
                             (make-array java.nio.file.attribute.FileAttribute 0))
                  temp-dir (.toFile temp-path)
                  proof-file (io/file temp-dir "Main.lean")
                  ;; Read the target's name from the source rather than
                  ;; constructing it. The corpus is not consistent about case:
                  ;; a98A02 declares apm_a98a02 and a97J07 declares apm_a97j07,
                  ;; but a96J08 declares apm_a96J08. Constructing the lowercase
                  ;; form asked for a theorem that does not exist, `#print
                  ;; axioms` errored, and the gate refused a proof that was in
                  ;; fact complete and axiom-clean -- a false refusal on f46.
                  theorem (or (last (map second
                                         (re-seq #"(?m)^(?:theorem|lemma)\s+(apm_[A-Za-z0-9_']+)"
                                                 (str (:out shown)))))
                              (str "apm_" (str/lower-case problem-id)))]
              (try
                (spit proof-file (str (:out shown) "\n#print axioms " theorem "\n"))
                (let [elaboration (run-lean repo proof-file)
                      axioms (axiom-list elaboration)]
                  (cond
                    (.exists project-mathlib)
                    {:status :refused :reason :mathlib-project-olean-created}

                    (not (zero? (:exit elaboration)))
                    {:status :refused :reason :elaboration-failed
                     :exit (:exit elaboration)}

                    (some #{"sorryAx"} axioms)
                    {:status :refused :reason :sorry-ax :axioms axioms}

                    (not= allowed-proof-axioms axioms)
                    {:status :refused :reason :unexpected-axioms :axioms axioms}

                    :else
                    (let [ref (str "refs/apm/banked-solves/" frame "/"
                                   problem-id "/" head)
                          pinned (git repo "update-ref" ref head)]
                      (if (zero? (:exit pinned))
                        {:status :pinned :ref ref}
                        {:status :refused :reason :pin-failed
                         :exit (:exit pinned)}))))
                (finally
                  (doseq [file (reverse (file-seq temp-dir))]
                    (io/delete-file file true)))))))))))
