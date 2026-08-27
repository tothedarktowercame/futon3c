(ns futon3c.apm.bank-audit
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def ^:private proof-path-template "problems/%s/lean/Main.lean")
(def ^:private clean-axioms ["propext" "Classical.choice" "Quot.sound"])

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
  "Classify solved frame receipts by comparing solver-head proof content to master."
  [{:keys [campaign-dir read-at-rev master-rev]
    ;; origin/master, not master. bank-sweep branches from origin/master and
    ;; pushes there, and never advances the local branch — so comparing against
    ;; local master reported f44 :unbanked immediately after the sweep had
    ;; banked and pushed it. Two tools disagreeing about which ref is "master"
    ;; reads as work lost when it is not. Overridable for tests and for a
    ;; repo with no remote.
    :or {read-at-rev default-read-at-rev master-rev "origin/master"}}]
  (->> (terminal-receipts campaign-dir)
       (filter #(= :solved (:problem/outcome %)))
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

(defn- axiom-list
  [{:keys [out err]}]
  (when-let [[_ axioms]
             (re-find #"(?s)depends on axioms:\s*\[([^]]*)\]"
                      (str out "\n" err))]
    (->> (str/split axioms #",")
         (mapv str/trim))))

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

                    (not= clean-axioms axioms)
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
