(ns futon3c.giraffe.preview-refresh
  "Refresh Mojira's GCC pages after a Giraffe mint (M-repl-circus).
   Helper processes only, never agents: giraffe_party.py against the new
   run, then gcc_preview.py --source futon1b for the items that changed
   since the last refresh plus the new party top list. Everything a page
   needs is read by the scripts; this namespace only launches and summarizes.
   The process launch is injectable (:exec-fn) so the service tests it
   without helper processes."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def default-config
  {:python "python3"
   :repo-dir (str (System/getProperty "user.home") "/code/futon3c")
   :out-dir (str (System/getProperty "user.home") "/storage/gcc-preview")
   :party-script "scripts/giraffe_party.py"
   :conformance-script "scripts/giraffe_party_conformance.py"
   :preview-script "scripts/gcc_preview.py"})

(defn default-exec
  "One helper process. Reads sources read-only; the only writes are the page
   files under the configured out-dir."
  [args]
  ;; Takes the command as ONE vector, as `run` calls it. The first live
  ;; refresh (2026-11-15) failed with "No value supplied for key: [...]"
  ;; because this took varargs and passed the vector to sh as a single
  ;; argument; the tests had replaced exec-fn, so only the live run hit it.
  (apply shell/sh args))

(defn- run [{:keys [exec-fn] :as _cfg} args]
  (let [{:keys [exit out err]} ((or exec-fn default-exec) (vec args))]
    {:ok? (zero? exit)
     :error (when-not (zero? exit)
              (str/trim (str (or err "") \space (or out ""))))}))

(defn- command [{:keys [python repo-dir] :as cfg} script args]
  (into [python (str (io/file repo-dir (get cfg script)))] args))

(defn run-party!
  "giraffe_party.py against RUN-DIR; writes party.json under the out-dir."
  [cfg run-dir]
  (run cfg (command cfg :party-script ["--run" run-dir "--out" (:out-dir cfg)])))

(defn party-top-items
  "Item ids of the party top list just written by giraffe_party.py.
   A missing or unreadable party.json is an empty list, not an error: the
   caller decides whether zero items still deserves a preview run."
  [out-dir]
  (try
    (vec (keep :item (get-in (json/parse-string (slurp (io/file out-dir "party.json")) true)
                             [:party :top])))
    (catch Exception _ [])))

(defn run-preview!
  "gcc_preview.py --source futon1b for ITEMS (changed ∪ party top)."
  [cfg items]
  (run cfg (command cfg :preview-script
                    ["--source" "futon1b" "--items" (str/join "," (sort items))
                     "--out" (:out-dir cfg)])))

(defn- run-conformance! [cfg run-dir]
  (let [file (io/file (:out-dir cfg) "conformance.json")]
    (try
      ;; Never mistake a previous successful report for this process's output.
      (when (.exists file) (io/delete-file file))
      (let [result ((or (:exec-fn cfg) default-exec)
                    (command cfg :conformance-script
                             [(str (io/file (:out-dir cfg) "party.json"))
                              "--run" run-dir "--json-out" (str file)]))
            report (json/parse-string (slurp file) true)]
        (when-not (and (#{0 1} (:exit result))
                       (map? (:summary report)) (vector? (:failing report))
                       (every? #(nat-int? (get-in report [:summary %])) [:PASS :FAIL :ABSENT])
                       (= (:exit result) (if (seq (:failing report)) 1 0)))
          (throw (ex-info "Conformance process failed or report invalid" {})))
        {:conformance (select-keys report [:summary :failing])})
      (catch Exception e {:error (str "conformance: " (.getMessage e))}))))

(defn refresh!
  "Party, conformance, then preview. A conformance FAIL is a reported result;
   a crashed checker/unreadable report is an error, but preview still runs."
  [{:keys [out-dir] :as cfg} run-dir items]
  (if-not run-dir
    {:ok? false :items [] :error "no giraffe run id for preview refresh"}
    (let [party (run-party! cfg run-dir)]
      (if-not (:ok? party)
        {:ok? false :items [] :error (str "giraffe_party.py failed: " (:error party))}
        (let [conformance (run-conformance! cfg run-dir)
              items (vec (distinct (concat items (party-top-items out-dir))))
              preview (if (seq items) (run-preview! cfg items) {:ok? true})
              errors (remove nil? [(:error conformance)
                                   (when (:error preview) (str "gcc_preview.py failed: " (:error preview)))])]
          (cond-> (merge {:ok? (and (:ok? preview) (empty? errors)) :items items}
                         (select-keys conformance [:conformance]))
            (seq errors) (assoc :error (str/join "; " errors))))))))
