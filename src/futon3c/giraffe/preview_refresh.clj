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
   :preview-script "scripts/gcc_preview.py"})

(defn default-exec
  "One helper process. Reads sources read-only; the only writes are the page
   files under the configured out-dir."
  [& args]
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

(defn refresh!
  "Both page refreshes as one summary {:ok? :items :error}. A failed script
   is reported, never thrown; the caller records it in health."
  [{:keys [out-dir] :as cfg} run-dir items]
  (if-not run-dir
    {:ok? false :items [] :error "no giraffe run id for preview refresh"}
    (let [party (run-party! cfg run-dir)]
      (if-not (:ok? party)
        {:ok? false :items [] :error (str "giraffe_party.py failed: " (:error party))}
        (let [items (vec (distinct (concat items (party-top-items out-dir))))]
          (if (empty? items)
            {:ok? true :items []}
            (let [preview (run-preview! cfg items)]
              (cond-> {:ok? (:ok? preview) :items items}
                (:error preview) (assoc :error
                                        (str "gcc_preview.py failed: " (:error preview)))))))))))
