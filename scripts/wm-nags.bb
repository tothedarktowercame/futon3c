#!/usr/bin/env bb
;; wm-nags.bb — read / dismiss the War Machine's pattern-warranted NAGs.
;;   list           → emit the NAGs as an elisp-readable list of plists (for war-machine-nags.el)
;;   dismiss <id>   → remove the NAG with :id from needs-you.edn (transient: a later WM run
;;                    re-emits any still-unresolved NAG)
(require '[clojure.edn :as edn]
         '[clojure.string :as str])

(def needs-you-file
  (str (System/getProperty "user.home") "/code/futon3c/data/wm/needs-you.edn"))

(defn load-nags []
  (try (vec (edn/read-string (slurp needs-you-file)))
       (catch Exception _ [])))

(defn kw->str [k] (when k (if (keyword? k) (subs (str k) 1) (str k))))

(defn ->plist [it]
  (let [pw (:pattern-warrant it)]
    (format "(:id %s :title %s :class %s :pattern %s :warrant %s :gap %s :unblock %s)"
            (pr-str (str (:id it)))
            (pr-str (str (:title it)))
            (pr-str (or (kw->str (:wm-action-class it)) ""))
            (pr-str (or (kw->str (:pattern-id pw)) ""))
            (pr-str (str (or (:warrant pw) "")))
            (pr-str (str (or (:gap pw) "")))
            (pr-str (str (or (:unblock-action it) (:unblock pw) ""))))))

(case (first *command-line-args*)
  "list"
  (let [nags (load-nags)]
    (println (str "(" (str/join "\n " (map ->plist nags)) ")")))

  "dismiss"
  (let [id (second *command-line-args*)
        nags (load-nags)
        remaining (vec (remove #(= id (str (:id %))) nags))]
    (spit needs-you-file (pr-str remaining))
    (println (format "(:dismissed %s :remaining %d)" (pr-str id) (count remaining))))

  (println "(:error \"usage: wm-nags.bb list|dismiss <id>\")"))
