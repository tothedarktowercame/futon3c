#!/usr/bin/env bb
(ns generate-intent-vocabulary
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def root (.getCanonicalFile (io/file (or (System/getenv "FUTON3C_ROOT") "."))))
(def futon3-root (.getCanonicalFile
                  (io/file (or (System/getenv "FUTON3_ROOT") "/home/joe/code/futon3"))))
(def reply-proforma (.getCanonicalFile
                     (io/file (or (System/getenv "REPLY_PROFORMA")
                                  "/home/joe/code/reply-proforma.md"))))
(def vocab (edn/read-string (slurp (io/file root "resources/arxana/intent-vocabulary-v1.edn"))))
(def intents (:intents vocab))

(defn replace-region [text begin end body]
  (let [start (.indexOf text begin)
        finish (.indexOf text end)]
    (when (or (neg? start) (neg? finish) (< finish start))
      (throw (ex-info "generated region markers missing" {:begin begin :end end})))
    (str (subs text 0 (+ start (count begin))) "\n" body "\n"
         (subs text finish))))

(defn cljs-body []
  (str "(def intent-mark\n  {"
       (str/join " " (map (fn [{:keys [id mark]}] (str (pr-str id) " " (pr-str mark))) intents))
       "})"))

(defn elisp-body []
  (str "(defconst xiaoxiang-preview-marks\n  '("
       (str/join " " (map (fn [{:keys [id mark]}] (str "(" (pr-str id) " . " (pr-str mark) ")")) intents))
       ")\n  \"Generated from intent-vocabulary/v1; do not edit this region.\")"))

(defn markdown-body []
  (let [pairs (partition-all 2 intents)]
    (str "| Mark | Intent | | Mark | Intent |\n|---|---|---|---|---|\n"
         (str/join "\n"
                   (map (fn [[a b]]
                          (str "| " (:mark a) " | " (:gloss a) " | | "
                               (or (:mark b) "") " | " (or (:gloss b) "") " |"))
                        pairs)))))

(def targets
  [{:path (io/file futon3-root "src-cljs/futon3/turnfeed/core.cljs")
    :begin ";; BEGIN GENERATED INTENT-VOCABULARY/V1"
    :end ";; END GENERATED INTENT-VOCABULARY/V1"
    :body cljs-body}
   {:path (io/file root "emacs/xiaoxiang-preview.el")
    :begin ";; BEGIN GENERATED INTENT-VOCABULARY/V1"
    :end ";; END GENERATED INTENT-VOCABULARY/V1"
    :body elisp-body}
   {:path reply-proforma
    :begin "<!-- BEGIN GENERATED INTENT-VOCABULARY/V1 -->"
    :end "<!-- END GENERATED INTENT-VOCABULARY/V1 -->"
    :body markdown-body}])

(def check? (some #{"--check"} *command-line-args*))

(doseq [{:keys [path begin end body]} targets]
  (let [before (slurp path)
        after (replace-region before begin end (body))]
    (if check?
      (when-not (= before after)
        (binding [*out* *err*]
          (println "DRIFT" (.getCanonicalPath path)))
        (System/exit 1))
      (spit path after))))

(println (if check? "intent-vocabulary/v1 presentations agree" "intent-vocabulary/v1 presentations generated"))
