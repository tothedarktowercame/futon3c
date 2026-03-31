(ns futon3c.dev.apm-frames
  "Owner-side proof-frame workspace integration for APM conductors.

   The frame receipt/workspace model lives in futon6. This namespace is the
   narrow bridge that lets futon3c conductors allocate isolated workspaces
   without reimplementing the owner-side path logic locally."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.time Instant]))

(def ^:private futon6-root
  "/home/joe/code/futon6")

(def ^:private init-workspace-script
  (str futon6-root "/scripts/frontiermath/init-proof-frame-workspace.py"))

(def ^:private emit-receipt-script
  (str futon6-root "/scripts/frontiermath/emit-proof-frame-receipt.py"))

(defn- trim-to-nil [s]
  (let [s (some-> s str str/trim)]
    (when (seq s) s)))

(defn- sanitize-segment [raw]
  (-> (or raw "")
      (str/replace #"[^A-Za-z0-9._-]" "-")
      (str/replace #"-+" "-")
      (str/replace #"^-|-$" "")))

(defn frame-id
  [conductor-tag problem-base]
  (str (sanitize-segment conductor-tag)
       "-"
       (sanitize-segment problem-base)
       "-"
       (.toEpochMilli (Instant/now))))

(defn read-workspace-metadata
  [path]
  (when-let [path (some-> path str io/file)]
    (when (.exists path)
      (json/parse-string (slurp path) true))))

(defn init-frame-workspace!
  [{:keys [problem-base conductor-tag frame-id-override]
    :or {conductor-tag "apm"}}]
  (let [resolved-frame-id (or (trim-to-nil frame-id-override) (frame-id conductor-tag problem-base))
        {:keys [exit out err]}
        (shell/sh "python3" init-workspace-script
                  "--problem-id" (str problem-base)
                  "--frame-id" resolved-frame-id)]
    (if (zero? exit)
      (let [metadata-path (trim-to-nil out)
            metadata (read-workspace-metadata metadata-path)]
        (if (map? metadata)
          metadata
          (throw (ex-info "Frame workspace metadata unreadable"
                          {:problem-base problem-base
                           :frame-id resolved-frame-id
                           :metadata-path metadata-path}))))
      (throw (ex-info "Frame workspace initialization failed"
                      {:problem-base problem-base
                       :frame-id resolved-frame-id
                       :exit exit
                       :stderr err})))))

(defn emit-frame-receipt!
  [{:keys [problem-base frame-workspace cycle-id blocker-id frame-label]
    :or {blocker-id "root"}}]
  (let [metadata-path (get-in frame-workspace [:artifacts :workspace-metadata])
        frame-id (get frame-workspace :frame/id)
        cmd (cond-> ["python3" emit-receipt-script
                     "--problem-id" (str problem-base)
                     "--frame-id" (str frame-id)
                     "--boundary-kind" "frame"
                     "--workspace-metadata" (str metadata-path)]
              frame-label (into ["--frame-label" frame-label])
              cycle-id (into ["--cycle-id" (str cycle-id)])
              blocker-id (into ["--blocker-id" (str blocker-id)]))
        {:keys [exit out err]} (apply shell/sh cmd)]
    (if (zero? exit)
      (trim-to-nil out)
      (throw (ex-info "Frame receipt emission failed"
                      {:problem-base problem-base
                       :frame-id frame-id
                       :exit exit
                       :stderr err})))))

(defn frame-paths
  [frame-workspace]
  (let [artifacts (:artifacts frame-workspace)]
    {:workspace-root (get frame-workspace :frame/workspace-root)
     :module-root (get frame-workspace :frame/module-root)
     :lean-root (get frame-workspace :frame/lean-root)
     :shared-extension-root (get frame-workspace :frame/shared-extension-root)
     :proof-plan-path (get artifacts :proof-plan)
     :changelog-path (get artifacts :changelog)
     :execute-notes-path (get artifacts :execute-notes)
     :workspace-readme-path (get artifacts :workspace-readme)
     :workspace-metadata-path (get artifacts :workspace-metadata)
     :lean-main-path (get artifacts :lean-main)
     :lean-scratch-path (get artifacts :lean-scratch)}))

(defn workspace-sidecar-path
  [frame-workspace phase]
  (case phase
    :execute (get-in frame-workspace [:artifacts :execute-notes])
    nil))

(defn proof-plan-path
  [frame-workspace]
  (get-in frame-workspace [:artifacts :proof-plan]))

(defn changelog-path
  [frame-workspace]
  (get-in frame-workspace [:artifacts :changelog]))

(defn workspace-lean-artifacts
  [frame-workspace]
  (->> [(get-in frame-workspace [:artifacts :lean-main])
        (get-in frame-workspace [:artifacts :lean-scratch])]
       (keep trim-to-nil)
       (filter #(try (.exists (io/file %)) (catch Exception _ false)))
       distinct
       vec))

(defn prompt-context
  [frame-workspace]
  (when frame-workspace
    (frame-paths frame-workspace)))
