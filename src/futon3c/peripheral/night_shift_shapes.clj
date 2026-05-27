(ns futon3c.peripheral.night-shift-shapes
  "Static shapes and invariants for the Night Shift peripheral.

   E-night-shift v0 constrains code modification by structure rather than
   discipline: branch naming, protected-branch exclusion, commit provenance,
   secret-path blocking, and PR-as-deliverable all bind here as closed sets
   and predicates that the backend/peripheral consults."
  (:require [clojure.string :as str]))

(def branch-pattern
  #"^e-night-shift/[^/]+/[^/]+$")

(def protected-branches
  #{"main" "master" "trunk" "develop" "prod" "production"})

(def default-commit-cap
  50)

(def default-stale-days
  14)

(def secret-patterns
  [#"(?:^|/)\.env(?:\..+)?$"
   #"(?:^|/)\.envrc$"
   #"\.pem$"
   #"credentials"
   #"(?:^|/)\.admintoken$"
   #"\.key$"])

(def substantive-tools
  #{:frame-provision
    :branch-create
    :edit-file
    :create-file
    :repo-stage
    :repo-commit
    :run-tests
    :push-feature-branch
    :pr-create})

(def ambient-tools
  #{:list-agendas
    :repo-scan
    :read-file
    :bell-emit
    :hop-in
   :hop-out
   :reap-check})

(def tool-operation-kinds
  {;; ambient/read-only or coordination
   :list-agendas         :observe
   :repo-scan            :observe
   :read-file            :observe
   :bell-emit            :action
   :hop-in               :action
   :hop-out              :action
   :reap-check           :action
   ;; substantive
   :frame-provision      :action
   :branch-create        :action
   :edit-file            :action
   :create-file          :action
   :repo-stage           :action
   :repo-commit          :action
   :run-tests            :action
   :push-feature-branch  :action
   :pr-create            :action})

(def phase-tools
  (set (keys tool-operation-kinds)))

(defn valid-feature-branch?
  [branch-name]
  (boolean (and (string? branch-name)
                (re-matches branch-pattern branch-name))))

(defn branch-segment
  [text fallback]
  (let [candidate (-> (str text)
                      str/lower-case
                      (str/replace #"[^a-z0-9]+" "-")
                      (str/replace #"(^-+|-+$)" ""))]
    (if (seq candidate) candidate fallback)))

(defn protected-branch?
  [branch-name]
  (contains? protected-branches (str branch-name)))

(defn secret-path?
  [path]
  (let [path-str (str path)
        lower (str/lower-case path-str)]
    (boolean (some #(re-find % lower) secret-patterns))))
