(ns futon3c.aif.emacs-bridge
  "Bridge from the Web War Machine to the user's running Emacs server.

   Resolves a clicked AIF+ node's underlying VSAT-shaped Markdown story and
   asks Emacs to open it in the existing VSATARCS reader (`futon4/dev/
   arxana-browser-vsatarcs.el`).  No new Emacs UI code is required — the
   bridge translates a node identifier to a story file path and invokes
   `arxana-vsatarcs-open-file`.

   Architectural note: this namespace is deliberately *outside* the transport
   layer (futon3c.transport.http) to preserve invariant I-2 (transport
   routes, it does not create).  The HTTP handler in transport/http.clj
   resolves this namespace's `open-in-vsatarcs` via `requiring-resolve` and
   delegates — no transport-side dependency on subprocess machinery.

   Path safety: the requested leaf name is constrained to the
   ~/code/futon5a/holes/stories/ directory and validated against a
   conservative regex.  Anything else returns a 4xx via the handler
   without spawning anything.

   Failure surfacing: emacsclient stderr is captured and returned in the
   response on non-zero exit.  No silent swallowing of errors."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

(def ^:private workspace-root
  (io/file (System/getProperty "user.home")
           "code"))

(def ^:private stories-dir
  (io/file workspace-root
           "futon5a" "holes" "stories"))

(def ^:private safe-name-re
  ;; Conservative whitelist: lowercase letters, digits, hyphen, dot.
  ;; Rejects path traversal characters (/, .., backslash) entirely.
  #"^[a-z0-9][a-z0-9.\-]*$")

(defn- safe-leaf-name? [s]
  (and (string? s)
       (boolean (re-matches safe-name-re s))
       (not (str/includes? s ".."))))

(def ^:private safe-rel-path-re
  #"^[A-Za-z0-9][A-Za-z0-9._/\-]*$")

(defn- safe-relative-path? [s]
  (and (string? s)
       (boolean (re-matches safe-rel-path-re s))
       (not (str/starts-with? s "/"))
       (not (str/includes? s ".."))))

(defn resolve-workspace-file
  "Return a java.io.File for REL-PATH under ~/code, or nil when it fails
   validation, does not exist, or escapes the workspace root."
  [rel-path]
  (when (safe-relative-path? rel-path)
    (let [f (io/file workspace-root rel-path)
          canonical (.getCanonicalFile ^File f)
          root-canonical (.getCanonicalFile ^File workspace-root)]
      (when (and (.exists canonical)
                 (str/starts-with? (.getPath canonical)
                                   (str (.getPath root-canonical) "/")))
        canonical))))

(defn resolve-story-file
  "Return a java.io.File for LEAF-NAME's .md story under stories-dir, or nil
   when the name fails validation or the file does not exist.  Two name
   forms are accepted: 'leaf-invariants' (no extension; '.md' is appended)
   and 'leaf-invariants.md' (extension already present)."
  [leaf-name]
  (when (safe-leaf-name? leaf-name)
    (let [filename (if (str/ends-with? leaf-name ".md")
                     leaf-name
                     (str leaf-name ".md"))
          f (io/file stories-dir filename)
          canonical (.getCanonicalFile ^File f)
          stories-canonical (.getCanonicalFile ^File stories-dir)]
      ;; Final containment check — canonical path must live under stories-dir.
      (when (and (.exists canonical)
                 (str/starts-with? (.getPath canonical)
                                   (str (.getPath stories-canonical) "/")))
        canonical))))

(defn- run-emacsclient
  "Invoke emacsclient -n with the given lisp form. Returns
     {:ok? bool :exit-code int :stdout str :stderr str}.
   When emacsclient is missing on PATH, returns
     {:ok? false :emacsclient-missing? true :stderr msg}.
   Hard timeout after 5 s so a wedged Emacs cannot hang the request."
  [lisp-form]
  (try
    (let [pb (ProcessBuilder. ["emacsclient" "-n" "-e" lisp-form])
          _  (.redirectErrorStream pb false)
          p  (.start pb)
          finished? (.waitFor p 5 TimeUnit/SECONDS)]
      (when-not finished?
        (.destroy p))
      {:ok?       (and finished? (zero? (.exitValue p)))
       :exit-code (when finished? (.exitValue p))
       :stdout    (slurp (.getInputStream p))
       :stderr    (slurp (.getErrorStream p))
       :timed-out? (not finished?)})
    (catch java.io.IOException e
      ;; Most likely cause: emacsclient not on PATH.
      {:ok? false
       :emacsclient-missing? true
       :stderr (str "emacsclient not found on PATH: " (.getMessage e))})
    (catch Exception e
      {:ok? false
       :stderr (str (.getName (class e)) ": " (.getMessage e))})))

(defn- escape-elisp-string
  "Escape a Clojure string for embedding inside an elisp double-quoted string."
  [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn open-workspace-file
  "Open REL-PATH under ~/code in the running Emacs via plain `find-file`.
   Returns a result map suitable for JSON responses."
  [rel-path]
  (if-let [^File f (resolve-workspace-file rel-path)]
    (let [path (.getPath f)
          form (str "(find-file \""
                    (escape-elisp-string path)
                    "\")")
          result (run-emacsclient form)]
      (assoc result
             :kind "workspace-file"
             :path rel-path
             :resolved-path path))
    {:ok? false
     :error "workspace-file-not-found"
     :path rel-path}))

(defn open-in-vsatarcs
  "Open LEAF-NAME's story in the running Emacs's VSATARCS reader.
   When SCENE-ANCHOR is provided, also call (arxana-vsatarcs-goto ...).

   Returns a result map suitable for a JSON response (see run-emacsclient).
   When the requested leaf cannot be resolved, returns
     {:ok? false :error 'leaf-not-found' :leaf leaf-name}."
  [leaf-name & {:keys [scene-anchor]}]
  (if-let [^File f (resolve-story-file leaf-name)]
    (let [path (.getPath f)
          ;; arxana-vsatarcs-open-file takes a single PATH argument; the
          ;; optional return-buffer / return-config slots are managed by the
          ;; reader internally.
          open-form (str "(arxana-vsatarcs-open-file \""
                         (escape-elisp-string path)
                         "\")")
          form (if (and scene-anchor (string? scene-anchor) (seq scene-anchor))
                 (str "(progn " open-form
                      " (arxana-vsatarcs-goto \""
                      (escape-elisp-string scene-anchor)
                      "\"))")
                 open-form)
          result (run-emacsclient form)]
      (assoc result
             :kind "vsatarcs-story"
             :leaf leaf-name
             :path path))
    {:ok? false
     :error "leaf-not-found"
     :leaf leaf-name}))

(defn open-target
  "Dispatch a generic Emacs opening target.

   Supported shapes:
   {:kind \"vsatarcs-story\" :leaf \"leaf-argument\" :scene-anchor \"optional\"}
   {:kind \"workspace-file\" :path \"futon3/holes/strategy/globe1-market-interface.devmap\"}"
  [{:keys [kind leaf scene-anchor path]}]
  (case (some-> kind str)
    "workspace-file" (open-workspace-file path)
    "vsatarcs-story" (open-in-vsatarcs leaf :scene-anchor scene-anchor)
    ;; Backward-compatible fallback for the pre-generalization caller shape.
    (if leaf
      (open-in-vsatarcs leaf :scene-anchor scene-anchor)
      {:ok? false
       :error "unsupported-target-kind"
       :kind kind})))
