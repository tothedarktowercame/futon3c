(ns futon3c.wm.run4-deployment-preflight
  "Read-only validation of a disabled RUN4 deployment template."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-pinned-run-config :as pinned]))

(defn- one [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [v (edn/read {:eof ::empty} r)]
      (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
        (throw (ex-info "invalid EDN" {:reason :malformed-edn}))) v)))

(defn- read-authority [root allowlist]
  (fn [ref]
    (when-not (and (string? ref) (contains? allowlist ref))
      (throw (ex-info "not allowlisted" {:reason :not-allowlisted :ref ref})))
    (let [base (.getCanonicalFile (io/file root))
          f (.getCanonicalFile (io/file base ref))]
      (when-not (and (.startsWith (.toPath f) (.toPath base)) (.isFile f))
        (throw (ex-info "unreadable authority" {:reason :unreadable :ref ref})))
      (slurp f))))

(defn inspect
  "Returns categorized facts only; creates no roots and invokes no service."
  [template-text]
  (let [t (one template-text)
        root (:authority-root t)
        manifest-ref (get-in t [:manifest :ref])
        allowlist (into (:source-allowlist t) (:pin-allowlist t))
        allowlist (conj allowlist manifest-ref)
        read-text (read-authority root allowlist)
        manifest-text (read-text manifest-ref)
        manifest-sha (digest/sha256 manifest-text)
        manifest (one manifest-text)
        trial (first (:trials manifest))
        pin-text (read-text (get-in trial [:packet :path]))
        pin (one pin-text)
        config-pin (:config pin)
        opts (pinned/load! config-pin read-text)
        pinned-sources (concat (:source-pins manifest) (:sources pin) [config-pin])
        all-current? (every? (fn [{:keys [path sha256]}]
                               (= sha256 (digest/sha256 (read-text path))))
                             pinned-sources)
        missing-roots (->> (:stores t) (keep (fn [[k p]] (when-not (.isDirectory (io/file p)) k))) vec)
        mission-ref (some #(when (str/includes? % "M-u88-contextual-preferences.md") %) (:source-allowlist t))
        mission-text (read-text mission-ref)]
    {:schema :wm/run4-deployment-preflight-v1
     :template-disabled (and (false? (:enabled? t))
                             (false? (get-in t [:serving :automatic-start?])))
     :credential :unprovisioned
     :sources (if (and (= manifest-sha (get-in t [:manifest :sha256]))
                       (= (digest/sha256 pin-text) (:pin-sha256 trial))
                       all-current?) :current :drift)
     :declaration (if (:run4/serving-declaration opts) :supported :missing)
     :consumer-state :unknown-not-loaded
     :roots (if (empty? missing-roots) :present {:missing missing-roots})
     :mission (if (str/includes? mission-text "Status: OPEN")
                :open-activated :unexpected-state)
     :route (get-in t [:serving :route])
     :eligible-to-launch? false}))

(defn -main [& [path]]
  (println (pr-str (inspect (slurp path)))))
