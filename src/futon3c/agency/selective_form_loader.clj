(ns futon3c.agency.selective-form-loader
  "Experimental offline form loader. Serving activation and production
  namespace targets are deliberately disabled; use a controlled restart."
  (:import (java.io PushbackReader StringReader)
           (java.nio.charset StandardCharsets) (java.nio.file Files Path)
           (java.security MessageDigest)))

(def http-sha256 "defb1ed3b0deeb16ba15857f8efe5ac4e53ed6c442723dfae8808b38406f65fb")
(def required-http-forms
  '[compact-terminal-job request-commission-archive-record invoke-commission-archive-dir
    commission-archive-path validate-commission-archive! read-commission-archive
    force-directory! force-commission-archive-directories!
    *force-commission-archive-directories!* persist-commission-archive!
    *persist-commission-archive!* archive-expired-jobs! compact-invoke-jobs-ledger
    normalized-invoke-commission invoke-job-request-digest invoke-job-request-commission
    create-invoke-job! bind-unbound-invoke-request!])
(def required-aliases '#{io edn campaign-machine parked-on coordination-ledger bb reg str})
(def required-classes
  '#{Instant BufferedWriter FileOutputStream OutputStreamWriter PushbackReader
     FileChannel StandardCharsets Files Path StandardCopyOption StandardOpenOption})

(defn- refuse! [code data] (throw (ex-info (name code) (assoc data :refusal code))))
(defn- digest [bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))
(defn- form-name [form]
  (when (and (seq? form) (#{'def 'defn 'defn-} (first form))) (second form)))
(defn read-pinned-forms [path expected-sha]
  (let [bs (try (Files/readAllBytes (Path/of path (make-array String 0)))
                (catch Throwable e (throw (ex-info "source read failed" {:refusal :loader/source-io} e))))
        observed (digest bs)]
    (when-not (= expected-sha observed)
      (refuse! :loader/source-sha-mismatch {:expected expected-sha :observed observed}))
    (try
      (with-open [r (PushbackReader. (StringReader. (String. bs StandardCharsets/UTF_8)))]
        (loop [out []]
          (let [form (read {:eof ::eof} r)]
            (if (= ::eof form) out (recur (conj out form))))))
      (catch Throwable e (throw (ex-info "source parse failed" {:refusal :loader/source-parse} e))))))
(defn select-exact-forms [forms required]
  (let [by-name (group-by form-name forms)
        missing (set (remove #(= 1 (count (get by-name %))) required))
        unexpected (set (remove (set required) (filter some? (map form-name forms))))]
    (when (seq missing) (refuse! :loader/allowlist-missing {:missing missing}))
    {:forms (mapv #(first (get by-name %)) required)
     :ignored-form-names unexpected}))
(defn preflight!
  ([target-ns ingress-proof] (preflight! target-ns ingress-proof required-aliases required-classes))
  ([target-ns ingress-proof aliases-required classes-required]
  (when-not (find-ns target-ns) (refuse! :loader/target-namespace-missing {:namespace target-ns}))
  (let [aliases (set (keys (ns-aliases (the-ns target-ns))))
        classes (set (keys (ns-imports (the-ns target-ns))))]
    (when-let [missing (seq (remove aliases aliases-required))]
      (refuse! :loader/dependency-alias-missing {:missing (set missing)}))
    (when-let [missing (seq (remove classes classes-required))]
      (refuse! :loader/dependency-class-missing {:missing (set missing)})))
  (when-not (and (= :agency/invoke-ingress-quiescence-v1 (:schema ingress-proof))
                 (= :independently-measured (:authority ingress-proof))
                 (true? (:admission-rejected? ingress-proof))
                 (zero? (:active-creations ingress-proof -1))
                 (zero? (:waiting-creations ingress-proof -1))
                 (true? (:all-creation-surfaces-covered? ingress-proof)))
    (refuse! :loader/ingress-quiescence-unproved {:proof ingress-proof}))
  true))
(defn current-serving-preflight
  "Honest status at HEAD: queue hold accepts bells and no pre-create ingress
  fence covers every call site, so selective live activation must refuse."
  [] {:schema :agency/invoke-ingress-quiescence-v1 :authority :source-census
      :admission-rejected? false :active-creations :unknown :waiting-creations :unknown
      :all-creation-surfaces-covered? false
      :refusal :loader/ingress-fence-unavailable})
(defn load-transactionally!
  [{:keys [target-ns forms required ingress-proof fail-after aliases-required classes-required
           offline-experimental?]
    :or {aliases-required required-aliases classes-required required-classes}}]
  (when-not offline-experimental?
    (refuse! :loader/offline-experimental-opt-in-required {}))
  (when (.startsWith (str target-ns) "futon3c.")
    (refuse! :loader/production-namespace-disabled {:namespace target-ns}))
  (preflight! target-ns ingress-proof aliases-required classes-required)
  (let [{selected :forms} (select-exact-forms forms required)
        ns-obj (the-ns target-ns)
        before (into {} (map (fn [s] [s (when-let [v (ns-resolve ns-obj s)]
                                          {:var v :root (var-get v) :meta (meta v)})]) required))]
    (try
      (binding [*ns* ns-obj]
        (doseq [[idx form] (map-indexed vector selected)]
          (eval form)
          (when (= (inc idx) fail-after) (throw (ex-info "induced partial load" {:stage idx})))))
      {:status :loaded
       :forms (mapv (fn [form] {:name (form-name form)
                                :line (:line (meta form))
                                :form-sha256 (digest (.getBytes (pr-str form) StandardCharsets/UTF_8))}) selected)}
      (catch Throwable e
        (doseq [s required]
          (if-let [{:keys [var root meta]} (before s)]
            (do (alter-var-root var (constantly root)) (reset-meta! var meta))
            (ns-unmap ns-obj s)))
        (throw (ex-info "selective load rolled back"
                        {:refusal :loader/partial-load-rolled-back
                         :restored (vec required)} e))))))

(defn activate-http-retention!
  "Historical entry retained only to give every attempted live activation a
  typed refusal. Caller assertions cannot authorize mutation of the server."
  [_]
  (refuse! :loader/live-activation-disabled
           {:required-mechanism :controlled-process-restart}))
