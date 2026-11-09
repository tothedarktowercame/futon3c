(ns futon3c.arxana.codedocs-demo
  "Private pinned code↔docs stack for the terminal Emacs client."
  (:require [clojure.edn :as edn]
            [futon3c.arxana.codedocs :as codedocs]
            [futon3c.arxana.codedocs-relocation :as relocation]
            [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.service :as service])
  (:import [java.lang ProcessHandle]
           [java.nio.charset StandardCharsets]))

(def default-port 7199)
(def fixture-path "test/resources/arxana/codedocs/fixture.edn")
(def roots {:futon2 "/home/joe/code/futon2" :futon4 "/home/joe/code/futon4"
            :mathlib4 "/home/joe/code/mathlib4"})

(defn- git-text [repo sha path]
  (let [p (.start (ProcessBuilder. (into-array String ["git" "-C" repo "show" (str sha ":" path)])))
        bytes (.readAllBytes (.getInputStream p))]
    (when-not (zero? (.waitFor p)) (throw (ex-info "git show failed" {:repo repo :sha sha :path path})))
    (String. bytes StandardCharsets/UTF_8)))

(defn prepare! [h]
  (let [cfg (:cfg h) repo (corr/in-memory-repository) f (edn/read-string (slurp fixture-path))
        old-sha (get-in f [:historical :repo/sha]) new-sha (get-in f [:current :repo/sha])
        code (into {} (for [[path symbols] (:symbols f)
                            :let [plan {:artifact-id (str "codedocs:futon2/" path) :symbols symbols
                                        :historical {:sha old-sha :text (git-text (:futon2 roots) old-sha path)}
                                        :current {:sha new-sha :text (git-text (:futon2 roots) new-sha path)}}]]
                        [path (codedocs/ingest-file! cfg repo plan)]))
        current-by-path (into {} (map (fn [[p vs]] [p (:scopes (second vs))]) code))
        current-by-symbol (into {} (for [[_ scopes] current-by-path [s x] scopes] [s (:scope x)]))
        doc-sha (get-in f [:docbook :repo/sha])
        docs (vec (for [[path _] (get-in f [:docbook :files])
                        :let [text (git-text (:futon4 roots) doc-sha path)
                              source (second (re-find #"(?m)^:SOURCE_PATH:\s*(.*?)\s*$" text))]]
                    (codedocs/ingest-doc! cfg {:artifact-id (str "codedocs:futon4/" path)
                                               :sha doc-sha :text text
                                               :symbols (get-in f [:symbols source])})))
        _ (codedocs/link-code-docs! cfg current-by-path docs)
        lean-sha (get-in f [:lean :repo/sha])
        lean-text (git-text (:mathlib4 roots) lean-sha (get-in f [:lean :file]))
        _ (codedocs/ingest-lean! cfg {:artifact-id "codedocs:mathlib4/SelfHealRoute.lean"
                                      :sha lean-sha :text lean-text
                                      :symbols (set (keys current-by-symbol))
                                      :code-scopes current-by-symbol})]
    (doseq [[_ versions] code
            :let [old (first versions) cur (second versions)
                  source (get-in (service/dispatch cfg {:protocol/version "arxana.v1" :message/id (str "demo:get:" (:revision-id old))
                                                        :operation :revision/get :principal/id "codedocs/demo"
                                                        :body {:artifact/revision-id (:revision-id old)}})
                                 [:ok :revision/text])]
            [_ old-info] (:scopes old)]
      (relocation/append-one!
       cfg (relocation/relocate-one (:artifact-provider cfg) source (:revision-id old)
                                    (:revision-id cur) (get-in old-info [:scope :id])
                                    (:codepoint-range old-info))))
    {:files (into (sorted-map)
                  (for [[path versions] code]
                    [path {:historical/revision-id (:revision-id (first versions))
                           :current/revision-id (:revision-id (second versions))}]))
     :checkout/path (:futon2 roots)}))

(defn start! [port]
  (let [h (base/start! port)]
    (try (assoc h :codedocs/prepared (prepare! h))
         (catch Throwable t (base/stop! h) (throw t)))))
(def stop! base/stop!)

(defn -main [& [port-str ready-path]]
  (let [port (Long/parseLong (or port-str (str default-port))) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))
        ready (merge {:status :ready :pid (.pid (ProcessHandle/current)) :port port}
                     (:codedocs/prepared h))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "codedocs-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str ready)))
    (println (pr-str ready)) (flush) @(promise)))
