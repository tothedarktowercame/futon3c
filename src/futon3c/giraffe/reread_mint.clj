(ns futon3c.giraffe.reread-mint
  "Single-flight Giraffe mint coordinator for landed corpus generations."
  (:require [clojure.java.shell :as shell]))

(defn coordinator
  "Create an in-process coordinator. MINT-FN performs the supported Giraffe
  mint and returns a typed result. Requests received during a mint coalesce
  into exactly one follow-up pass."
  [mint-fn]
  {:state (atom {:requested 0 :completed 0 :running? false
                 :last-result nil :last-run-id nil :last-at-ms nil})
   :mint-fn mint-fn})

(defn- run-loop! [{:keys [state mint-fn] :as coordinator}]
  (future
    (loop []
      (let [through (:requested @state)
            result (try (mint-fn)
                        (catch Exception exc
                          {:status :failed :reason :giraffe-mint-exception
                           :message (.getMessage exc)}))]
        (swap! state assoc :completed through :last-result result
               :last-run-id (:run-id result)
               :last-at-ms (System/currentTimeMillis))
        (let [again? (atom false)]
          (swap! state (fn [s]
                         (if (> (:requested s) through)
                           (do (reset! again? true) s)
                           (assoc s :running? false))))
        (if @again?
          (recur)
          nil)))))
  coordinator)

(defn request!
  "Request a recomputation. Starts one mint when idle, otherwise coalesces."
  [{:keys [state] :as coordinator}]
  (let [start? (atom false)
        snapshot (swap! state
                        (fn [s]
                          (let [idle? (not (:running? s))]
                            (reset! start? idle?)
                            (-> s (update :requested inc)
                                (cond-> idle? (assoc :running? true))))))]
    (when @start? (run-loop! coordinator))
    {:status (if @start? :started :coalesced)
     :request (:requested snapshot)}))

(defn await-idle!
  "Test/CLI join point. Returns coordinator state or throws on timeout."
  [{:keys [state]} timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [s @state]
        (cond
          (not (:running? s)) s
          (>= (System/currentTimeMillis) deadline)
          (throw (ex-info "Giraffe mint did not become idle"
                          {:kind :giraffe-reread/mint-timeout :state s}))
          :else (do (Thread/sleep 10) (recur)))))))

(defn supported-mint!
  "Run the same supported mint invocation as wm_click.sh source-giraffe."
  [{:keys [checkout edges readings-root giraffe-root]}]
  (let [checkout (or checkout (System/getenv "FUTON3C_CHECKOUT")
                     "/home/joe/code/futon3c")
        edges (or edges (System/getenv "GIRAFFE_EDGES")
                  "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json")
        readings-root (or readings-root (System/getenv "GIRAFFE_READINGS_ROOT")
                          "/home/joe/notes/wm-next/giraffe-reads-2026-11-04/corpus")
        giraffe-root (or giraffe-root (System/getenv "GIRAFFE_ROOT")
                         "/home/joe/code/storage/giraffe")
        result (shell/sh "clojure" "-M" "-m" "futon3c.giraffe.run"
                         "--edges" edges "--source" "missions-excursions"
                         "--readings-root" readings-root "--root" giraffe-root
                         "--with-retained-focus" :dir checkout)
        run-id (second (re-find #"(?m)^run id: (.+)$" (:out result)))]
    (if (and (zero? (:exit result)) run-id)
      {:status :minted :run-id run-id}
      {:status :failed :reason :giraffe-mint-failed
       :exit (:exit result) :out (:out result) :err (:err result)})))

(defonce ^:private shared-default
  (delay (coordinator #(supported-mint! {}))))

(defn shared-coordinator
  "Process-wide coordinator for the canonical mint path. Agency/JVM callers
  share this single flight; the systemd unit also prevents overlapping runs."
  []
  @shared-default)
