(ns futon3c.arxana.together-http
  "Loopback Together hub. Each document lock covers durable append, state change,
  and announcement, as well as stream replay/registration. No live defaults for
  storage: callers MUST supply :dir. Start/stop own all sockets and timers."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.together :as t]
            [org.httpkit.server :as hk])
  (:import [java.io FileOutputStream PushbackReader StringReader IOException]
           [java.net InetAddress InetSocketAddress Socket URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(defn- response [status body]
  {:status status :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string body)})

(defn- valid-name? [s max-length]
  (and (string? s) (<= 1 (count s) max-length) (boolean (re-matches #"[A-Za-z0-9._-]+" s))))

(defn- valid-identity?
  "An identity is a person's name with no dot, so that \"joe\" can never be
  mistaken for a site of another identity (see together/site-of?)."
  [s]
  (and (string? s) (boolean (re-matches #"[A-Za-z0-9_-]{1,60}" s))))

(def ^:private web-types
  {"html" "text/html; charset=utf-8" "js" "text/javascript; charset=utf-8"
   "css" "text/css; charset=utf-8"})

(defn- web-response
  "Serve one file of the browser client from the classpath, or nil. NAME is
  matched against a strict pattern, so no path can leave the web directory."
  [name]
  (when-let [[_ ext] (re-matches #"[a-z0-9-]{1,40}\.(html|js|css)" name)]
    (when-let [resource (io/resource (str "arxana/together/web/" name))]
      {:status 200
       :headers {"Content-Type" (web-types ext) "Cache-Control" "no-store"}
       :body (slurp resource :encoding "UTF-8")})))

(defn- read-entry [line]
  (with-open [r (PushbackReader. (StringReader. line))]
    (let [entry (edn/read {:eof ::eof} r)]
      (when (or (= entry ::eof) (not= ::eof (edn/read {:eof ::eof} r)))
        (throw (ex-info "Expected exactly one EDN entry per line" {})))
      entry)))

(defn- load-hub [file]
  (if-not (.exists ^java.io.File file)
    {:doc [] :log []}
    (let [s (Files/readString (.toPath ^java.io.File file) StandardCharsets/UTF_8)
          _ (when (and (seq s) (not (str/ends-with? s "\n")))
              (throw (ex-info "Incomplete log line (missing newline)" {})))
          entries (if (empty? s) [] (mapv read-entry (str/split-lines s)))]
      (reduce (fn [hub entry]
                (when-not (and (= #{:rev :op :author :parents :at} (set (keys entry)))
                               (= (:rev entry) (inc (count (:log hub))))
                               (valid-identity? (:author entry))
                               (= #{:rev} (set (keys (:parents entry)))))
                  (throw (ex-info "Invalid log entry/revision" {:entry entry})))
                (Instant/parse (:at entry))
                (let [result (t/submit hub (:op entry) (:author entry) (get-in entry [:parents :rev]) (:at entry))]
                  (when (:error result) (throw (ex-info "Log operation refused" result)))
                  (:hub result)))
              {:doc [] :log []} entries)
      {:doc (t/rebuild entries) :log entries})))

(defn- document! [{:keys [documents dir]} key]
  (locking documents
    (or (get @documents key)
        (let [file (io/file dir (str key ".log.edn"))
              state (atom (try {:hub (load-hub file) :subscribers #{} :file file}
                               (catch Exception e {:error (str "Damaged document log: " (ex-message e))
                                                   :file file :subscribers #{}})))]
          (swap! documents assoc key state)
          state))))

(defn- append-entry! [file entry]
  ;; sync before publishing; on any failure the document is poisoned until
  ;; restart/repair, since a partial line may now be on disk.
  (with-open [out (FileOutputStream. ^java.io.File file true)]
    (.write out (.getBytes (binding [*print-length* nil *print-level* nil] (str (pr-str entry) "\n")) StandardCharsets/UTF_8))
    (.sync (.getFD out))))

(defn- send-to! [document channel message]
  (try
    (when-not (hk/send! channel message false)
      (swap! document update :subscribers disj channel))
    (catch Exception _
      (swap! document update :subscribers disj channel)
      (hk/close channel))))

(defn- event [entry]
  (str "id: " (:rev entry) "\ndata: " (json/generate-string entry) "\n\n"))

(defn- stream! [request document after]
  (hk/as-channel
   request
   {:on-close (fn [ch _] (locking document (swap! document update :subscribers disj ch)))
    :on-open
    (fn [ch]
      (locking document
        (if-let [error (:error @document)]
          (hk/send! ch (response 500 {:error error}) true)
          (do
            (hk/send! ch {:status 200
                          :headers {"Content-Type" "text/event-stream; charset=utf-8"
                                    "Cache-Control" "no-cache" "X-Accel-Buffering" "no"}
                          :body ":connected\n\n"} false)
            (when (hk/open? ch)
              (swap! document update :subscribers conj ch)
              (doseq [entry (get-in @document [:hub :log]) :when (> (:rev entry) after)]
                (send-to! document ch (event entry))))))))}))

(defn- parse-after [request]
  (let [params (into {} (map #(let [[k v] (str/split % #"=" 2)]
                               [(URLDecoder/decode k "UTF-8") (URLDecoder/decode (or v "") "UTF-8")])
                            (remove empty? (str/split (or (:query-string request) "") #"&"))))
        raw (or (get-in request [:headers "last-event-id"]) (get params "after") "0")]
    (when (re-matches #"[0-9]+" raw) (parse-long raw))))

(defn- parse-body [request]
  (try
    (let [body (json/parse-string-strict (slurp (:body request) :encoding "UTF-8") true)
          op (:op body)
          id? #(and (vector? %) (= 2 (count %)) (string? (first %)) (integer? (second %)))
          shape? (and (map? body) (= #{:parents :op} (set (keys body)))
                      (map? (:parents body)) (= #{:rev} (set (keys (:parents body))))
                      (integer? (get-in body [:parents :rev])) (map? op)
                      (case (:op op)
                        "insert" (and (= #{:op :id :after :text :attrs} (set (keys op)))
                                      (id? (:id op)) (or (nil? (:after op)) (id? (:after op)))
                                      (string? (:text op)) (map? (:attrs op)))
                        "delete" (and (= #{:op :ids} (set (keys op)))
                                      (vector? (:ids op)) (every? id? (:ids op)))
                        false))]
      (when shape? (update-in body [:op :op] keyword)))
    (catch Exception _ nil)))

(defn- submit! [document body identity]
  (let [result (t/submit (:hub @document) (:op body) identity
                         (get-in body [:parents :rev]) (str (Instant/now)))]
    (if-let [error (:error result)]
      (response 409 {:error (name error)})
      (try
        (append-entry! (:file @document) (:entry result))
        (swap! document assoc :hub (:hub result))
        (doseq [ch (:subscribers @document)] (send-to! document ch (event (:entry result))))
        (response 200 {:rev (get-in result [:entry :rev])})
        (catch Exception e
          (let [error (str "Document append failed: " (ex-message e))]
            (swap! document assoc :error error)
            (doseq [ch (:subscribers @document)] (hk/close ch))
            (response 500 {:error error})))))))

(defn- handler [server]
  (fn [request]
    (let [route (re-matches #"/together/v1/doc/(.*)/(state|ops|events)" (:uri request))
          [_ key action] route
          method (:request-method request)
          health? (and (= method :get) (= (:uri request) "/together/v1/health"))
          editor (get-in request [:headers "x-together-editor"])
          web (when (= method :get)
                (when-let [[_ name] (re-matches #"/together/v1/web/(.+)" (:uri request))]
                  (web-response name)))]
      (cond
        web web
        (not (or health? (and route (= method (if (= action "ops") :post :get)))))
        (response 404 {:error "not-found"})
        (not (valid-identity? editor)) (response 401 {:error "invalid-editor"})
        health? (response 200 {:ok true :documents (count @(:documents server))})
        (not (valid-name? key 120)) (response 400 {:error "invalid-key"})
        :else
        (let [document (document! server key)]
          (locking document
            (if-let [error (:error @document)]
              (response 500 {:error error})
              (case action
                "state" (response 200 {:key key :rev (count (get-in @document [:hub :log]))
                                        :runs (t/runs (get-in @document [:hub :doc]))})
                "ops" (if-let [body (parse-body request)] (submit! document body editor)
                          (response 400 {:error "invalid-body"}))
                "events" (let [after (try (parse-after request) (catch Exception _ nil))]
                           (if (some? after) (stream! request document after)
                               (response 400 {:error "invalid-after"})))))))))))

(defn stop! "Stop this handle's streams, keepalive timer and HTTP listener." [handle]
  (.shutdownNow ^ScheduledExecutorService (:scheduler handle))
  (doseq [document (vals @(:documents handle))]
    (locking document
      (doseq [ch (:subscribers @document)] (hk/close ch))
      (swap! document assoc :subscribers #{})))
  (when-let [stopped (hk/server-stop! (:server handle) {:timeout 1000})] @stopped))

(defn start!
  "Start a loopback listener; :dir REQUIRED, :port defaults to 7096 (0 allowed).
  Returns a handle with actual :port. Never loads or binds the shared Arxana JVM."
  [{:keys [host port dir] :or {host "127.0.0.1" port 7096}}]
  (when-not (and (string? dir) (not (str/blank? dir)))
    (throw (ex-info "Together requires an explicit :dir" {:together/error :missing-dir})))
  (when-not (.isLoopbackAddress (InetAddress/getByName host))
    (throw (ex-info "Together binds loopback only" {:together/error :non-loopback})))
  (when (and (pos? port)
             (try (with-open [s (Socket.)] (.connect s (InetSocketAddress. ^String host (int port)) 200) true)
                  (catch IOException _ false)))
    (throw (ex-info "Together port already in use" {:together/error :port-in-use :port port})))
  (Files/createDirectories (.toPath (io/file dir)) (make-array java.nio.file.attribute.FileAttribute 0))
  (let [scheduler (Executors/newSingleThreadScheduledExecutor
                   (reify ThreadFactory
                     (newThread [_ r] (doto (Thread. r "together-keepalive") (.setDaemon true)))))
        state {:dir dir :documents (atom {}) :scheduler scheduler}]
    (try
      (let [server (hk/run-server (handler state) {:ip host :port port :legacy-return-value? false})]
        (.scheduleAtFixedRate
         scheduler ^Runnable
         (fn [] (doseq [document (vals @(:documents state))]
                  (locking document
                    (doseq [ch (:subscribers @document)] (send-to! document ch ":keepalive\n\n")))))
         25 25 TimeUnit/SECONDS)
        (assoc state :server server :port (hk/server-port server)))
      (catch Exception e (.shutdownNow scheduler) (throw e)))))
