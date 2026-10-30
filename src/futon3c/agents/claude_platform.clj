(ns futon3c.agents.claude-platform
  "Stateless Claude Platform invoke functions for bounded Agency readers.

   This is deliberately not a Claude Code compatibility layer.  馬 and 驴
   receive one prompt and return one text result; no conversation is carried
   between calls.  Every successful charge is appended to a JSONL ledger."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.file Files LinkOption Paths]
           [java.nio.file.attribute PosixFilePermission]
           [java.time Instant]
           [java.util UUID]))

(def default-url "https://api.anthropic.com/v1/messages")
(def default-key-file "~/.joe-claude-api-key")
(def default-ledger "~/.local/state/futon3c/claude-platform.jsonl")

(def prices
  {"claude-haiku-5-5" {:input 0.10 :output 0.50}
   "claude-sonnet-5-5" {:input 2.00 :output 10.00}})

(def reader-profiles
  {"馬-sonnet" {:model "claude-sonnet-5-5" :max-tokens 2048 :max-usd 0.08}
   "驴-haiku" {:model "claude-haiku-5-5" :max-tokens 4096 :max-usd 0.02}})

(defn- expand-home [path]
  (str/replace-first (str path) #"^~" (System/getProperty "user.home")))

(defn read-api-key
  "Read a nonempty API key, refusing a group/world-accessible POSIX file."
  [path]
  (let [p (Paths/get (expand-home path) (make-array String 0))
        perms (try (Files/getPosixFilePermissions p (make-array LinkOption 0))
                   (catch UnsupportedOperationException _ nil))
        unsafe #{PosixFilePermission/GROUP_READ PosixFilePermission/GROUP_WRITE
                 PosixFilePermission/GROUP_EXECUTE PosixFilePermission/OTHERS_READ
                 PosixFilePermission/OTHERS_WRITE PosixFilePermission/OTHERS_EXECUTE}]
    (when (and perms (some #(contains? unsafe %) perms))
      (throw (ex-info "Claude Platform key file is group/world accessible"
                      {:path (str p)})))
    (let [key (str/trim (slurp (str p)))]
      (when (str/blank? key)
        (throw (ex-info "Claude Platform key file is empty" {:path (str p)})))
      key)))

(defn usage-cost
  [model usage]
  (let [{:keys [input output]} (or (get prices model)
                                   (throw (ex-info "No Claude Platform price table"
                                                   {:model model})))]
    (/ (+ (* (long (or (:input_tokens usage) 0)) input)
          (* (long (or (:output_tokens usage) 0)) output))
       1000000.0)))

(defn conservative-cost
  "UTF-8 bytes upper-bound input tokens; MAX-TOKENS bounds output tokens."
  [model prompt max-tokens]
  (let [{:keys [input output]} (or (get prices model)
                                   (throw (ex-info "No Claude Platform price table"
                                                   {:model model})))]
    (/ (+ (* (alength (.getBytes (str prompt) "UTF-8")) input)
          (* max-tokens output))
       1000000.0)))

(defn call!
  [{:keys [api-key model prompt max-tokens url]
    :or {url default-url}}]
  (let [body (json/generate-string
              {:model model :max_tokens max-tokens
               :messages [{:role "user" :content (str prompt)}]})
        req (-> (HttpRequest/newBuilder (URI/create url))
                (.header "content-type" "application/json")
                (.header "x-api-key" api-key)
                (.header "anthropic-version" "2023-06-01")
                (.POST (HttpRequest$BodyPublishers/ofString body))
                (.build))
        response (.send (HttpClient/newHttpClient) req (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        parsed (json/parse-string (.body response) true)]
    (when-not (<= 200 status 299)
      (throw (ex-info "Claude Platform request failed" {:status status :body parsed})))
    {:text (->> (:content parsed)
                (filter #(= "text" (:type %)))
                (map :text)
                (apply str))
     :usage (:usage parsed)
     :model (:model parsed)
     :request-id (some-> response .headers (.firstValue "request-id") (.orElse nil))}))

(defn- append-receipt! [path receipt]
  (let [f (io/file (expand-home path))]
    (.mkdirs (.getParentFile f))
    (locking append-receipt!
      (spit f (str (json/generate-string receipt) "\n") :append true))))

(defn- current-month [] (subs (str (Instant/now)) 0 7))

(defn ledger-month-cost
  "Sum valid receipts in PATH for the current UTC month."
  [path]
  (let [f (io/file (expand-home path))
        month (current-month)]
    (if-not (.isFile f)
      0.0
      (with-open [reader (io/reader f)]
        (reduce (fn [total line]
                  (try
                    (let [row (json/parse-string line true)]
                      (if (str/starts-with? (str (:at row)) month)
                        (+ total (double (or (:cost-usd row) 0.0)))
                        total))
                    (catch Throwable _ total)))
                0.0 (line-seq reader))))))

(defn make-invoke-fn
  "Create a stateless Agency invoke function.

   `max-usd` is a hard per-call preflight bound. `call-fn` is injectable so
   the exact overspend and malformed-response cases can be tested without a
   network stub."
  [{:keys [agent-id model key-file ledger max-tokens max-usd monthly-limit-usd call-fn]
    :or {agent-id "claude-platform" model "claude-sonnet-5-5"
         key-file default-key-file ledger default-ledger max-tokens 2048
         max-usd 0.25 monthly-limit-usd 200.0 call-fn call!}}]
  (let [month-spend (atom (ledger-month-cost ledger))]
    (fn [prompt _session-id & _]
      (let [reserved (atom 0.0)]
        (try
          (let [bound (conservative-cost model prompt max-tokens)]
        (when (> bound max-usd)
          (throw (ex-info "Claude Platform request exceeds per-call budget"
                          {:bound-usd bound :max-usd max-usd})))
        (let [new-total (swap! month-spend + bound)]
          (reset! reserved bound)
          (when (> new-total monthly-limit-usd)
            (swap! month-spend - bound)
            (reset! reserved 0.0)
            (throw (ex-info "Claude Platform monthly budget exhausted"
                            {:reserved-total-usd new-total
                             :monthly-limit-usd monthly-limit-usd}))))
        (let [started (System/nanoTime)
              response (call-fn {:api-key (read-api-key key-file) :model model
                                 :prompt prompt :max-tokens max-tokens})
              text (str (:text response))
              cost (usage-cost model (:usage response))
              sid (str "platform-" (UUID/randomUUID))
              receipt {:schema "claude-platform-invoke.v1" :at (str (Instant/now))
                       :agent-id agent-id :model model :request-id (:request-id response)
                       :usage (:usage response) :cost-usd cost
                       :elapsed-ms (long (/ (- (System/nanoTime) started) 1000000))}]
          (when (str/blank? text)
            (throw (ex-info "Claude Platform response contained no text" {})))
          (swap! month-spend + (- cost bound))
          (reset! reserved 0.0)
          (append-receipt! ledger receipt)
          {:result text :session-id sid :usage (:usage response)
           :cost-usd cost :request-id (:request-id response)}))
          (catch Throwable t
            (when (pos? @reserved)
              (swap! month-spend - @reserved))
            {:result nil :session-id nil :error (.getMessage t)
             :error-data (ex-data t)}))))))

(defn make-reader-invoke-fn
  "Construct one of the two approved bounded readers. Caller options may
   lower budgets or select a ledger, but may not silently change the model."
  [{:keys [agent-id model] :as opts}]
  (let [profile (or (get reader-profiles agent-id)
                    (throw (ex-info "No Claude Platform reader profile"
                                    {:agent-id agent-id})))]
    (when (and model (not= model (:model profile)))
      (throw (ex-info "Claude Platform reader model conflicts with its profile"
                      {:agent-id agent-id :model model
                       :required-model (:model profile)})))
    (make-invoke-fn (merge profile opts))))
