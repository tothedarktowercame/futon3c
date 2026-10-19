(ns futon3c.xiang.ma-reading
  "馬 alignment reading: each AGENT reply read against the operator turn
   it answers.

   象 (the analysis seats) reads every operator turn into acts; nothing
   read the reply side.  This namespace is that reader.  For every
   operator turn whose record has a reply and computed ports it runs two
   passes:

   1. Classical, no model: an operator act is `answered` when a reply act
      of the same turn carries :target = that act's id (the reply's
      bracket quotes the operator's words verbatim; futon3c.xiang.turn-acts
      reply-acts).  Method \"quote\".
   2. Model, only for acts the classical pass left open: one bell to the
      configured seat (default 馬-sonnet) carrying the operator acts
      (id, kind, text) and the reply's marked paragraphs (id, mark, text),
      asking for strict JSON.  A model verdict is accepted only when its
      quote occurs verbatim in the reply text — the reader does not
      certify itself; otherwise the act is `unread`, reason
      \"quote not found\".  Method \"model\".

   The result is stored on the record as :alignment (version 1) and a
   {:event :aligned :id id} turn event raised so the stepper can refresh.
   When the seat is not configured (no :ask effect) or does not answer
   within the bound, the classical pass alone is stored with :seat nil and
   :unread the open count.  Nothing here blocks the turn pipeline: the
   listener runs off the caller's path (turn-service schedules listeners
   at delay 0) and the ask is bounded by :timeout-ms.

   The trigger is the :ported event turn-service raises after
   compute-ports! stores its answer.  Operator turns only: bell turns
   (origin \"agent\") are declared and need no reading."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [futon3c.xiang.turn-record :as tr]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.util.concurrent Executors ExecutorService ThreadFactory]))

(def defaults
  "The 馬 alignment reading's knobs. :seat names the seat to bell;
   :ask is the effect that bells it and waits, bounded:
   (fn [{:keys [seat prompt timeout-ms]}] -> reply text or nil).
   nil means no seat wired (claude-3 registers 馬-sonnet after review):
   the classical pass alone is stored."
  {:seat "馬-sonnet"
   :ask nil
   :timeout-ms 60000})

(def ^:private verdicts #{"answered" "deferred" "declined" "not-answered"})

;; ------------------------------------------------------------------ passes

(defn- operator-acts
  "The record's operator acts: author `operator`, id under this turn's
  fragment namespace (\"<turn-id>-f-…\")."
  [record]
  (let [base (str (:turn_id record) "-f-")]
    (->> (:acts record)
         (filter #(and (= "operator" (:author %))
                       (string? (:id %))
                       (str/starts-with? (:id %) base))))))

(defn- reply-acts
  "The record's reply acts: id under \"<turn-id>-r-<n>\"."
  [record]
  (let [base (str (:turn_id record) "-r-")]
    (->> (:acts record)
         (filter #(and (string? (:id %))
                       (str/starts-with? (:id %) base))))))

(defn- paragraphs
  "The reply's marked paragraphs with their act ids: [{:id :mark :text}],
  id = \"<turn-id>-r-<n>\" as reply-acts numbers them (the index into
  reply-marks, the same enumeration both use)."
  [record]
  (let [turn-id (:turn_id record)]
    (->> (tr/reply-marks (str (or (:reply_text record) "")))
         (map-indexed (fn [idx {:keys [mark text]}]
                        {:id (str turn-id "-r-" idx)
                         :mark (some-> mark name)
                         :text (str text)}))
         vec)))

(defn classical-pass
  "Pass 1, no model: every operator act of RECORD, `answered` with method
  `quote` when a reply act of the same turn targets it, else open (nil
  verdict).  Returns {:entries […] :open […]} with entries carrying
  {:act :kind :text :verdict :by :mark :quote :method}."
  [record]
  (let [replies (reply-acts record)
        targeters (into {} (keep (fn [a] (when-let [t (:target a)] [t a])) replies))
        marks (into {} (map (juxt :id :mark) (paragraphs record)))]
    (letfn [(entry [a]
              {:act (:id a)
               :kind (some-> (:kind a) name)
               :text (str (or (:text a) ""))
               :verdict nil :by nil :mark nil :quote nil :method nil})]
      (let [entries (mapv (fn [a]
                            (if-let [by (get targeters (:id a))]
                              (assoc (entry a)
                                     :verdict "answered"
                                     :by (:id by)
                                     :mark (get marks (:id by))
                                     :method "quote")
                              (entry a)))
                          (operator-acts record))]
        {:entries entries
         :open (filterv #(nil? (:verdict %)) entries)}))))

(defn- extract-json-array
  "The model's reply, cut to its outermost [ … ] (models wrap JSON in
  prose or fences); nil when none parses."
  [text]
  (try
    (let [^String s (str text)
          lo (.indexOf s "[")
          hi (.lastIndexOf s "]")]
      (when (and (not (neg? lo)) (> hi lo))
        (json/parse-string (subs s lo (inc hi)) true)))
    (catch Exception _ nil)))

(defn- accept-verdict
  "One model object against the reply text and paragraph ids: the entry to
  store, or nil when the object is not shaped like a verdict."
  [para-ids {:keys [act verdict by quote]}]
  (when (and (string? act) (string? verdict))
    (cond->
      {:act act :verdict nil :by nil :quote nil :method "model"}
      (contains? verdicts verdict) (assoc :verdict verdict)
      (and (string? by) (contains? para-ids by)) (assoc :by by)
      (string? quote) (assoc :quote quote))))

(def ^:private min-quote-words
  "A quote of fewer words than this matches almost any reply (\"the\", \"yes\")."
  3)

(defn quote-holds?
  "Does QUOTE support VERDICT against REPLY-TEXT?  \"not-answered\" needs no
  quote.  Every other verdict needs a quote of at least `min-quote-words`
  words that occurs verbatim in the reply: an empty quote is contained in
  every text, so it would let the reader certify itself."
  [verdict quote reply-text]
  (or (= "not-answered" verdict)
      (let [q (str/trim (str (or quote "")))]
        (and (>= (count (re-seq #"\S+" q)) min-quote-words)
             (str/includes? (str reply-text) q)))))

(defn model-pass
  "Pass 2: one bounded bell to the seat for OPEN acts.  Returns
  {:seat … :entries […] :unanswered n} where entries replace the open
  ones; a model verdict is kept only if its quote is verbatim in
  REPLY-TEXT, else the act is `unread` (reason `quote not found`).
  :seat is nil when no seat is wired or it did not answer in time."
  [{:keys [seat ask timeout-ms] :or {seat (:seat defaults)
                                     timeout-ms (:timeout-ms defaults)}}
   open reply-text paragraphs]
  (let [ask (or ask (:ask defaults))
        para-ids (set (map :id paragraphs))
        unread (fn [entry reason]
                 (assoc entry
                        :verdict "unread" :reason reason :method nil))
        open-ids (set (map :act open))]
    (if-not (and ask (seq open))
      {:seat nil :entries (mapv #(unread % (if ask "no model verdict" "seat not wired"))
                                open)
       :unanswered (count open)}
      (let [prompt (str "馬 alignment reading: for each operator act below, decide whether "
                        "the agent's reply answers it.\n\n"
                        "Operator acts (id, kind, text):\n"
                        (json/generate-string
                         (mapv #(select-keys % [:act :kind :text]) open) true)
                        "\n\nReply paragraphs (id, mark, text):\n"
                        (json/generate-string
                         (mapv #(select-keys % [:id :mark :text]) paragraphs) true)
                        "\n\nAnswer with strict JSON only, an array with one object per "
                        "operator act:\n"
                        "[{\"act\": \"<act-id>\", \"verdict\": \"answered\"|\"deferred\"|"
                        "\"declined\"|\"not-answered\", \"by\": \"<reply-act-id or null>\", "
                        "\"quote\": \"<exact words from the reply>\"}]\n"
                        "Use \"by\" only for the paragraph that answers it; \"quote\" must "
                        "be verbatim words from the reply text.")
            reply (try (ask {:seat seat :prompt prompt :timeout-ms timeout-ms})
                       (catch Exception _ nil))
            verdicts* (when (string? reply) (extract-json-array reply))]
        (if-not (sequential? verdicts*)
          {:seat nil :entries (mapv #(unread % "no model verdict") open)
           :unanswered (count open)}
          (let [by-act (into {} (comp (map #(accept-verdict para-ids %))
                                       (keep (fn [v]
                                               (when (and (:verdict v)
                                                          (contains? open-ids (:act v)))
                                                 [(:act v) v]))))
                             verdicts*)
                entries (mapv (fn [entry]
                                (let [v (get by-act (:act entry))]
                                  (cond
                                    (nil? v) (unread entry "no model verdict")
                                    (and (= "answered" (:verdict v)) (nil? (:by v)))
                                    (unread entry "answered without a paragraph")
                                    (not (quote-holds? (:verdict v) (:quote v) reply-text))
                                    (unread entry "quote not found")
                                    :else (merge entry v))))
                              open)]
            {:seat (some-> seat str)
             :entries entries
             :unanswered (count (filter #(= "unread" (:verdict %)) entries))}))))))

;; ----------------------------------------------------------------- driver

(defn alignment-map
  "The version-1 alignment map for RECORD, running both passes.  Pure
  apart from the (bounded, injected) ask."
  [opts record]
  (let [{:keys [entries open]} (classical-pass record)
        reply-text (str (or (:reply_text record) ""))
        paragraphs (paragraphs record)
        {:keys [seat] model-entries :entries}
        (model-pass opts open reply-text paragraphs)
        marks (into {} (map (juxt :id :mark) paragraphs))
        model-entries (mapv #(cond-> % (:by %) (assoc :mark (get marks (:by %)))) model-entries)
        by-act (into {} (map (juxt :act identity) model-entries))
        acts (mapv (fn [e] (if-let [m (get by-act (:act e))]
                             (let [m (dissoc m :kind :text)]
                               (cond-> (merge e m)
                                 (contains? m :reason)
                                 (assoc :reason (:reason m))))
                             e))
                   entries)
        unread-n (count (filter #(= "unread" (:verdict %)) acts))]
    {:version 1
     :at (:at opts)
     :seat seat
     :acts acts
     :unread unread-n}))

(defn align!
  "Read one operator turn's reply against its acts and store :alignment.
  Nil (nothing stored) when the record is absent, not an operator turn,
  has no reply, or its ports are not computed.  Raises the injected
  :notify! with {:event :aligned :id id} after storing.  Never throws."
  [{:keys [store notify!] :as opts} id]
  (try
    (let [record (and store (ts/read-record store id))]
      (when (and record
                 (= "operator" (:origin record))
                 (seq (str (or (:reply_text record) "")))
                 (= "computed" (get-in record [:ports_status :state])))
        (let [alignment (-> (alignment-map (assoc opts :at (str (java.time.Instant/now)))
                                           record))]
          (ts/update-record! store id #(assoc % :alignment alignment))
          (when notify! (notify! {:event :aligned :id id}))
          alignment)))
    (catch Exception _ nil)))

(defonce ^:private ^ExecutorService executor
  ;; One daemon thread: the model pass can take up to :timeout-ms, and the
  ;; turn service's scheduler is shared with every other turn listener.
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "ma-reading") (.setDaemon true))))))

(defn start!
  "Register the 馬 listener on turn-service events.  OPTS as per defaults
  plus :store (turn-store, or a 0-arg fn returning it, resolved at event
  time) and :notify! (a 1-arg fn, usually the running service's notify).
  The reading runs on this namespace's own thread.  Re-registering replaces
  the listener."
  [{:keys [store notify!] :as opts}]
  (turn-service/add-turn-listener!
   ::ma-reading
   (fn [{:keys [event id]}]
     (when (and (= :ported event) id)
       (.submit executor
                ^Runnable
                (fn []
                  (let [store (if (fn? store) (store) store)]
                    (when store
                      (align! (assoc opts :store store :notify! notify!) id))))))))
  nil)

;; ------------------------------------------------------------ live wiring

(defn whistle-ask
  "The production :ask: a bounded whistle to SEAT through Agency's HTTP API
  (synchronous, so the request and its answer cannot cross).  Returns the
  seat's text, or nil on any failure or timeout."
  [{:keys [seat prompt timeout-ms]}]
  (try
    (let [conn (doto ^java.net.HttpURLConnection
                (.openConnection (java.net.URL. "http://127.0.0.1:7070/api/alpha/whistle"))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout (int (+ (or timeout-ms 60000) 15000)))
                 (.setRequestProperty "Content-Type" "application/json"))]
      (with-open [w (io/writer (.getOutputStream conn))]
        (.write w (json/generate-string {:agent-id seat :prompt prompt
                                         :timeout-ms (or timeout-ms 60000)
                                         :caller "ma-reading"})))
      (let [body (json/parse-string (slurp (.getInputStream conn)) true)]
        (when (:ok body) (some-> (:response body) str))))
    (catch Throwable _ nil)))

(defn- live-service []
  (try (some-> (requiring-resolve 'futon3c.transport.http/xiang-bell-service) deref (apply []))
       (catch Throwable _ nil)))

(defn start-live!
  "Wire the listener to the running service: its store and notify are found
  at event time, so this survives reloads of either namespace."
  []
  (start! {:ask whistle-ask
           :store (fn [] (some-> (live-service) :config :store))
           :notify! (fn [ev] (when-let [svc (live-service)]
                               (@#'turn-service/notify-turn! svc ev)))}))
