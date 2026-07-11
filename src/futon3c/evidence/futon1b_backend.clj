(ns futon3c.evidence.futon1b-backend
  "EvidenceBackend over HTTP/EDN against the futon1b server (XTDB 2 store
   JVM) — the B1 slice of E-futon1b-operational-switchover.

   Why not http_backend: that one speaks JSON to a futon3c Agency API
   (keyword/namespace fidelity lost in translation). futon1b is EDN
   end-to-end, so entries round-trip byte-faithfully.

   Correctness stance: every -query/-count re-applies
   backend/filter-and-sort-entries LOCALLY with the full params — protocol
   semantics are the shared implementation's. Pushdown scope depends on the
   query: when every membership-deciding filter is one the server applies
   IDENTICALLY (type/claim-type/author/session-id/since/before/fork-of +
   the explicit ephemeral flag), `limit` passes through too and the server
   windows (the unlimited author=joe fetch hydrated 8,882 docs/10MB for a
   5-item recall and, cold, silently timed out zai-1's first live demo,
   2026-07-11). When client-only filters are present (tags/subject/
   pattern-id), the fetch stays ephemeral-inclusive and unlimited so the
   local filter owns membership.

   -append preserves AtomBackend/XtdbBackend semantics: duplicate-id /
   reply-not-found / fork-not-found come back as SocialError maps (the
   server's 409 maps to :duplicate-id; reply/fork existence is checked
   client-side — the append-only server doesn't enforce those).
   -delete! is a logged no-op: futon1b evidence is append-only and no
   live path compacts (verified 2026-07-10).

   Reads throw on transport errors (R4 loud failure); -append returns a
   SocialError so the invoke path can surface it as data."
  (:require [futon3c.evidence.backend :as backend]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def default-url
  ;; 127.0.0.1, NOT localhost: the futon1b JVM runs preferIPv4Stack (XTDB
  ;; pgwire needs it), so its HttpServer binds IPv4 only — a client JVM
  ;; resolving localhost to ::1 gets "unreachable" (bit us at Gate 1).
  "http://127.0.0.1:7074")

(defn- penholder []
  (or (System/getenv "FUTON1B_PENHOLDER")
      (System/getenv "FUTON1A_PENHOLDER")
      "api"))

(defn- api-url [base-url path]
  (str (str/replace base-url #"/$" "") path))

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))

(defn- read-edn
  "Tolerant EDN read: unknown tagged literals pass through as their value
   (backfilled docs can carry tags this client doesn't register readers for)."
  [s]
  (let [s (if (string? s) s (some-> s slurp))]
    (when (seq (str s))
      (edn/read-string {:default (fn [_tag v] v)} s))))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (str (Instant/now))}
    (seq context) (assoc :error/context context)))

(defn- get-edn
  "GET url, EDN-parse the body. Returns {:status n :body v}.
   Throws on transport-level failure (connection refused etc.)."
  [url]
  (let [{:keys [status body error]} @(http/get url {:timeout 30000 :as :text})]
    (when error
      (throw (ex-info "futon1b unreachable" {:url url} error)))
    {:status status :body (read-edn body)}))

(defn- client-only-filters?
  "Filters the server does not apply identically — membership must then be
   decided locally over an ephemeral-inclusive, unlimited fetch."
  [{:query/keys [tags subject pattern-id]}]
  (boolean (or (seq tags) subject pattern-id)))

(defn- query-string
  "Pushdown params. See ns docstring for the two regimes."
  [{:query/keys [type claim-type author session-id since before fork-of
                 limit include-ephemeral?] :as params}]
  (let [server-decidable? (not (client-only-filters? params))
        pairs (cond-> [["include-ephemeral"
                        (str (boolean (or include-ephemeral?
                                          (not server-decidable?))))]]
                (and server-decidable? (int? limit) (pos? limit))
                (conj ["limit" (str limit)])
                type (conj ["type" (name type)])
                claim-type (conj ["claim-type" (name claim-type)])
                author (conj ["author" (str author)])
                session-id (conj ["session-id" (str session-id)])
                since (conj ["since" (str since)])
                before (conj ["before" (str before)])
                fork-of (conj ["fork-of" (str fork-of)]))]
    (str/join "&" (map (fn [[k v]] (str k "=" (enc v))) pairs))))

(defn- fetch-entries [base-url params]
  (let [url (str (api-url base-url "/api/alpha/evidence") "?" (query-string params))
        {:keys [status body]} (get-edn url)]
    (if (= 200 status)
      (:entries body)
      (throw (ex-info "futon1b evidence query failed" {:status status :body body})))))

(defrecord Futon1bBackend [base-url]
  backend/EvidenceBackend

  (-append [this validated]
    (let [eid (:evidence/id validated)
          reply (:evidence/in-reply-to validated)
          fork (:evidence/fork-of validated)]
      (cond
        (and reply (not (backend/-exists? this reply)))
        (social-error :reply-not-found "in-reply-to references missing entry"
                      :in-reply-to reply :evidence-id eid)

        (and fork (not (backend/-exists? this fork)))
        (social-error :fork-not-found "fork-of references missing entry"
                      :fork-of fork :evidence-id eid)

        :else
        (let [{:keys [status body error]}
              @(http/post (api-url base-url "/api/alpha/evidence")
                          {:timeout 30000
                           :as :text
                           :headers {"content-type" "application/edn"
                                     "x-penholder" (penholder)}
                           :body (pr-str validated)})
              parsed (read-edn body)]
          (cond
            error
            (social-error :store-unreachable "futon1b server unreachable"
                          :evidence-id eid :detail (str error))

            (= 201 status)
            {:ok true :entry (or (:entry parsed) validated)}

            (= 409 status)
            (social-error :duplicate-id "Evidence id already exists"
                          :evidence-id eid)

            :else
            (social-error :store-rejected "futon1b rejected the append"
                          :evidence-id eid :status status :body parsed))))))

  (-get [_ evidence-id]
    (let [{:keys [status body]}
          (get-edn (str (api-url base-url "/api/alpha/evidence/") (enc evidence-id)))]
      (when (= 200 status) body)))

  (-exists? [this evidence-id]
    (some? (backend/-get this evidence-id)))

  (-query [_ params]
    (backend/filter-and-sort-entries (fetch-entries base-url params) params))

  (-count [_ params]
    (let [params (dissoc params :query/limit)]
      (if (client-only-filters? params)
        (count (backend/filter-and-sort-entries (fetch-entries base-url params) params))
        ;; fully server-decidable -> the server'\''s projected /count path
        (let [url (str (api-url base-url "/api/alpha/evidence/count")
                       "?" (query-string params))
              {:keys [status body]} (get-edn url)]
          (if (= 200 status)
            (long (or (:count body) 0))
            (throw (ex-info "futon1b count failed" {:status status :body body})))))))

  (-forks-of [_ evidence-id]
    ;; include-ephemeral? true: -forks-of does not filter ephemeral (protocol)
    (->> (fetch-entries base-url {:query/fork-of evidence-id
                                  :query/include-ephemeral? true})
         (sort-by backend/entry-at)
         vec))

  (-delete! [_ ids]
    (println (str "[futon1b-backend] -delete! is a no-op (append-only store); "
                  "requested " (count ids) " ids"))
    {:compacted 0})

  (-all [_]
    (fetch-entries base-url {:query/include-ephemeral? true})))

(defn health
  "GET /health on the futon1b server. Returns the parsed body or nil when
   unreachable — used by the I-evidence-per-turn boot check."
  [base-url]
  (try
    (let [{:keys [status body]} (get-edn (api-url base-url "/health"))]
      (when (= 200 status) body))
    (catch Exception _ nil)))

(defn make-futon1b-backend
  "Construct the backend. base-url default: FUTON1B_URL env, then
   http://localhost:7074 (lucy's port — nginx owns :7073 there)."
  ([] (make-futon1b-backend (or (System/getenv "FUTON1B_URL") default-url)))
  ([base-url] (->Futon1bBackend base-url)))
