(ns futon3c.evidence.invariant
  "Runtime invariant: I-evidence-per-turn.

   Statement: each substantive agent turn must produce an evidence entry
   that is readable back from XTDB (not just accepted by a volatile in-memory
   atom). The invariant has two components:

     (a) Store-backing: the configured evidence-store is a durable backend
         (Futon1bBackend — XTDB 2 over HTTP/EDN).
     (b) Per-turn persistence: after a turn writes an entry, reading that
         entry's id back through the same backend returns the entry.

   The two checks below are callable from (a) boot in dev/bootstrap.clj
   after !evidence-store is reset, and (b) per turn in dev/invoke.clj
   right after each append. Both return data — no throwing — so callers
   can decide how loud to be.

   Grep for `I-evidence-per-turn` to enumerate enforcement sites."
  (:require [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as f1b])
  (:import [futon3c.evidence.backend AtomBackend]
           [futon3c.evidence.futon1b_backend Futon1bBackend]))

(def I-evidence-per-turn
  "Canonical definition of the invariant. String, intentionally."
  (str "I-evidence-per-turn: each substantive agent turn must append an "
       "evidence entry whose id is readable back from the XTDB-backed "
       "EvidenceBackend within the same process."))

(defn- classify-store
  "Return :xtdb, :atom-backend, :raw-atom, or :unknown.
   A raw clojure atom would be wrapped by store.clj/resolve-backend at call
   time, but is not durable on its own."
  [store]
  (cond
    (instance? Futon1bBackend store) :futon1b
    (instance? AtomBackend store) :atom-backend
    (instance? clojure.lang.IAtom store) :raw-atom
    (and store (satisfies? backend/EvidenceBackend store)) :unknown-backend
    :else :unknown))

(defn check-store-backing
  "Verify the evidence-store is XTDB-backed.
   Returns {:ok true :kind :xtdb} when it is, or
   {:ok false :kind <k> :reason <string>} otherwise.

   Intended to run once at boot after !evidence-store is reset."
  [store]
  (let [kind (classify-store store)]
    (case kind
      ;; Durable iff the store JVM is actually reachable — a stronger boot
      ;; check than the in-process kinds get (the server owns persistence).
      :futon1b
      (let [url (:base-url store)]
        (if-let [h (f1b/health url)]
          {:ok true :kind :futon1b
           :invariant I-evidence-per-turn
           :tables (:tables h)}
          {:ok false :kind :futon1b
           :invariant I-evidence-per-turn
           :reason (str "futon1b server unreachable at " url
                        " — start it (futon1b: clojure -M:node -m futon1b-server"
                        " --store-dir <dir> --port <port>) or unset"
                        " FUTON3C_EVIDENCE_BACKEND.")}))

      :atom-backend
      {:ok false :kind :atom-backend
       :invariant I-evidence-per-turn
       :reason (str "Evidence store is an AtomBackend — writes do not persist "
                    "across JVM restarts. Set FUTON3C_DIRECT_XTDB=true or "
                    "ensure role-defaults supplies :direct-xtdb? true.")}

      :raw-atom
      {:ok false :kind :raw-atom
       :invariant I-evidence-per-turn
       :reason (str "Evidence store is a raw atom — writes do not persist "
                    "across JVM restarts. Bootstrap likely took the "
                    "make-evidence-store false branch.")}

      :unknown-backend
      {:ok false :kind :unknown-backend
       :invariant I-evidence-per-turn
       :reason "Evidence store satisfies EvidenceBackend but is not the durable Futon1bBackend."}

      :unknown
      {:ok false :kind :unknown
       :invariant I-evidence-per-turn
       :reason (str "Evidence store is not a known backend type: "
                    (some-> store class .getName))})))

(defn verify-persisted
  "After an append, confirm the entry is readable back through the same backend.
   Takes the store and an evidence-id.
   Returns {:ok true :kind <k> :evidence/id id} when readable, or
   {:ok false :kind <k> :reason <string> :evidence/id id} when not.

   Intended to run synchronously after emit-invoke-evidence! writes. For the
   durable Futon1bBackend the server has already indexed the write before
   returning, so this should be a hit. A miss indicates either a non-durable
   backend or a silent append failure."
  [store evidence-id]
  (let [kind (classify-store store)
        resolved (cond
                   (satisfies? backend/EvidenceBackend store) store
                   (instance? clojure.lang.IAtom store) (backend/->AtomBackend store)
                   :else nil)
        readable (when resolved (backend/-get resolved evidence-id))]
    (cond
      (nil? resolved)
      {:ok false :kind kind
       :invariant I-evidence-per-turn
       :evidence/id evidence-id
       :reason "Store is not resolvable as an EvidenceBackend."}

      (nil? readable)
      {:ok false :kind kind
       :invariant I-evidence-per-turn
       :evidence/id evidence-id
       :reason (str "Append returned success but entry is not readable back "
                    "from the " (name kind) " backend. "
                    "Backend is not durable (server did not persist / index "
                    "the entry, or the append silently failed).")}

      :else
      {:ok true :kind kind
       :invariant I-evidence-per-turn
       :evidence/id evidence-id})))
