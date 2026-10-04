(ns futon3c.wm.ordinary-click-budget
  "Issue-time accounting for Joe's five ordinary clicks. Specialized RUN4/R10
   requests retain their own authority; only the plain HTTP branch calls this."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]))

(def authorization
  ;; Eighth grant, Joe 2026-09-24, heard directly by claude-5 in the operator
  ;; buffer: "I award 5 more clicks".
  ;; The seventh (AUTH-ordinary-click-budget-renewal-6-2026-09-23.md @
  ;; 2996eb6b) was fully consumed, 7 of 7, so nothing carries forward and this
  ;; allocates five. The seven earlier grants allocated five each bar that one;
  ;; consumption counts ledger entries whose :authorization equals THIS map, so
  ;; the thirty-five already spent are neither double-counted nor erased --
  ;; each remains in the ledger citing the authority in force when it was
  ;; spent.
  ;;
  ;; The grant does not restart the machine. The grounding misreport that ran
  ;; from 2026-09-23 17:44 (entity :props read back as a string, so :resolved?
  ;; was false about a commit the entity does name) is repaired and loaded.
  ;; What holds the clicks now is Joe's stop-line rule, settled 2026-09-24:
  ;; "if there is a stop-line, in my vocabulary that means the system should be
  ;; repaired from outside." An open stop-line stops the line -- it is not
  ;; per-defect -- so no click issues while the board is dirty. 35 open
  ;; :machine-failure findings stand. The authorization document carries the
  ;; board and the repair route.
  ;;
  ;; 2026-09-24, later: Joe to claude-8, heard directly in claude-8's
  ;; operator buffer: "I would award 10 clicks for you to use overnight as
  ;; you see fit." Recorded in the same document's section "Joe's grant to
  ;; claude-8, 2026-09-24" (futon2 326826ac) per that document's own rule
  ;; that a draw raises `allocated` here rather than minting a new file.
  ;; 5 (claude-5's lane, banked) + 10 (claude-8's lane) = 15. The ledger's
  ;; :caller says which lane spent which. No click-path gate reads the
  ;; board; the stop-line discipline is the lane's, taken from the store.
  {:path "futon2/holes/labs/wm-contract/AUTH-ordinary-click-budget-renewal-7-2026-09-24.md"
   :sha "326826ac"})
(def allocated
  "Five of the eighth grant (unspent, claude-5) plus ten granted to claude-8
   on 2026-09-24 for overnight use at claude-8's discretion, plus twenty
   granted to claude-1 on 2026-09-29 (\"OK let's run 20 ticks, you can do
   repairs between them\"), plus ten granted to codex-10 on 2026-10-01 for
   the new registered-run series (\"I'll grant a block of 10 clicks\"; repairs
   may land between clicks), plus one granted to codex-10 on 2026-10-04
   after the repaired read-only META preview ("1 is authorised"). All grants
   are recorded in the authorization document named above."
  46)
(def ^:dynamic *ledger-path*
  "/home/joe/code/futon2/data/wm-ordinary-clicks/consumption.jsonl")
(defonce ^:private issue-lock (Object.))

(defn- charged-click-ids [entries]
  (let [consumed (into #{} (keep #(when (and (= authorization (:authorization %))
                                             (nil? (:event %)))
                                    (:click-id %))) entries)
        refunded (into #{} (keep #(when (and (= authorization (:authorization %))
                                             (= "refund" (:event %)))
                                    (:click-id %))) entries)]
    (set/difference consumed refunded)))

(defn- parse-ledger [text]
  (mapv #(json/parse-string % true)
        (remove str/blank? (str/split-lines text))))

(defn- ledger-entries [file]
  (if (.exists file) (parse-ledger (slurp file)) []))

(defn availability
  "Return a non-consuming, source-pinned snapshot of the ordinary-click ration.
   The same lock as `consume!` makes the ledger bytes and count one observation."
  []
  (locking issue-lock
    (let [file (io/file *ledger-path*)
          bytes (if (.exists file)
                  (with-open [channel (FileChannel/open
                                       (.toPath file)
                                       (into-array StandardOpenOption
                                                   [StandardOpenOption/READ]))
                              _file-lock (.lock channel 0 Long/MAX_VALUE true)]
                    (java.nio.file.Files/readAllBytes (.toPath file)))
                  (byte-array 0))
          entries (parse-ledger (String. bytes "UTF-8"))
          consumed (count (charged-click-ids entries))]
      {:schema :wm/ordinary-click-availability-v1
       :authorization authorization
       :allocated allocated
       :consumed consumed
       :available (max 0 (- allocated consumed))
       :unit :ordinary-click
       :ledger-source
       {:path *ledger-path*
        :sha256 (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256") bytes)]
                   (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))}})))

(defn consume!
  "Append and force consumption before the worker starts. Serialize the count
   and append across threads and processes; failed runs never refund a grant."
  [click-id issued-at caller]
  (locking issue-lock
    (let [file (io/file *ledger-path*)]
      (io/make-parents file)
      (with-open [channel (FileChannel/open
                          (.toPath file)
                          (into-array StandardOpenOption
                                      [StandardOpenOption/CREATE StandardOpenOption/READ
                                       StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (let [entries (ledger-entries file)
              consumed (count (charged-click-ids entries))]
          (when (>= consumed allocated)
            (throw (ex-info
                    (str "Ordinary click budget exhausted. Return to Joe for renewal. Authority: "
                         (:path authorization) " @ " (:sha authorization))
                    {:status 409 :error :ordinary-click-budget-exhausted
                     :authorization authorization :allocated allocated
                     :consumed consumed :renewal "Joe"})))
          (let [entry {:click-id click-id :issued-at issued-at
                       :authorization authorization
                       :caller (or caller :caller-unknown)}
                buffer (ByteBuffer/wrap
                        (.getBytes (str (json/generate-string entry) "\n") "UTF-8"))]
            (.position channel (.size channel))
            (while (.hasRemaining buffer) (.write channel buffer))
            (.force channel true)
            ;; Force the ledger directory and its entry in the existing data
            ;; directory, including the first issue that creates this store.
            (doseq [dir [(.getParentFile file) (.getParentFile (.getParentFile file))]]
              (with-open [directory (FileChannel/open
                                     (.toPath dir)
                                     (into-array StandardOpenOption [StandardOpenOption/READ]))]
                (.force directory true)))
            entry))))))

(defn refund!
  "Append a compensating event for one previously consumed ordinary click.
   This preserves the original charge and refuses unknown or already-refunded
   click IDs. REASON names the operator-authorized reason for restoration."
  [click-id refunded-at caller reason]
  (locking issue-lock
    (let [file (io/file *ledger-path*)]
      (when-not (.exists file)
        (throw (ex-info "ordinary click ledger is absent"
                        {:error :ordinary-click-refund-unknown :click-id click-id})))
      (with-open [channel (FileChannel/open
                          (.toPath file)
                          (into-array StandardOpenOption
                                      [StandardOpenOption/READ StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (let [entries (ledger-entries file)
              charge (some #(when (and (= authorization (:authorization %))
                                       (= click-id (:click-id %))
                                       (nil? (:event %))) %) entries)
              prior-refund (some #(when (and (= authorization (:authorization %))
                                             (= click-id (:click-id %))
                                             (= "refund" (:event %))) %) entries)]
          (when-not charge
            (throw (ex-info "ordinary click was not charged under this authority"
                            {:error :ordinary-click-refund-unknown :click-id click-id})))
          (when prior-refund
            (throw (ex-info "ordinary click was already refunded"
                            {:error :ordinary-click-already-refunded :click-id click-id})))
          (let [entry {:event "refund" :click-id click-id :refunded-at refunded-at
                       :authorization authorization :caller (or caller :caller-unknown)
                       :reason reason}
                buffer (ByteBuffer/wrap
                        (.getBytes (str (json/generate-string entry) "\n") "UTF-8"))]
            (.position channel (.size channel))
            (while (.hasRemaining buffer) (.write channel buffer))
            (.force channel true)
            entry))))))
