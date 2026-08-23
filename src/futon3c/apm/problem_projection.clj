(ns futon3c.apm.problem-projection
  "Ledger-derived, content-addressed projection for the live *problem* buffer."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-projection :as campaign-projection]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file AtomicMoveNotSupportedException CopyOption Files
            OpenOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.time Instant]
           [java.util Base64]))

(defn- path [value]
  (cond
    (instance? Path value) value
    (string? value) (Path/of value (make-array String 0))
    :else nil))

(defn- sha256 [content]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes content StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn- receipt-refs [events frame-id]
  (->> events
       (keep (fn [event]
               (let [body (:event/body event)
                     certificate (:certificate body)]
                 (when (and (= frame-id (:receipt/frame-id certificate))
                            (string? (:receipt/id certificate)))
                   {:event/seq (:event/seq event)
                    :event/type (:event/type event)
                    :phase/from (:from body)
                    :phase/to (:to body)
                    :receipt/id (:receipt/id certificate)
                    :receipt/type (:receipt/type certificate)}))))
       vec))

(defn derive-projection
  "Derive one problem view from a validated ledger and snapshot certificate."
  [{:keys [events projection]} certificate
   {:keys [expected-frame-id expected-problem-id solver-progress operation]}]
  (let [active (:active/frame projection)
        certificate-active (:active/frame certificate)
        identity-keys [:frame-id :problem-id :phase :status
                       :registration-hash :harness-hash]
        certificate-body (dissoc certificate :certificate/id)
        mismatch
        (cond
          (not= :valid (:projection/status projection))
          :problem-projection-ledger-invalid
          (not= (:certificate/id certificate)
                (machine/ledger-digest [certificate-body]))
          :problem-projection-certificate-content-invalid
          (not= :valid (:snapshot/status certificate))
          :problem-projection-snapshot-not-valid
          (not= (:campaign/id projection) (:campaign/id certificate))
          :problem-projection-campaign-mismatch
          (not= (:campaign/version projection) (:campaign/version certificate))
          :problem-projection-version-mismatch
          (not= (:ledger/digest projection) (:ledger/digest certificate))
          :problem-projection-ledger-digest-mismatch
          (not= (:ledger/event-count projection) (:ledger/event-count certificate))
          :problem-projection-event-count-mismatch
          (nil? active) :problem-projection-active-frame-required
          (not= (select-keys active identity-keys)
                (select-keys certificate-active identity-keys))
          :problem-projection-active-frame-mismatch
          (and expected-frame-id (not= expected-frame-id (:frame-id active)))
          :problem-projection-frame-mismatch
          (and expected-problem-id (not= expected-problem-id (:problem-id active)))
          :problem-projection-problem-mismatch)]
    (if mismatch
      {:ok false :error/code mismatch}
      (let [body {:projection/type :apm-problem-buffer
                  :projection/version 1
                  :campaign/id (:campaign/id projection)
                  :campaign/version (:campaign/version projection)
                  :ledger/digest (:ledger/digest projection)
                  :ledger/event-count (:ledger/event-count projection)
                  :certificate/id (:certificate/id certificate)
                  :frame (select-keys active identity-keys)
                  :operation operation
                  :solver/progress solver-progress
                  :receipts (receipt-refs events (:frame-id active))}]
        {:ok true
         :problem-projection
         (assoc body :projection/id (machine/ledger-digest [body]))}))))

(defn render [problem-projection]
  (let [frame (:frame problem-projection)]
    (str "# APM problem projection\n\n"
         "Projection: `" (:projection/id problem-projection) "`  \n"
         "Ledger: `" (:ledger/digest problem-projection) "` ("
         (:ledger/event-count problem-projection) " events)  \n"
         "Certificate: `" (:certificate/id problem-projection) "`\n\n"
         "Frame: **" (:frame-id frame) "**  \n"
         "Problem: **" (:problem-id frame) "**  \n"
         "Phase: **" (some-> (:phase frame) name) "**  \n"
         "Status: **" (some-> (:status frame) name) "**\n\n"
         (when-let [operation (:operation problem-projection)]
           (str "Operational state: **"
                (some-> (:status operation) name) "**  \n"
                "Waiting for role: **"
                (or (some-> (:role operation) name) "none") "**  \n"
                "Seat: **" (or (:agent-id operation) "none") "**  \n"
                "Job: `" (or (:job-id operation) "none") "`\n\n"))
         (when-let [progress (:solver/progress problem-projection)]
           (str "Solver rounds completed: **" (:rounds/completed progress)
                " / " (:rounds/max progress) "**  \n"
                "Active solver round: **"
                (or (:round/active progress) "none") "**  \n"
                "Next strategy checkpoint: **"
                (or (:checkpoint/next progress) "none") "**\n\n"))
         "## Certified receipts\n\n"
         (if (seq (:receipts problem-projection))
           (apply str
                  (map (fn [receipt]
                         (str "- " (name (:receipt/type receipt)) " `"
                              (:receipt/id receipt) "` (event "
                              (:event/seq receipt) ")\n"))
                       (:receipts problem-projection)))
           "- none\n"))))

(defn- atomic-write! [target content]
  (let [target (path target)
        directory (some-> target .toAbsolutePath .getParent)]
    (if (or (nil? target) (nil? directory))
      {:ok false :error/code :problem-projection-output-path-invalid}
      (try
        (Files/createDirectories directory (make-array FileAttribute 0))
        (let [temporary (Files/createTempFile directory ".problem-" ".md"
                                              (make-array FileAttribute 0))]
          (try
            (Files/writeString temporary content StandardCharsets/UTF_8
                               (into-array OpenOption
                                           [StandardOpenOption/TRUNCATE_EXISTING
                                            StandardOpenOption/WRITE
                                            StandardOpenOption/SYNC]))
            (try
              (Files/move temporary target
                          (into-array CopyOption
                                      [StandardCopyOption/ATOMIC_MOVE
                                       StandardCopyOption/REPLACE_EXISTING]))
              (catch AtomicMoveNotSupportedException _
                (Files/move temporary target
                            (into-array CopyOption
                                        [StandardCopyOption/REPLACE_EXISTING]))))
            {:ok true :path (str target)}
            (finally (Files/deleteIfExists temporary))))
        (catch Throwable t
          {:ok false :error/code :problem-projection-atomic-write-failed
           :finding {:message (.getMessage t)}})))))

(defn emacs-buffer-sink
  "Replace BUFFER-NAME in one Emacs evaluation. RUN defaults to shell/sh and is
  injectable for tests. Content is base64-encoded, never interpolated as Lisp."
  ([payload] (emacs-buffer-sink shell/sh payload))
  ([run {:keys [buffer-name content]}]
   (if-not (and (fn? run) (string? buffer-name) (string? content))
     {:ok false :error/code :problem-projection-buffer-payload-invalid}
     (let [encoded (.encodeToString (Base64/getEncoder)
                                    (.getBytes content StandardCharsets/UTF_8))
           form (str "(let ((inhibit-read-only t)"
                     " (text (decode-coding-string (base64-decode-string \""
                     encoded "\") 'utf-8)))"
                     " (with-current-buffer (get-buffer-create " (pr-str buffer-name) ")"
                     " (erase-buffer) (insert text) (goto-char (point-min))"
                     " (set-buffer-modified-p nil)"
                     " (secure-hash 'sha256 (current-buffer))))")
           result (run "emacsclient" "--eval" form)]
       (if (zero? (:exit result))
         (let [observed (try (edn/read-string (str/trim (:out result)))
                             (catch Throwable _ nil))]
           (if (string? observed)
             {:ok true :buffer-name buffer-name :content/digest observed}
             {:ok false :error/code :problem-projection-buffer-readback-invalid
              :finding {:stdout (str/trim (:out result))}}))
         {:ok false :error/code :problem-projection-buffer-replace-failed
          :finding {:exit (:exit result) :stderr (str/trim (:err result))}})))))

(defn- publication-directory [{:keys [publication-directory output-path]}]
  (or (path publication-directory)
      (some-> (path output-path) .toAbsolutePath .getParent
              (.resolve "publications"))))

(defn transition-log-path [{:keys [transition-log-path output-path]}]
  (or (path transition-log-path)
      (some-> (path output-path) .toAbsolutePath .getParent
              (.resolve "problem-transitions.edn"))))

(defn- transition-key [receipt]
  (machine/ledger-digest
   [(select-keys receipt
                 [:ledger/digest :ledger/event-count :frame-id :problem-id
                  :phase :operation :solver/progress :buffer/name])]))

(defn- append-transition! [options receipt]
  (let [target (transition-log-path options)
        directory (some-> target .toAbsolutePath .getParent)]
    (if-not (and target directory)
      {:ok false :error/code :problem-projection-transition-log-path-invalid}
      (try
        (Files/createDirectories directory (make-array FileAttribute 0))
        (with-open [channel
                    (FileChannel/open
                     target
                     (into-array OpenOption
                                 [StandardOpenOption/CREATE
                                  StandardOpenOption/READ
                                  StandardOpenOption/WRITE
                                  StandardOpenOption/SYNC]))
                    _lock (.lock channel)]
          (let [entries
                (if (zero? (.size channel))
                  []
                  (->> (Files/readAllLines target StandardCharsets/UTF_8)
                       (remove str/blank?)
                       (mapv edn/read-string)))
                key (transition-key receipt)
                existing (some #(when (or (= key (:transition/key %))
                                          (= (:receipt/id receipt)
                                             (:publication/receipt-id %))) %)
                               entries)]
            (if existing
              {:ok true :status :already-logged :event existing
               :path (str target)}
              (let [body
                    {:event/type :problem-projection-transition
                     :event/sequence (count entries)
                     :event/observed-at (str (Instant/now))
                     :transition/key key
                     :publication/receipt-id (:receipt/id receipt)
                     :projection/id (:projection/id receipt)
                     :ledger/digest (:ledger/digest receipt)
                     :ledger/event-count (:ledger/event-count receipt)
                     :certificate/id (:certificate/id receipt)
                     :frame-id (:frame-id receipt)
                     :problem-id (:problem-id receipt)
                     :phase (:phase receipt)
                     :operation (:operation receipt)
                     :solver/progress (:solver/progress receipt)
                     :content/digest (:content/digest receipt)
                     :buffer/name (:buffer/name receipt)}
                    event (assoc body :event/id
                                 (machine/ledger-digest [body]))
                    encoded (.getBytes (str (pr-str event) "\n")
                                       StandardCharsets/UTF_8)]
                (.position channel (.size channel))
                (loop [buffer (ByteBuffer/wrap encoded)]
                  (when (.hasRemaining buffer)
                    (.write channel buffer)
                    (recur buffer)))
                (.force channel true)
                {:ok true :status :logged :event event :path (str target)}))))
        (catch Throwable t
          {:ok false :error/code :problem-projection-transition-log-failed
           :finding {:message (.getMessage t)}})))))

(defn- persist-publication! [options receipt]
  (let [directory (publication-directory options)
        receipt-id (:receipt/id receipt)]
    (if-not (and directory (string? receipt-id))
      {:ok false :error/code :problem-projection-publication-path-invalid}
      (let [receipt-path (.resolve directory (str receipt-id ".edn"))
            pointer-path (.resolve directory "latest.edn")
            receipt-write (atomic-write! receipt-path (str (pr-str receipt) "\n"))]
        (if-not (:ok receipt-write)
          receipt-write
          (let [transition (append-transition! options receipt)]
            (if-not (:ok transition)
              transition
              (let [pointer {:receipt/id receipt-id
                             :receipt/path (str receipt-path)
                             :projection/id (:projection/id receipt)
                             :ledger/digest (:ledger/digest receipt)
                             :transition/event-id
                             (get-in transition [:event :event/id])}
                    pointer-write (atomic-write! pointer-path
                                                 (str (pr-str pointer) "\n"))]
                (if (:ok pointer-write)
                  {:ok true :receipt receipt :pointer pointer
                   :transition transition}
                  pointer-write)))))))))

(defn project!
  [{:keys [ledger-path certificate-path output-path buffer-sink buffer-name
           expected-frame-id expected-problem-id solver-progress operation]
    :or {buffer-name "*problem*"}
    :as options}]
  (let [loaded-ledger (ledger/read-ledger ledger-path)
        loaded-certificate (snapshot/read-certificate certificate-path)]
    (cond
      (not (:ok loaded-ledger))
      {:ok false :error/code :problem-projection-ledger-unavailable
       :ledger loaded-ledger}
      (not (:ok loaded-certificate))
      {:ok false :error/code :problem-projection-certificate-unavailable
       :certificate loaded-certificate}
      (not (fn? buffer-sink))
      {:ok false :error/code :problem-projection-buffer-sink-required}
      :else
      (let [derived (derive-projection
                     loaded-ledger (:certificate loaded-certificate)
                     {:expected-frame-id expected-frame-id
                      :expected-problem-id expected-problem-id
                      :solver-progress solver-progress
                      :operation operation})]
        (if-not (:ok derived)
          derived
          (let [problem-projection (:problem-projection derived)
                content (render problem-projection)
                content-digest (sha256 content)
                written (atomic-write! output-path content)]
            (if-not (:ok written)
              written
              (let [disk-digest (sha256 (Files/readString (path output-path)))]
                (if-not (= content-digest disk-digest)
                  {:ok false :error/code :problem-projection-disk-readback-mismatch
                   :expected content-digest :observed disk-digest}
                  (try
                    (let [buffer-result
                          (buffer-sink {:buffer-name buffer-name :content content
                                        :content/digest content-digest
                                        :problem-projection problem-projection})]
                      (cond
                        (not (and (map? buffer-result) (:ok buffer-result)))
                        {:ok false :error/code :problem-projection-buffer-sink-failed
                         :publication written :buffer buffer-result}

                        (not= content-digest (:content/digest buffer-result))
                        {:ok false :error/code :problem-projection-buffer-readback-mismatch
                         :expected content-digest
                         :observed (:content/digest buffer-result)}

                        :else
                        (let [receipt-body
                              {:receipt/type :problem-projection-publication
                               :projection/id (:projection/id problem-projection)
                               :ledger/digest (:ledger/digest problem-projection)
                               :ledger/event-count (:ledger/event-count problem-projection)
                               :certificate/id (:certificate/id problem-projection)
                               :frame-id (get-in problem-projection [:frame :frame-id])
                               :problem-id (get-in problem-projection [:frame :problem-id])
                               :phase (get-in problem-projection [:frame :phase])
                               :operation (:operation problem-projection)
                               :solver/progress (:solver/progress problem-projection)
                               :output/path (:path written)
                               :buffer/name buffer-name
                               :content/digest content-digest}
                              receipt (assoc receipt-body :receipt/id
                                             (machine/ledger-digest [receipt-body]))
                              persisted (persist-publication! options receipt)]
                          (if (:ok persisted)
                            {:ok true :projection problem-projection
                             :publication written :buffer buffer-result
                             :publication-receipt (:receipt persisted)
                             :publication-pointer (:pointer persisted)
                             :transition (get-in persisted [:transition :event])}
                            persisted))))
                    (catch Throwable t
                      {:ok false :error/code :problem-projection-buffer-sink-threw
                       :publication written
                       :finding {:message (.getMessage t)}})))))))))))

(defn project-latest!
  [{:keys [projection-directory] :as options}]
  (let [latest (campaign-projection/read-latest projection-directory)]
    (if-not (and (:ok latest) (map? (:pointer latest)))
      {:ok false :error/code :problem-projection-latest-certificate-unavailable
       :latest latest}
      (project! (assoc options :certificate-path
                       (get-in latest [:pointer :certificate/path]))))))
