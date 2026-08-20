(ns futon3c.apm.problem-projection
  "Ledger-derived, content-addressed projection for the live *problem* buffer."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-projection :as campaign-projection]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file AtomicMoveNotSupportedException CopyOption Files
            OpenOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]))

(defn- path [value]
  (cond
    (instance? Path value) value
    (string? value) (Path/of value (make-array String 0))
    :else nil))

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
   {:keys [expected-frame-id expected-problem-id]}]
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
                     " (set-buffer-modified-p nil)) t)")
           result (run "emacsclient" "--eval" form)]
       (if (zero? (:exit result))
         {:ok true :buffer-name buffer-name}
         {:ok false :error/code :problem-projection-buffer-replace-failed
          :finding {:exit (:exit result) :stderr (str/trim (:err result))}})))))

(defn project!
  [{:keys [ledger-path certificate-path output-path buffer-sink
           expected-frame-id expected-problem-id]}]
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
                      :expected-problem-id expected-problem-id})]
        (if-not (:ok derived)
          derived
          (let [problem-projection (:problem-projection derived)
                content (render problem-projection)
                written (atomic-write! output-path content)]
            (if-not (:ok written)
              written
              (try
                (let [buffer-result
                      (buffer-sink {:buffer-name "*problem*" :content content
                                    :problem-projection problem-projection})]
                  (if (and (map? buffer-result) (:ok buffer-result))
                    {:ok true :projection problem-projection
                     :publication written :buffer buffer-result}
                    {:ok false :error/code :problem-projection-buffer-sink-failed
                     :publication written :buffer buffer-result}))
                (catch Throwable t
                  {:ok false :error/code :problem-projection-buffer-sink-threw
                   :publication written
                   :finding {:message (.getMessage t)}})))))))))

(defn project-latest!
  [{:keys [projection-directory] :as options}]
  (let [latest (campaign-projection/read-latest projection-directory)]
    (if-not (and (:ok latest) (map? (:pointer latest)))
      {:ok false :error/code :problem-projection-latest-certificate-unavailable
       :latest latest}
      (project! (assoc options :certificate-path
                       (get-in latest [:pointer :certificate/path]))))))
