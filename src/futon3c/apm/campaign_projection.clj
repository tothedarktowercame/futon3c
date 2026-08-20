(ns futon3c.apm.campaign-projection
  "Rebuildable publication cache and stable rendering for campaign certificates.

  Immutable certificates are authority. The latest pointer and UI projection
  are disposable caches and never advance the campaign machine."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.channels FileChannel]
           [java.nio.file AtomicMoveNotSupportedException CopyOption Files LinkOption
            OpenOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn- display [x]
  (cond
    (nil? x) "—"
    (keyword? x) (name x)
    :else (str x)))

(defn- finding-line [{:keys [severity route code details]}]
  (str "  " (str/upper-case (display severity)) " "
       (display route) "/" (display code)
       (when details (str " " (pr-str details))) "\n"))

(defn render
  "Render a verified certificate value into a stable, glanceable text view."
  [certificate]
  (let [active (:active/frame certificate)
        reconciliation (:reconciliation certificate)
        findings (or (:findings reconciliation)
                     (when-let [failures (:observation/failures certificate)]
                       [{:severity :stale :route :observation
                         :code (:observation/error certificate)
                         :details {:failed-routes (vec (sort (keys failures)))}}]))]
    (str
     "CAMPAIGN " (str/upper-case (display (:snapshot/status certificate))) "\n"
     "Campaign: " (display (:campaign/id certificate))
     "  Series: " (display (:campaign/series certificate))
     "  Version: " (display (:campaign/version certificate)) "\n"
     "Certificate: " (display (:certificate/id certificate)) "\n"
     "Ledger: " (display (:ledger/digest certificate))
     "  events=" (display (:ledger/event-count certificate)) "\n"
     "Facts: " (display (:facts/digest certificate))
     "  observed=" (display (get-in certificate [:reconciliation :observed-at]
                                      (:generated-at certificate))) "\n"
     "Active: block=" (display (:active/block certificate))
     " frame=" (display (:frame-id active))
     " problem=" (display (:problem-id active))
     " phase=" (display (:phase active)) "\n"
     "Counts: blocks=" (display (get-in certificate [:counts :blocks]))
     " frames=" (display (get-in certificate [:counts :frames]))
     " closed=" (display (get-in certificate [:counts :closed-frames]))
     " stopped=" (display (get-in certificate [:counts :stopped-frames])) "\n"
     (if (seq findings)
       (str "Findings:\n" (apply str (map finding-line findings)))
       "Findings: none\n"))))

(defn- path [x]
  (cond
    (instance? Path x) x
    (string? x) (Path/of x (make-array String 0))
    :else nil))

(defn- parse-time [x]
  (when (string? x)
    (try (Instant/parse x) (catch Throwable _ nil))))

(defn read-latest [directory]
  (if-let [directory (path directory)]
    (let [latest (.resolve directory "latest.edn")]
      (try
        (if-not (Files/isRegularFile latest (make-array LinkOption 0))
          {:ok true :pointer nil :path (str latest)}
          {:ok true :pointer (edn/read-string
                              (Files/readString latest StandardCharsets/UTF_8))
           :path (str latest)})
        (catch Throwable t
          {:ok false :error/code :campaign-projection-pointer-unreadable
           :path (str latest) :finding {:message (.getMessage t)}})))
    {:ok false :error/code :campaign-projection-directory-invalid}))

(defn- pointer [certificate certificate-path]
  {:pointer/version 1
   :campaign/id (:campaign/id certificate)
   :campaign/version (:campaign/version certificate)
   :snapshot/status (:snapshot/status certificate)
   :generated-at (:generated-at certificate)
   :ledger/digest (:ledger/digest certificate)
   :certificate/id (:certificate/id certificate)
   :certificate/path (str certificate-path)})

(defn- candidate-order [current candidate]
  (cond
    (nil? current) :newer
    (not= (:campaign/id current) (:campaign/id candidate)) :campaign-conflict
    (< (:campaign/version candidate) (:campaign/version current)) :older
    (> (:campaign/version candidate) (:campaign/version current)) :newer
    (not= (:ledger/digest current) (:ledger/digest candidate)) :ledger-conflict
    (= (:certificate/id current) (:certificate/id candidate)) :same
    :else
    (let [current-at (parse-time (:generated-at current))
          candidate-at (parse-time (:generated-at candidate))]
      (cond
        (or (nil? current-at) (nil? candidate-at)) :time-invalid
        (.isAfter candidate-at current-at) :newer
        (.isBefore candidate-at current-at) :older
        :else :observation-conflict))))

(defn- atomic-write! [^Path target value]
  (let [directory (.getParent target)
        temporary (Files/createTempFile directory ".latest-" ".edn"
                                        (make-array FileAttribute 0))]
    (try
      (Files/writeString temporary (str (pr-str value) "\n")
                         StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/TRUNCATE_EXISTING
                                                 StandardOpenOption/WRITE
                                                 StandardOpenOption/SYNC]))
      (try
        (Files/move temporary target
                    (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                            StandardCopyOption/REPLACE_EXISTING]))
        (catch AtomicMoveNotSupportedException _
          (Files/move temporary target
                      (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
      (finally (Files/deleteIfExists temporary)))))

(defn publish-pointer!
  "Verify CERTIFICATE-PATH and monotonically publish it as DIRECTORY/latest.edn."
  [directory certificate-path]
  (let [directory (path directory)
        certificate-path (path certificate-path)]
    (cond
      (or (nil? directory) (nil? certificate-path))
      {:ok false :error/code :campaign-projection-path-invalid}

      :else
      (let [loaded (snapshot/read-certificate certificate-path)]
        (if-not (:ok loaded)
          {:ok false :error/code :campaign-projection-certificate-invalid
           :certificate loaded}
          (try
            (Files/createDirectories directory (make-array FileAttribute 0))
            (let [lock-path (.resolve directory ".latest.lock")]
              (with-open [channel (FileChannel/open
                                   lock-path
                                   (into-array OpenOption
                                               [StandardOpenOption/CREATE
                                                StandardOpenOption/WRITE]))
                          _lock (.lock channel)]
                (let [current-result (read-latest directory)
                      candidate (pointer (:certificate loaded) certificate-path)]
                  (if-not (:ok current-result)
                    current-result
                    (case (candidate-order (:pointer current-result) candidate)
                      :newer
                      (do (atomic-write! (.resolve directory "latest.edn") candidate)
                          {:ok true :published? true :pointer candidate})

                      :same
                      {:ok true :published? false :pointer candidate}

                      :older
                      {:ok false :error/code :campaign-projection-regression
                       :current (:pointer current-result) :candidate candidate}

                      :campaign-conflict
                      {:ok false :error/code :campaign-projection-campaign-conflict
                       :current (:pointer current-result) :candidate candidate}

                      :ledger-conflict
                      {:ok false :error/code :campaign-projection-ledger-conflict
                       :current (:pointer current-result) :candidate candidate}

                      :time-invalid
                      {:ok false :error/code :campaign-projection-time-invalid
                       :current (:pointer current-result) :candidate candidate}

                      :observation-conflict
                      {:ok false :error/code :campaign-projection-observation-conflict
                       :current (:pointer current-result) :candidate candidate})))))
            (catch Throwable t
              {:ok false :error/code :campaign-projection-publish-failed
               :finding {:message (.getMessage t)}})))))))

(defn project!
  "Publish CERTIFICATE-PATH monotonically, then send its stable rendering to
  PROJECT-FN. PROJECT-FN receives {:buffer-name :content :certificate} and is
  the only UI-specific effect. A failed UI write never alters the certificate."
  [directory certificate-path project-fn]
  (let [published (publish-pointer! directory certificate-path)]
    (if-not (:ok published)
      published
      (let [loaded (snapshot/read-certificate certificate-path)]
        (if-not (fn? project-fn)
          {:ok false :error/code :campaign-projection-sink-required
           :pointer (:pointer published)}
          (try
            (project-fn {:buffer-name "*problem*"
                         :content (render (:certificate loaded))
                         :certificate (:certificate loaded)})
            (assoc published :projected? true)
            (catch Throwable t
              {:ok false :error/code :campaign-projection-sink-failed
               :pointer (:pointer published)
               :finding {:message (.getMessage t)}})))))))
