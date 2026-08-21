(ns futon3c.apm.analyst-campaign
  "Post-close Analyst state machine. It never changes a frame ledger."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.io RandomAccessFile]
           [java.nio.charset StandardCharsets]))

(def tenure-limit 2)

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- addressed? [record id-key]
  (= (get record id-key)
     (machine/ledger-digest [(dissoc record id-key)])))

(defn register
  [{:keys [campaign-id analyst-seat analyst-card-path analyst-card-blob]}]
  (if-not (every? nonblank? [campaign-id analyst-seat analyst-card-path
                             analyst-card-blob])
    {:ok false :error/code :analyst-registration-pins-required}
    {:ok true
     :state {:analyst/version 1 :campaign/id campaign-id
             :analyst/processed-closes #{} :analyst/pending nil
             :analyst/series-inputs [] :analyst/regime-proposals []
             :analyst/tenure {:ordinal 1 :completed 0 :limit tenure-limit
                              :seat analyst-seat
                              :card {:path analyst-card-path
                                     :blob analyst-card-blob}}}}))

(defn wake-after-close
  "Mint exactly one durable obligation for a content-valid terminal close."
  [state close-receipt]
  (let [close-id (:receipt/id close-receipt)]
    (cond
      (not (and (= :frame-close (:receipt/type close-receipt))
                (= :closed (:receipt/result close-receipt))
                (addressed? close-receipt :receipt/id)))
      {:ok false :error/code :analyst-wake-close-invalid}
      (contains? (:analyst/processed-closes state) close-id)
      {:ok true :state state :status :already-completed}
      (= close-id (get-in state [:analyst/pending :close-receipt-id]))
      {:ok true :state state :status :already-pending
       :obligation (:analyst/pending state)}
      (:analyst/pending state)
      {:ok false :error/code :analyst-wake-already-pending}
      :else
      (let [tenure (:analyst/tenure state)
            body {:obligation/type :analyst-post-close
                  :campaign/id (:campaign/id state)
                  :frame-id (:receipt/frame-id close-receipt)
                  :problem-id (:receipt/problem-id close-receipt)
                  :close-receipt-id close-id
                  :analyst-seat (:seat tenure) :analyst-card (:card tenure)
                  :tenure/ordinal (:ordinal tenure)
                  :tenure/frame-number (inc (:completed tenure))
                  :tenure/limit (:limit tenure)}
            obligation (assoc body :obligation/id
                              (machine/ledger-digest [body]))]
        {:ok true :status :new :obligation obligation
         :state (assoc state :analyst/pending obligation)}))))

(defn- valid-handoff? [handoff]
  (and (map? handoff)
       (every? nonblank? [(:successor-seat handoff)
                          (get-in handoff [:successor-card :path])
                          (get-in handoff [:successor-card :blob])])
       (nonblank? (:handoff-receipt-id handoff))))

(defn accept-analysis
  "Accept a typed terminal Analyst report. Implementation packets are recorded
  only as proposals for a named later regime; this function cannot apply them."
  [state obligation report]
  (let [pending (:analyst/pending state)
        tenure (:analyst/tenure state)
        final-frame? (= (:limit tenure) (inc (:completed tenure)))
        packets (vec (:implementation-packets report))
        findings (cond-> []
                   (not= pending obligation) (conj :analyst-obligation-not-pending)
                   (not (addressed? obligation :obligation/id))
                   (conj :analyst-obligation-invalid)
                   (not= (:seat tenure) (:analyst-seat report))
                   (conj :analyst-seat-mismatch)
                   (not= (:card tenure) (:analyst-card report))
                   (conj :analyst-card-mismatch)
                   (not (map? (:series-entry report)))
                   (conj :analyst-series-entry-missing)
                   (not (vector? (:findings report)))
                   (conj :analyst-findings-missing)
                   (not (vector? (:implementation-packets report)))
                   (conj :analyst-packets-invalid)
                   (some #(not (nonblank? (:proposed-regime-id %))) packets)
                   (conj :analyst-packet-regime-boundary-missing)
                   (and final-frame? (not (valid-handoff? (:handoff report))))
                   (conj :analyst-successor-handoff-required))]
    (if (seq findings)
      {:ok false :error/code :analyst-terminal-invalid :findings findings}
      (let [body {:receipt/type :analyst-post-close
                  :campaign/id (:campaign/id state)
                  :frame-id (:frame-id obligation)
                  :problem-id (:problem-id obligation)
                  :close-receipt-id (:close-receipt-id obligation)
                  :obligation-id (:obligation/id obligation)
                  :analyst-seat (:seat tenure) :analyst-card (:card tenure)
                  :tenure/ordinal (:ordinal tenure)
                  :tenure/frame-number (:tenure/frame-number obligation)
                  :series-entry (:series-entry report)
                  :findings (:findings report)
                  :implementation-packets packets
                  :handoff (:handoff report)}
            receipt (assoc body :receipt/id (machine/ledger-digest [body]))
            completed (inc (:completed tenure))
            next-tenure
            (if final-frame?
              {:ordinal (inc (:ordinal tenure)) :completed 0
               :limit (:limit tenure)
               :seat (get-in report [:handoff :successor-seat])
               :card (get-in report [:handoff :successor-card])}
              (assoc tenure :completed completed))
            successor (-> state
                          (assoc :analyst/pending nil :analyst/tenure next-tenure)
                          (update :analyst/processed-closes conj
                                  (:close-receipt-id obligation))
                          (update :analyst/series-inputs conj
                                  {:receipt-id (:receipt/id receipt)
                                   :entry (:series-entry report)})
                          (update :analyst/regime-proposals into
                                  (mapv #(assoc % :analyst-receipt-id
                                                (:receipt/id receipt)) packets)))]
        {:ok true :receipt receipt :state successor
         :succession? final-frame?}))))

(defn append-series-input!
  "Append one accepted, content-addressed Analyst receipt as one EDN line."
  [path receipt]
  (if-not (and (= :analyst-post-close (:receipt/type receipt))
               (addressed? receipt :receipt/id))
    {:ok false :error/code :analyst-series-receipt-invalid}
    (do
      (io/make-parents (io/file (str path)))
      (with-open [raf (RandomAccessFile. (str path) "rw")
                  channel (.getChannel raf)
                  _lock (.lock channel)]
        (.position channel (.size channel))
        (.write channel
                (.encode StandardCharsets/UTF_8 (str (pr-str receipt) "\n")))
        (.force channel true))
      {:ok true :receipt/id (:receipt/id receipt)})))
