(ns futon3c.apm.conductor-open
  "Cold production assembly for opening one APM conductor cycle."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.registry :as registry]
            [futon3c.apm.conductor :as conductor]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.peripheral.problem :as problem]
            [futon3c.substrate.client :as substrate]))

(def ^:private seat-keys
  [:reg/solver-seat :reg/student-seat :reg/guide-seat
   :reg/proctor-seat :reg/scribe-seat])

(defonce ^:private loaded-harness-revision
  ;; Capture process image provenance once. A Drawbridge reload must not move
  ;; the stamp forward to whatever revision happens to be in the git tree.
  (try
    (:harness-revision
     (problem/measure-harness-repository "/home/joe/code/futon3c"))
    (catch Throwable _ nil)))

(defn- refusal [code & [details]]
  (cond-> {:ok false :error/code code}
    details (merge details)))

(defn- nonblank [value]
  (some-> value str str/trim not-empty))

(defn- read-registration [path]
  (try
    {:ok true :registration (edn/read-string (slurp (io/file path)))}
    (catch Throwable throwable
      (refusal :registration-read-failed
               {:error/message (.getMessage throwable)
                :registration-path path}))))

(defn- normalize-mode [mode]
  (cond
    (keyword? mode) mode
    (string? mode) (keyword mode)
    :else mode))

(defn- harness-pin-check [registration options]
  (let [repo (or (:harness-repo options) "/home/joe/code/futon3c")
        measure (or (:harness-measurer options)
                    #_{:clj-kondo/ignore [:private-call]}
                    problem/measure-harness-repository)
        measured (:harness-revision (measure repo))
        pinned (:reg/harness-revision registration)
        loaded (cond
                 (contains? options :loaded-harness-revision)
                 (:loaded-harness-revision options)

                 ;; Existing test dependency injection models the git
                 ;; measurement only; absent an explicit image override it
                 ;; assumes the fixture registration is the loaded image.
                 (contains? options :harness-measurer) pinned

                 :else loaded-harness-revision)]
    (cond
      (nil? loaded)
      (refusal :harness-image-revision-unknown
               {:pinned pinned :loaded loaded})

      (not= pinned loaded)
      (refusal :harness-image-pin-mismatch
               {:pinned pinned :loaded loaded})

      (not= pinned measured)
      (refusal :harness-pin-stale {:pinned pinned :measured measured}))))

(defn- production-config
  [payload registration guide-session options]
  (let [frame (:frame payload)
        batch (:batch payload)
        guide-seat (:reg/guide-seat registration)
        problem-id (:problem-id payload)
        mode (normalize-mode (:mode payload))
        agency-base (or (:agency-base options)
                        dispatch-with-recall/default-agency-base)]
    {:session-id guide-session
     :problem-id problem-id
     :mode mode
     :deposit-state (if (= :store-mode mode) :with-deposit :n/a)
     :registration-path (:registration-path payload)
     :frame {:scaffold-path (:scaffold frame)
             :closing-path (:closing frame)
             :witness-path (:witness frame)}
     :checkout {:batch batch
                :base-rev (:reg/environment-revision registration)
                :solver-seat (:reg/solver-seat registration)
                :student-seat (:reg/student-seat registration)
                :recall-system "futon1b"}
     :evidence-store (or (:evidence-store options)
                         (f1b/make-futon1b-backend
                          (or (:evidence-store-url options)
                              (substrate/configured-url))))
     :evidence-store-url (or (:evidence-store-url options)
                             (substrate/configured-url))
     :harness-repo (or (:harness-repo options) "/home/joe/code/futon3c")
     ;; This is the path persisted by every recovered round-1 cycle. It is the
     ;; Mathlib checkout against which preregistration's Lean source pin is
     ;; measured, not the separately provisioned apm-lean worktree.
     :lean-repo (or (:lean-repo options) "/home/joe/code/mathlib4")
     :agency-endpoint (or (:agency-endpoint options)
                          (str agency-base "/api/alpha/invoke/jobs?limit=200"))
     :agency-base agency-base
     ;; These are frozen experimental arms, not caller preferences. The parsed
     ;; and validated registration is their only authority; omission preserves
     ;; the pre-f9 defaults for historical registrations.
     :memory-cascade-enabled?
     (true? (:reg/memory-cascade-enabled? registration))
     :memory-cascade-cap (:reg/memory-cascade-cap registration)
     :analyst-seat (:reg/analyst-seat registration)
     :close-hook (:close-hook options)
     :conductor {:agent guide-seat
                 :session guide-session
                 :surface :problem
                 :park-base agency-base}
     :peripheral (or (:peripheral options) (problem/make-problem))}))

(defn open!
  "Open a production problem cycle from a frozen registration.

   OPTIONS is dependency injection for tests only; absent overrides select the
   production problem peripheral and substrate adapters."
  ([payload] (open! payload {}))
  ([payload options]
   (let [registration-path (nonblank (:registration-path payload))
         problem-id (nonblank (:problem-id payload))
         batch (nonblank (:batch payload))]
     (cond
       (nil? registration-path)
       (refusal :registration-path-required)

       (nil? problem-id)
       (refusal :problem-id-required)

       (nil? batch)
       (refusal :batch-required)

       :else
       (let [read-result (read-registration registration-path)]
         (if-not (:ok read-result)
           read-result
           (let [registration (:registration read-result)
                 shape-failures (vec (prereg/registration-shape-failures
                                      registration))
                 registered-problem (get-in registration [:problem :problem-id])
                 guide-seat (:reg/guide-seat registration)
                 guide (when (nonblank guide-seat)
                         (registry/get-agent guide-seat))
                 guide-session (:agent/session-id guide)]
             (cond
               (seq shape-failures)
               (refusal :registration-shape-invalid
                        {:findings shape-failures})

               (not= problem-id registered-problem)
               (refusal :registration-problem-mismatch
                        {:expected registered-problem :received problem-id})

               (not (every? #(nonblank (get registration %)) seat-keys))
               (refusal :registration-shape-invalid
                        {:findings
                         (->> seat-keys
                              (remove #(nonblank (get registration %)))
                              (mapv (fn [seat-key]
                                      {:finding :unstaffed-seat
                                       :seat-key seat-key})))})

               (nil? guide)
               (refusal :conductor-guide-unregistered
                        {:guide-seat guide-seat})

               (not (true? (get-in (registry/registry-status)
                                   [:agents guide-seat :invoke-ready?])))
               (refusal :conductor-guide-not-invoke-ready
                        {:guide-seat guide-seat})

               (not (nonblank guide-session))
               (refusal :conductor-guide-session-absent
                        {:guide-seat guide-seat})

               (binding/lookup guide-seat guide-session)
               (refusal :conductor-binding-exists
                        {:guide-seat guide-seat})

               :else
               (if-let [pin-refusal (harness-pin-check registration options)]
                 pin-refusal
                 (let [config (production-config payload registration
                                                 guide-session options)
                       opened ((or (:open-frame-fn options)
                                   conductor/open-frame!)
                               config)]
                   (if (false? (:ok opened))
                     (merge {:ok false}
                            (select-keys opened [:error :error/code :findings]))
                     {:ok true
                      :cycle-id (:cycle-id opened)
                      :version (binding/handle-version opened)
                      :phase (get-in opened [:state :current-phase])
                      :seats (select-keys registration seat-keys)})))))))))))
