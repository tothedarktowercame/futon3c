(ns futon3c.peripheral.evidence
  "Evidence emission helpers for peripheral execution.

   Every peripheral action produces an EvidenceEntry:
   - start → :goal claim (the session begins)
   - step → :step claim (a tool was invoked)
   - stop → :conclusion claim (the session produced fruit)

   These helpers produce shape-valid EvidenceEntry maps that can be
   appended directly to the evidence store via store/append! or store/append*.

   Evidence types: :coordination for most peripherals, :reflection for reflect."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- gen-id []
  (str "e-" (UUID/randomUUID)))

(defn- now-str []
  (str (Instant/now)))

(defn- session-ref
  "Create an ArtifactRef pointing to a session."
  [session-id]
  {:ref/type :session :ref/id session-id})

(defn- evidence-type-for
  "Determine the evidence type for a peripheral.
   Reflect emits :reflection; all others emit :coordination."
  [peripheral-id]
  (if (= peripheral-id :reflect)
    :reflection
    :coordination))

(defn make-start-evidence
  "Create an EvidenceEntry for peripheral start (claim-type :goal).
   This marks the beginning of a peripheral session."
  [peripheral-id session-id author]
  {:evidence/id (gen-id)
   :evidence/subject (session-ref session-id)
   :evidence/type (evidence-type-for peripheral-id)
   :evidence/claim-type :goal
   :evidence/author author
   :evidence/at (now-str)
   :evidence/body {:peripheral peripheral-id
                   :event :start}
   :evidence/tags [:peripheral (keyword (name peripheral-id))]
   :evidence/session-id session-id})

(defn make-step-evidence
  "Create an EvidenceEntry for a peripheral tool invocation (claim-type :step).
   in-reply-to: evidence-id of the previous entry in this session's chain."
  [peripheral-id session-id author tool args result in-reply-to]
  (cond-> {:evidence/id (gen-id)
           :evidence/subject (session-ref session-id)
           :evidence/type (evidence-type-for peripheral-id)
           :evidence/claim-type :step
           :evidence/author author
           :evidence/at (now-str)
           :evidence/body {:peripheral peripheral-id
                           :event :step
                           :tool tool
                           :args (vec args)
                           :result result}
           :evidence/tags [:peripheral (keyword (name peripheral-id))]
           :evidence/session-id session-id}
    in-reply-to (assoc :evidence/in-reply-to in-reply-to)))

(defn make-stop-evidence
  "Create an EvidenceEntry for peripheral stop (claim-type :conclusion).
   fruit: the peripheral's output (what the constrained session produced).
   in-reply-to: evidence-id of the previous entry in this session's chain."
  [peripheral-id session-id author fruit reason in-reply-to]
  (cond-> {:evidence/id (gen-id)
           :evidence/subject (session-ref session-id)
           :evidence/type (evidence-type-for peripheral-id)
           :evidence/claim-type :conclusion
           :evidence/author author
           :evidence/at (now-str)
           :evidence/body {:peripheral peripheral-id
                           :event :stop
                           :fruit fruit
                           :reason reason}
           :evidence/tags [:peripheral (keyword (name peripheral-id))]
           :evidence/session-id session-id}
    in-reply-to (assoc :evidence/in-reply-to in-reply-to)))
