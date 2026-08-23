(ns library-lane-run
  "Run the Codex-only library lane for one keyed problem, for real.

  This is the lane's ENTRY POINT, deliberately its own script and its own JVM:
  the lane announces, activates, polls and banks through its own machinery
  (futon3c.apm.library-lane-*), so it must not be driven from inside the
  serving image where the countdown machine lives. Parameters arrive as
  environment variables so a dispatch loop can iterate problems without
  editing code.

  APM_PROBLEM     problem id, e.g. t00J02. Unset: take the head of the queue,
                  which is what the outer loop wants.
  APM_TARGET      keying target: the declaration whose axioms bank prints.
                  Unset: derive apm_<lowercased problem id> and REFUSE unless
                  that declaration is actually in the file.
  APM_AREA        obstruction area used to select the queue slice
  APM_TRUNK       branch the increment lands on
  APM_MAX         max problems this invocation may attempt (default 1)
  APM_DRY_RUN     when \"1\", stub the bank boundary only"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.library-lane :as lane]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-effects :as fx]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.library-lane-queue :as queue]))

(def corpus-root "/home/joe/code/apm-lean")
(def frames-root "/home/joe/code/apm-frames")
(def agency-base "http://localhost:7070")
(def state-root "/home/joe/code/futon3c-frame18-control/data/apm-lane")

(defn- env [k default] (or (System/getenv k) default))

(defn derive-keying-target
  "apm_<lowercased problem id>, confirmed present in the problem file.

  The convention holds across the corpus, but a convention is not evidence:
  bank runs `#print axioms <target>`, and a target that does not exist fails
  there, after an entire solve episode has been spent. Check it here, where
  refusing costs nothing."
  [corpus-root problem-id]
  (let [target (str "apm_" (str/lower-case problem-id))
        file (io/file corpus-root "problems" problem-id "lean" "Main.lean")]
    (when (.isFile file)
      (when (re-find (re-pattern (str "(?m)^(?:noncomputable )?(?:theorem|lemma) "
                                      (java.util.regex.Pattern/quote target)
                                      "\\b"))
                     (slurp file))
        target))))

(defn -main [& _]
  (let [area (env "APM_AREA" nil)
        problem-id (or (env "APM_PROBLEM" nil)
                       (first (lane/queue corpus-root :library area)))
        keying-target (or (env "APM_TARGET" nil)
                          (when problem-id
                            (derive-keying-target corpus-root problem-id)))
        trunk (env "APM_TRUNK" "repair/m97A06-energy-regularity")
        max-problems (Long/parseLong (env "APM_MAX" "1"))
        dry-run? (= "1" (env "APM_DRY_RUN" "0"))
        contract (edn/read-string
                  (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn"))]
    (when-not problem-id
      (println "REFUSED: no problem — queue empty for area" (pr-str area))
      (System/exit 3))
    (when-not keying-target
      (println "REFUSED: no keying target for" problem-id
               "— set APM_TARGET explicitly")
      (System/exit 2))
    (println "LANE" problem-id "target" keying-target "area" area
             "trunk" trunk (if dry-run? "(dry bank)" "(live bank)"))
    (let [eff (fx/live-effects {:agency-base agency-base
                               :corpus-root corpus-root
                               :frames-root frames-root})
          launched (launch/launch!
                    (merge eff {:corpus-root corpus-root
                                :problem-id problem-id
                                :trunk-branch trunk
                                :keying-target keying-target
                                :state-root state-root
                                :agency-base agency-base}))]
      (if-not (:ok launched)
        (println "LAUNCH REFUSED:" (pr-str launched))
        (let [c (:config launched)]
          (println "FRAME" (or (:frame-id c) (get-in c [:unit :frame/id]))
                   "seats" (pr-str (into {} (map (fn [[r s]] [r (:agent-id s)])
                                                 (:seats c)))))
          (let [result (queue/run-queue!
                        {:corpus-root corpus-root
                         :trunk-branch trunk
                         :area area
                         :max-problems max-problems
                         :dry-run? dry-run?
                         :contract contract
                         :seat (:seats c)
                         :phase-inputs-fn (adapters/make-phase-inputs-fn c)
                         :bank-request-fn (adapters/make-bank-request-fn c)})]
            (println "STOP:" (:stop-condition result))
            (doseq [r (:reports result)]
              (println "PROBLEM:" (:problem-id r))
              (println "  targets :" (:keying-targets r))
              (println "  ruling  :" (:ruling r))
              (println "  seam    :" (:seam r))
              (println "  receipt :" (:bank-receipt-id r))
              (println "  finding :" (pr-str (:finding r)))))))
      (flush)
      (shutdown-agents))))
