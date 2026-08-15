(ns write-apm-demonstration-launch-authorization
  (:require [clojure.java.io :as io]
            [futon3c.apm.preregistration :as prereg])
  (:import [java.security MessageDigest]))

(defn sha256 [path]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [input (io/input-stream path)]
      (let [buffer (byte-array 65536)]
        (loop []
          (let [n (.read input buffer)]
            (when (pos? n)
              (.update digest buffer 0 n)
              (recur))))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn -main [& [registration-path trace-path lean-repo agency-endpoint solver-seat
                authorization-revision output]]
  (when-not (every? some? [registration-path trace-path lean-repo
                           agency-endpoint solver-seat authorization-revision
                           output])
    (throw (ex-info
            "usage: write_apm_demonstration_launch_authorization.clj REGISTRATION TRACE LEAN_REPO AGENCY_ENDPOINT SOLVER_SEAT AUTHORIZATION_REVISION OUTPUT"
            {})))
  (when-not (re-matches #"[0-9a-f]{40}" authorization-revision)
    (throw (ex-info "authorization revision must be a full commit SHA"
                    {:revision authorization-revision})))
  (let [registration (prereg/read-edn registration-path)
        trace (prereg/read-edn trace-path)
        report (prereg/report registration trace lean-repo agency-endpoint
                              solver-seat)]
    (when-not (:launchable? report)
      (throw (ex-info "APM demonstration round 1 is not launchable"
                      {:report report})))
    (spit output
          (str (pr-str
                {:kind :apm-demonstration-round1-launch-authorization
                 :schema 1
                 :authorization-revision authorization-revision
                 :lean-revision (:lean-revision registration)
                 :lean-source-revision (:lean-source-revision report)
                 :registration-sha256 (sha256 registration-path)
                 :trace-sha256 (sha256 trace-path)
                 :problem (:problem registration)
                 :validation report})
               "\n"))))

(apply -main *command-line-args*)
