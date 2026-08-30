(require '[futon3c.apm.transport-conformance :as transport])

(let [[directory] *command-line-args*]
  (if-not directory
    (do (prn {:ok false
              :error/code :transport-certificate-directory-required})
        (System/exit 2))
    (let [result (transport/replay-directory directory)]
      (prn result)
      (when-not (:ok result) (System/exit 1)))))
