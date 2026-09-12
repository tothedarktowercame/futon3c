(let [runner-ns (find-ns 'futon2.aif.full-loop-runner)
      wiring-var (when runner-ns (ns-resolve runner-ns 'construction-wiring-result))]
  {:runner-loaded? (boolean runner-ns)
   :wiring-var? (boolean wiring-var)
   :wiring-arglists (:arglists (meta wiring-var))
   :nil-port-probe
   (when wiring-var
     (try
       (wiring-var {:shown ["iching/hexagram-43-guai"]} (constantly nil))
       (catch Throwable t
         {:thrown (.getName (class t)) :message (.getMessage t)})))})
