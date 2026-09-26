(let [runner-ns (find-ns 'futon2.aif.full-loop-runner)
      wiring-var (when runner-ns (ns-resolve runner-ns 'construction-wiring-result))
      config-var (ns-resolve 'futon3c.transport.http '!handler-config)
      cfg (when config-var @(var-get config-var))]
  {:runner-loaded? (boolean runner-ns)
   :wiring-var? (boolean wiring-var)
   :wiring-arglists (:arglists (meta wiring-var))
   :nil-port-probe
   (when wiring-var
     (try
       (wiring-var {:shown ["iching/hexagram-43-guai"]} (constantly nil))
       (catch Throwable t
         {:thrown (.getName (class t)) :message (.getMessage t)})))
   :installed-run4? (map? (:run4 cfg))
   :installed-fold-fn? (fn? (get-in cfg [:run4 :construction-wiring-fn]))})
