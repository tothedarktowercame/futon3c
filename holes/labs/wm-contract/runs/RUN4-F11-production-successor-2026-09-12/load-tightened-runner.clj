(do
  ;; Canonical master files only. The runner service resolves these Vars for
  ;; every click, so no listener restart or handler replacement is needed.
  (load-file "/home/joe/code/futon2/src/futon2/aif/fold.clj")
  (load-file "/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj")
  (let [wiring-var (ns-resolve 'futon2.aif.full-loop-runner
                               'construction-wiring-result)
        compare-var (ns-resolve 'futon2.aif.full-loop-runner
                                'selection-enaction-record)
        negative (wiring-var {:shown ["iching/hexagram-43-guai"]}
                             nil true)]
    {:runner-head "679746a3"
     :wiring-var? (boolean wiring-var)
     :selection-enaction-var? (boolean compare-var)
     :negative-control
     (select-keys negative [:status :failure-kind :findings])}))
