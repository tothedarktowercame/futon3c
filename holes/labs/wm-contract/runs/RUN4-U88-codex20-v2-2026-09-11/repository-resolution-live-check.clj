(let [path "/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj"
      sha #(futon2.aif.c-fold-config/sha256 (slurp path))
      before (sha)]
  (assert (false? (:running? (futon3c.wm.runner-service/status))) "Runner must be idle")
  (assert (= before (sha)))
  (let [build (futon2.aif.full-loop-runner/resolve-build
               "ee5ab864f5df54cc5b2edb0581b57531f7d48028" "/home/joe/code/futon2")]
    (assert (= "/home/joe/code/futon2" (:repo build)))
    (assert (some #{"src/futon2/aif/contextual_preferences.clj"} (:files build)))
    {:source-sha256 before :resolved-build build
     :operation :read-only-commit-resolution :new-attempt? false}))
