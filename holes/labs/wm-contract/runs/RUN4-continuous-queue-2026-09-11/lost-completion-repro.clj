(require '[futon3c.wm.run4-series-queue :as q]
 '[futon3c.wm.run4-series-queue-test :as qt]
 '[futon3c.wm.run4-series-service :as s]
 '[futon3c.wm.runner-service :as r])
(let [root (#'qt/temp-root) c (#'qt/fixture root "lost-completion") calls (atom 0)]
 (try
  (q/start! c)
  (#'q/persist! c (assoc (q/read-state! c) :in-flight {:entry-id "entry-1" :click-id "completed-before-process-exit"}))
  (reset! (var-get #'r/!completion) nil)
  (with-redefs [s/step! (fn [& _] (swap! calls inc) {:status :trial-terminal})]
   (println :result (select-keys (q/tick! c) [:status :reason :in-flight]))
   (println :durable-reader-calls @calls))
  (finally (q/stop! c) (#'qt/delete-tree! root))))
(shutdown-agents)
