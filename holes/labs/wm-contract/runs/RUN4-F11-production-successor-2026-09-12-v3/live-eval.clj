(try
  (load-file "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v3/install-and-launch.clj")
  (catch Throwable t
    {:class (.getName (class t)) :message (.getMessage t)
     :cause (some-> t .getCause .getMessage)
     :data (ex-data t)}))
