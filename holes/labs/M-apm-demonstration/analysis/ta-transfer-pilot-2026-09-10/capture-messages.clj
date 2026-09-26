;; Read-only forensic snapshot of this commissioned Student's captured messages.
;; No keys, clients, backend objects, other sessions, or runtime mutations.
(try
 (let [a ((requiring-resolve 'futon3c.agency.registry/get-agent) "zai-student-transfer-20260910")
       f (:agent/invoke-fn a)
       field (.getDeclaredField (class f) "_BANG_messages")
       _ (.setAccessible field true)
       messages @(.get field f)
       opts-field (.getDeclaredField (class f) "opts")
       _ (.setAccessible opts-field true)
       opts (.get opts-field f)]
   ((requiring-resolve 'cheshire.core/generate-string)
    {:agent "zai-student-transfer-20260910"
     :captured-at (str (java.time.Instant/now))
     :model (select-keys opts [:model :max-tokens :temperature :memory-mode])
     :messages (mapv #(dissoc % :reasoning_content) messages)}))
 (catch Throwable t {:error (.getMessage t)}))
