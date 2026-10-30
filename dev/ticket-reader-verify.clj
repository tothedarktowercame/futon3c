;; Pre-implementation verification on REAL live ticket sources. Read-only.
;; Run from /home/joe/worktrees/futon3c/q1-ticket-admission with futon2 override.
(require '[futon2.aif.mission-registry :as registry]
         '[futon2.aif.wm.construction-inputs :as sources]
         '[futon2.aif.task-completion-surface :as completion]
         '[cheshire.core :as json])
(let [tickets (:tickets (registry/load-tickets "/home/joe/code"))
      live (filter registry/live-ticket? tickets)
      dead (remove registry/live-ticket? tickets)
      _ (prn :tickets (count tickets) :live (count live) :non-live (count dead))
      _ (prn :status-classes (frequencies (map :status-class live)))
      decls (sources/target-source-declarations
             (mapv :id live)
             {:loaded-missions {:missions []} :loaded-excursions {:excursions []}
              :loaded-tickets {:tickets tickets} :ticket-targets (mapv :id live)})
      ok (count (remove :source-absent decls))
      absent (count (filter :source-absent decls))
      _ (prn :declarations ok :source-absent absent)
      sample (first decls)
      _ (prn :declaration-sample (select-keys sample [:target :source]))
      ;; completion/describe on first 5 live tickets
      comps (map (fn [t] (try {:id (:id t) :completion (completion/describe t)}
                              (catch Exception e {:id (:id t) :error (:kind (ex-data e))})))
                 (take 5 live))
      _ (prn :completion-samples comps)
      ;; non-live exclusion shape
      _ (prn :non-live-sample (map #(select-keys % [:id :status-class]) (take 3 dead)))]
  (shutdown-agents))
