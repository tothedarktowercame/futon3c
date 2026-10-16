(ns futon3c.wm.cast-registry
  "Install the dedicated, independent ordinary-click execution cast."
  (:require [clojure.java.io :as io]
            [futon3c.agency.registry :as reg]))

(def specs
  [{:id "wm-author" :role "war-machine-author"}
   {:id "wm-reviewer" :role "war-machine-reviewer"}
   {:id "wm-repair-reviewer" :role "war-machine-repair-reviewer"}])

(defn install!
  []
  (let [make-fn (resolve 'futon3c.dev/make-codex-invoke-fn)]
    (mapv
     (fn [{:keys [id role]}]
       (let [session-file (io/file (str "/tmp/futon-codex-session-id-" id
                                        "-20261016"))
             session-id-atom (atom nil)
             invoke-fn (@make-fn {:agent-id id
                                  :session-file session-file
                                  :session-id-atom session-id-atom
                                  :cwd "/home/joe/code"
                                  :model "gpt-5.6-sol"})]
         (reg/register-agent!
          {:agent-id {:id/value id :id/type :continuity}
           :type :codex
           :invoke-fn invoke-fn
           :capabilities [:explore :edit :test :coordination/execute]
           :metadata {:auto-registered? false
                      :model "gpt-5.6-sol"
                      :role role
                      :freshness {:kind :new-role-session
                                  :boundary "2026-10-16-live-q7-q10"}
                      :cwd "/home/joe/code"}})
         (reg/update-agent! id :agent/invoke-fn invoke-fn)
         {:id id :registered? (reg/agent-registered? id)
          :session-file (.getPath session-file)
          :session-existed? (.exists session-file)}))
     specs)))
