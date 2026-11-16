(ns futon3c.xiang.ma-reading-live
  "Wire the 馬 alignment reading to the running service (required from
   futon3c.transport.http, never from tests): on each :ported operator turn,
   a bounded whistle to the 馬-sonnet seat."
  (:require [futon3c.agency.dog-provider :as dog]
            [futon3c.xiang.lu-memory :as lum]
            [futon3c.xiang.lu-rerank :as lu]
            [futon3c.xiang.ma-occurrence :as mo]
            [futon3c.xiang.ma-reading :as ma]))

(ma/start-live!)
(mo/start-live!)
(lu/start-live!)
(lum/start-live!)
;; 狗's recall (:recorded) and use/burial (:settled) listener. It was only
;; ever started by hand, so the 2026-11-13 JVM start left 狗 at "not-ready"
;; on every operator turn, with no use checks or burials, until 2026-11-16.
(dog/start-live!)

;; Packet 2b: the 驴/m40 read resident prewarmed in the background at JVM
;; start, after the listeners — the same pattern as pattern-search.
(lum/prewarm!)
