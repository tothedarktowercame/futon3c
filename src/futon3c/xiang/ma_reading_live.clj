(ns futon3c.xiang.ma-reading-live
  "Wire the 馬 alignment reading to the running service (required from
   futon3c.transport.http, never from tests): on each :ported operator turn,
   a bounded whistle to the 馬-sonnet seat."
  (:require [futon3c.xiang.ma-occurrence :as mo]
            [futon3c.xiang.ma-reading :as ma]))

(ma/start-live!)
(mo/start-live!)
