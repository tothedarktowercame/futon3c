(ns futon3c.wm.r10-commission-binding
  "Server-owned binding for the single-use R10 WM click commission."
  (:require [futon3c.wm.r10-commission :as commission]))

(def authority-path
  "/home/joe/code/futon3c/holes/labs/wm-contract/r10-click-commission-2026-09-14-01.edn")

(def authority-sha256
  "7c5018493c8b10987277231c15c67b9fc9a2102e2c358c47bd688a2a303217df")

(def reservation-root
  "/home/joe/code/futon3c/data/r10-reservations")

(defn authorized-commission []
  (commission/load-authorized-commission
   {:authority-path authority-path
    :authority-sha256 authority-sha256}))
