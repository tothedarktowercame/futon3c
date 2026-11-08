(ns futon3c.arxana.formal-f6-page
  "Deterministic Tufte-style projection of the public F6 argument view."
  (:require [clojure.string :as str]))

(defn- esc [x] (str/escape (str x) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))
(defn- content [item] (get-in item [:record :asserted-content :content]))
(defn- item-key [item] (get-in item [:record :asserted-content :item/id]))
(defn- endpoint-id [record role]
  (get-in (first (filter #(= role (:role %)) (:endpoints record))) [:target :clusion/id]))
(defn- placement-key [item]
  [(get-in item [:placement :asserted-content :order]) (str (:record/id item))])
(defn- visible-id [id] (str "<div class=record-id>" (esc id) "</div>"))
(defn- support-mark [supports item-id]
  (apply str (for [{:keys [record current?]} supports
                   :when (and current? (= item-id (endpoint-id record :supporting-content)))]
               (str "<div class=support data-record-id='" (esc (:id record)) "'>supports → "
                    (esc (endpoint-id record :supported-claim)) (visible-id (:id record)) "</div>"))))
(defn- former-marks [supports item-id]
  (apply str (for [{:keys [record current?]} supports
                   :when (and (not current?) (= item-id (endpoint-id record :supporting-content)))]
               (str "<div class=former data-record-id='" (esc (:id record)) "'>formerly supported "
                    (esc (endpoint-id record :supported-claim)) ", changed by "
                    (esc (get-in record [:asserted-content :actor])) ": "
                    (esc (get-in record [:asserted-content :reason])) (visible-id (:id record)) "</div>"))))
(defn- block [supports item]
  (let [r (:record item) id (:record/id item) kind (get-in r [:asserted-content :item/kind])
        c (content item)]
    (str "<article class='item " (name kind) "' data-record-id='" (esc id) "'><h2>"
         (esc (or (:title c) (:text c) (:description c) (name (item-key item)))) "</h2>"
         (when (= kind :diagram)
           (str "<svg viewBox='0 0 220 140' role=img data-record-id='" (esc id)
                "'><circle cx=70 cy=70 r=48 fill=none stroke=currentColor/><circle cx=70 cy=22 r=5/>"
                "<circle cx=70 cy=118 r=5/><line x1=130 y1=70 x2=205 y2=70 stroke=currentColor/></svg>"))
         (visible-id id) (support-mark supports id) (former-marks supports id) "</article>")))

(defn project [argument]
  (let [items (sort-by placement-key (:items argument)) supports (:supports argument)
        main (filter #(= :main (get-in % [:placement :asserted-content :zone])) items)
        margin (filter #(= :margin (get-in % [:placement :asserted-content :zone])) items)
        html (str "<!doctype html><html><head><meta charset=utf-8><title>Formal Arxana F6</title>"
                  "<style>body{font:18px serif;max-width:1100px;margin:auto;display:grid;grid-template-columns:2fr 1fr;gap:3rem}"
                  ".item{margin:2rem 0}.margin{font-size:.85em}.record-id{font:10px monospace;overflow-wrap:anywhere;color:#666}"
                  ".support{border-left:3px solid #397;padding:.4rem}.former{border-left:3px solid #999;padding:.4rem;color:#666}"
                  "svg{max-width:100%}@media(max-width:700px){body{display:block;padding:1rem}}</style></head><body>"
                  "<main id=main>" (apply str (map #(block supports %) main)) "</main>"
                  "<aside id=margin class=margin>" (apply str (map #(block supports %) margin)) "</aside>"
                  "</body></html>")]
    {:result/status :complete :argument/id (:argument/id argument) :html html
     :item/ids (mapv :record/id items) :support/ids (mapv #(get-in % [:record :id]) supports)}))
