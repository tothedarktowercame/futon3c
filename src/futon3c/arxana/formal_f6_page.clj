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
(defn- item-by-id [items id] (first (filter #(= id (:record/id %)) items)))
(defn- claim-label [items id]
  (or (get-in (item-by-id items id) [:record :asserted-content :content :text]) id))
(defn- support-number [supports support-id]
  (inc (.indexOf (vec (sort (map #(get-in % [:record :id]) supports))) support-id)))
(defn- support-mark [items supports item-id]
  (apply str (for [{:keys [record current?]} supports
                   :when (and current? (= item-id (endpoint-id record :supporting-content)))]
               (let [claim-id (endpoint-id record :supported-claim)]
                 (str "<div class=support data-record-id='" (esc (:id record)) "'><sup>["
                      (support-number supports (:id record)) "]</sup> supports “"
                      (esc (claim-label items claim-id)) "”" (visible-id claim-id)
                      (visible-id (:id record)) "</div>")))))
(defn- former-marks [items supports item-id]
  (apply str (for [{:keys [record current? supersession]} supports
                   :when (and (not current?) (= item-id (endpoint-id record :supporting-content)))]
               (str "<div class=former data-record-id='" (esc (:id record)) "'>formerly supported “"
                    (esc (claim-label items (endpoint-id record :supported-claim))) "”, changed by "
                    (esc (or (get-in supersession [:asserted-content :actor])
                             (get-in record [:asserted-content :actor]))) ": "
                    (esc (or (get-in supersession [:asserted-content :reason])
                             (get-in record [:asserted-content :reason]))) (visible-id (:id record)) "</div>"))))
(defn- svg-only [html]
  (when-let [[_ svg] (and html (re-find #"(?s)(<svg.*?</svg>)" html))] svg))
(defn- block [items supports geometry item]
  (let [r (:record item) id (:record/id item) kind (get-in r [:asserted-content :item/kind])
        c (content item)]
    (str "<article class='item " (name kind) "' data-record-id='" (esc id) "'><h2>"
         (esc (or (:title c) (:text c) (:description c) (name (item-key item)))) "</h2>"
         (when (= kind :diagram)
           (str "<div class=f5-diagram>" (or (svg-only (get-in geometry [:panels :cartesian])) "")
                (or (svg-only (get-in geometry [:panels :angular])) "")
                (apply str (map visible-id (:geometry/record-ids geometry))) "</div>"))
         (visible-id id) (support-mark items supports id) (former-marks items supports id) "</article>")))

(defn project [argument geometry]
  (let [items (sort-by placement-key (:items argument)) supports (:supports argument)
        main (filter #(= :main (get-in % [:placement :asserted-content :zone])) items)
        margin (filter #(= :margin (get-in % [:placement :asserted-content :zone])) items)
        html (str "<!doctype html><html><head><meta charset=utf-8><title>Formal Arxana F6</title>"
                  "<style>body{font:18px serif;max-width:1100px;margin:auto;display:grid;grid-template-columns:2fr 1fr;gap:3rem}"
                  ".item{margin:2rem 0}.margin{font-size:.85em}.record-id{font:10px monospace;overflow-wrap:anywhere;color:#666}"
                  ".support{border-left:3px solid #397;padding:.4rem}.former{border-left:3px solid #999;padding:.4rem;color:#666}"
                  "svg{max-width:100%}@media(max-width:700px){body{display:block;padding:1rem}}</style></head><body>"
                  "<main id=main>" (apply str (map #(block items supports geometry %) main)) "</main>"
                  "<aside id=margin class=margin>" (apply str (map #(block items supports geometry %) margin)) "</aside>"
                  "</body></html>")]
    {:result/status :complete :argument/id (:argument/id argument) :html html
     :item/ids (mapv :record/id items) :support/ids (mapv #(get-in % [:record :id]) supports)}))
