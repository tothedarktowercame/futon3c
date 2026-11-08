(ns futon3c.arxana.formal-f5-render
  "Deterministic, record-driven Cartesian and angular geometry projector."
  (:require [clojure.string :as str]
            [futon3c.arxana.formal-f5-query :as fibre]
            [futon3c.arxana.records :as records]))
(def theory-version "formal-arxana/geometry-v1")
(defn- panel-id [id]
  (:id (records/make-clusion {:type-revision [:geometry/entity 1] :endpoints []
                              :asserted-content {:entity/id id :entity/kind :projection-panel
                                                 :theory/version theory-version}})))

(defn- ok! [r] (or (:ok r) (throw (ex-info "geometry public read failed" {:response r}))))
(defn- record [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- reverse-ids [read! type role id]
  (:result/edge-ids
   (ok! (read! :graph/query {:query/type :role-reverse :edge/type type
                             :endpoint/role role :endpoint/id id
                             :time/order :asc :query/limit 200}))))
(defn- endpoint-id [r role]
  (get-in (first (filter #(= role (:role %)) (:endpoints r))) [:target :clusion/id]))
(defn- escape [x]
  (str/escape (str x) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))
(defn- fmt [x] (String/format java.util.Locale/ROOT "%.3f" (object-array [(double x)])))

(defn- point-data [read! fibre-id]
  (->> (reverse-ids read! [:geometry/member 1] :fibre fibre-id)
       (map #(record read! %))
       (map (fn [membership]
              (let [point-id (endpoint-id membership :member)
                    point (record read! point-id)
                    values (map #(record read! %)
                                (reverse-ids read! [:geometry/value 1] :point point-id))
                    value (first values)]
                (when-not (= 1 (count values))
                  (throw (ex-info "point must have exactly one mathematical value"
                                  {:point/id point-id :value-count (count values)})))
                {:point point :membership membership :value value
                 :turn (/ (get-in value [:asserted-content :turn/numerator])
                          (get-in value [:asserted-content :turn/denominator]))})))
       (sort-by #(str (get-in % [:point :asserted-content :entity/id]))) vec))

(defn- placement [read! panel-id point-id]
  (->> (reverse-ids read! [:geometry/placement 1] :panel panel-id)
       (map #(record read! %))
       (filter #(= point-id (endpoint-id % :subject)))
       (sort-by :id) last))
(defn- common-record-ids [answer points extra-ids]
  (->> (concat (:record/ids answer) extra-ids
               (mapcat (fn [x] [(get-in x [:point :id])
                                 (get-in x [:membership :id])
                                 (get-in x [:value :id])]) points))
       distinct sort vec))
(defn- records-list [ids]
  (str "<ol class=records>"
       (apply str (map #(str "<li>" (escape %) "</li>") ids)) "</ol>"))

(defn- cartesian [read! panel circle-id points ids]
  (let [pid (:id panel)]
    (str "<section id=cartesian><h2>Cartesian-plane circle</h2>"
         "<svg viewBox='0 0 300 300' role=img><circle data-record-id='" (escape circle-id)
         "' cx=150 cy=150 r=110 fill=none stroke='#456'/><g>"
         (apply str
                (for [{:keys [point turn]} points
                      :let [a (* 2.0 Math/PI turn)
                            p (placement read! pid (:id point))
                            dx (or (get-in p [:asserted-content :offset/x]) 0)
                            dy (or (get-in p [:asserted-content :offset/y]) 0)
                            x (+ 150 (* 110 (Math/cos a)) dx)
                            y (+ 150 (* -110 (Math/sin a)) dy)]]
                  (str "<g data-record-id='" (escape (:id point)) "'><circle cx='" (fmt x)
                       "' cy='" (fmt y) "' r=7/><text x='" (fmt (+ x 9)) "' y='" (fmt y) "'>"
                       (escape (get-in point [:asserted-content :entity/id])) "</text></g>")))
         "</g></svg><h3>Records</h3>" (records-list ids) "</section>")))
(defn- angular [map-id points ids]
  (str "<section id=angular><h2>Angular projection (cut at 0 turns)</h2>"
       "<svg viewBox='0 0 600 180' role=img><line data-record-id='" (escape map-id)
       "' x1=30 y1=70 x2=570 y2=70 stroke='#456'/>"
       (apply str
              (for [{:keys [point turn]} points
                    :let [x (+ 30 (* 540 turn)) doubled (mod (* 2 turn) 1)]]
                (str "<g data-record-id='" (escape (:id point)) "'><circle cx='" (fmt x)
                     "' cy='70' r='7'/><text x='" (fmt x) "' y='45'>"
                     (escape (get-in point [:asserted-content :entity/id])) "</text><text x='"
                     (fmt x) "' y='105'>2θ=" (fmt doubled) "</text></g>")))
       "</svg><h3>Records</h3>" (records-list ids) "</section>"))

(defn render [read! body]
  (let [answer (fibre/fibre read! body)
        points (point-data read! (:fibre/id answer))
        cart-panel (record read! (or (:cartesian/panel-id body) (panel-id :panel/cartesian)))
        angular-panel (record read! (or (:angular/panel-id body) (panel-id :panel/angular)))
        map-record (first (filter #(= (:map/id body) (get-in % [:asserted-content :map/id]))
                                  (map #(record read! %) (:record/ids answer))))
        circle-id (endpoint-id map-record :domain)
        ids (common-record-ids answer points [circle-id])
        a (cartesian read! cart-panel circle-id points ids) b (angular (:id map-record) points ids)
        html (str "<!doctype html><html><head><meta charset=utf-8><title>Formal Arxana F5</title>"
                  "<style>body{font:16px sans-serif;display:grid;grid-template-columns:1fr 1fr;gap:1rem}"
                  "section{border:1px solid #999;padding:1rem}svg{width:100%;height:auto}.records{font:11px monospace;overflow-wrap:anywhere}"
                  "@media(max-width:700px){body{grid-template-columns:1fr}}</style></head><body>"
                  a b "</body></html>")]
    {:html html :panel/ids [(:id cart-panel) (:id angular-panel)]
     :geometry/record-ids ids :panels {:cartesian a :angular b}}))
