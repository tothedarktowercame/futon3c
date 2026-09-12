(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[futon3c.agents.chip-board :as board]
         '[futon3c.agents.cascade-verifier-board :as cascade])

(defn read1 [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (let [x (edn/read {:eof ::eof} r)]
      (assert (not= ::eof x))
      (assert (= ::eof (edn/read {:eof ::eof} r))) x)))
(defn tag [x] (if x (subs (str x) 1) ""))
(defn line! [xs] (println (str/join "|" xs)))

(let [recorded (read1 "holes/labs/wm-contract/runs/chip-board-cascade-verifier/run-2026-09-12.edn")
      cert (:certificate recorded)
      debt (get-in recorded [:final-state :shelf-debt])
      inputs {:verification-debt debt}
      b (#'cascade/resolve-args inputs)
      replayed (cascade/run inputs (fn [_] nil))
      effects (vec (mapcat :effects (:trace recorded)))
      observe (second (first effects))
      request (second (second effects))
      meters (:meters (second (nth effects 2)))]
  ;; Stronger than verify-trace: compare the actual effects and final state too.
  (assert (= (select-keys recorded [:trace :end-reason :final-state])
             (select-keys replayed [:trace :end-reason :final-state])))
  (assert (= :chip-board/run-v0 (:cert/type cert)))
  (assert (= "b-cascade-verifier-0" (:board/id cert)))
  (assert (true? (:verified? cert)))
  (assert (= (:board/digest cert) (board/board-digest b)))
  (assert (nil? (board/validate-board b)))
  (assert (nil? (board/validate-wiring b)))
  (assert (= 324 (:count observe) (count debt) (count (:ids observe))))
  (assert (= (mapv :id debt) (:ids observe)))
  (assert (= (:id request) (:id (first debt))))
  (assert (every? #{:observe :verify-request :report :yield-turn :typed-none} (map first effects)))
  (line! ["BOARD" (:board/digest cert)])
  (line! ["END" (tag (:end-reason recorded))])
  (line! ["DEBT" (:count observe)])
  (line! ["REQUEST" (:id request) (:pointer request) (:basis-sha256 request)
          (tag (:freshness request)) (:basis-count request)])
  (line! ["METERS" (:range-finder meters) (:fuel meters) (:damage meters)])
  (doseq [c (:chips b)]
    (line! ["CHIP" (tag (:chip/id c)) (tag (:verb c))
            (get-in c [:args :id] "")
            (tag (get-in c [:wires :true])) (tag (get-in c [:wires :false]))]))
  (doseq [r (:trace recorded)]
    (assert (contains? #{:true :false} (:branch r)))
    (line! ["ROW" (tag (:chip r)) (tag (:verb r)) (name (:branch r))
            (:board/digest r) (str/join "," (map (comp name first) (:effects r)))])))
