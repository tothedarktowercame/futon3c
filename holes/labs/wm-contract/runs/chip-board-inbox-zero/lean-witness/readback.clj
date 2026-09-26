(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[futon3c.agents.chip-board :as board]
         '[futon3c.agents.inbox-zero-board :as inbox])

(defn forms [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (loop [out []]
      (let [x (edn/read {:eof ::eof} r)]
        (if (= ::eof x) out (recur (conj out x)))))))
(defn tag [x] (if x (subs (str x) 1) ""))
(defn line! [xs] (println (str/join "|" xs)))

(let [rows (forms "holes/labs/wm-contract/runs/chip-board-inbox-zero/run-2026-09-12.txt")
      cert (:certificate (last rows))
      ;; Only reconstruct the board's target substitution. These are NOT the
      ;; retained production inputs; no input digest or real I/O is certified.
      resolved (inbox/resolve-args {:sweep [{:repo "futon5a" :clean? false}]})
      chips (into {} (map (juxt :chip/id identity) (:chips resolved)))]
  (assert (= :chip-board/run-v0 (:cert/type cert)))
  (assert (= (:board/id resolved) (:board/id cert)))
  (assert (true? (:verified? cert)))
  (assert (= :end/yield (:end-reason (last rows)) (get-in cert [:claim :end-reason])))
  (assert (= (:board/digest cert) (board/board-digest resolved)))
  (assert (nil? (board/validate-board resolved)))
  (assert (nil? (board/validate-wiring resolved)))
  (line! ["BOARD" (:board/digest cert)])
  (line! ["INPUTS" (:inputs/digest cert)])
  (line! ["END" (tag (get-in cert [:claim :end-reason]))])
  (doseq [c (:chips resolved)]
    (line! ["CHIP" (tag (:chip/id c)) (tag (:verb c))
            (get-in c [:args :repo] "")
            (tag (get-in c [:wires :true])) (tag (get-in c [:wires :false]))]))
  (doseq [r (get-in cert [:claim :trace])]
    (let [c (get chips (:chip r))]
      (assert (= (:verb c) (:verb r)))
      (assert (contains? #{:true :false} (:branch r)))
      (line! ["ROW" (tag (:chip r)) (tag (:verb r)) (name (:branch r))
              (:board/digest r) (get-in c [:args :repo] "")
              (str/join ","
                        (for [[kind payload] (:effects r)]
                          (do (assert (contains? #{:observe :report :refusal :yield-turn :commit} kind))
                              (if (= :commit kind)
                                (do (assert (= :commit (:mode payload)))
                                    (str "commit:" (:repo payload)))
                                (name kind)))))]))))
