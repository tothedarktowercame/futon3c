(ns futon3c.arxana.catalogue-document-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.catalogue-document :as doc]))

(defn- choice [key status & [checks]]
  {:choice/key key :statement (str "Statement of " (name key) ".") :kind :decision :lifecycle :accepted
   :sources [{:repo "futon3c" :path "src/x.clj" :section "f"}]
   :checks (or checks []) :enforcement-status status})

(def view
  {:status :ok :valid-time "2026-11-20T16:00:00.000Z"
   :choices [(choice :together/convergence {:status :enforced}
                     [{:check/key :together-probe/convergence
                       :latest-run {:result :pass :event-time "2026-11-20T15:13:40.542Z"}}])
             (choice :incubator/undo-is-an-act {:status :unknown :reason :no-check})
             (choice :scope {:status :unknown :reasons [:no-run]}
                     [{:check/key :scope}])
             (choice :elsewhere/thing {:status :enforced}
                     [{:check/key :x/y :latest-run {:result :pass :event-time "2026-11-19T00:00:00Z"}}])]})

(deftest renders-every-choice-once-under-its-heading
  (let [text (doc/render view) lines (str/split-lines text)]
    (is (= "Arxana documents itself" (first lines)))
    (is (str/includes? text "4 choices, 2 enforced, 2 not."))
    (doseq [k ["convergence" "undo-is-an-act" "scope" "thing"]]
      (is (= 1 (count (filter #(str/starts-with? % (str "• " k " — Statement")) lines))) k))
    (is (< (.indexOf text "== Real-time editing ==") (.indexOf text "• convergence")
           (.indexOf text "== The incubator form ==") (.indexOf text "• undo-is-an-act")
           (.indexOf text "== The catalogue's own rules ==") (.indexOf text "• scope")))
    ;; a namespace with no section of its own is not dropped
    (is (< (.indexOf text "== The catalogue's own rules ==") (.indexOf text "• thing")))
    (is (str/includes? text "Enforced: 1 check passed, last run 2026-11-20. Check: together-probe/convergence. Source: futon3c src/x.clj (f)."))
    (is (str/includes? text "Not enforced (unknown: no-check)."))
    (is (str/includes? text "• incubator/undo-is-an-act — Not enforced (unknown: no-check)."))
    (is (str/includes? text "• scope — Not enforced (unknown: no-run)."))
    (is (not-any? #(> (count (re-seq #"\n" %)) 0) lines))))

(deftest an-incomplete-read-shows-nothing
  (let [text (doc/render (assoc view :status :inconclusive))]
    (is (str/includes? text "could not be read completely"))
    (is (not (str/includes? text "convergence")))))

(deftest no-debt-says-so
  (is (str/includes? (doc/render (update view :choices #(vec (take 1 %))))
                     "Every choice is enforced by a passing check.")))
