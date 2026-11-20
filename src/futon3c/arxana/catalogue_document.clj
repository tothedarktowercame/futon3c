(ns futon3c.arxana.catalogue-document
  "Render a catalogue view (catalogue-view/read-catalogue) as plain text for
  reading and editing in a Together document. Pure: no store, no network.
  One paragraph per line, because the browser page shows line breaks as they are."
  (:require [clojure.string :as str]))

(def sections
  "Choice-key namespace -> heading and one sentence of introduction, in order."
  [["together" "Real-time editing"
    "Several people edit one document at once, from Emacs and from a browser, through one hub in the Arxana JVM."]
   ["incubator" "The incubator form"
    "One person at a time revises the answers of a form, with layers, forks and the source of every word kept."]
   ["backend" "The store and its queries"
    "What the record store promises to the views that read it."]
   [nil "The catalogue's own rules"
    "Rules about the records that make up this catalogue."]])

(defn- status-line [{:keys [checks enforcement-status]}]
  (let [{:keys [status reason reasons]} enforcement-status
        runs (keep :latest-run checks)
        day (some-> (last (sort (map :event-time runs))) (subs 0 10))]
    (case status
      :enforced (str "Enforced: " (count checks) (if (= 1 (count checks)) " check" " checks")
                     " passed, last run " day ".")
      (str "Not enforced (" (name status)
           (when-let [why (seq (if reason [reason] reasons))]
             (str ": " (str/join ", " (map name why))))
           ")."))))

(defn- mark
  "Display mark for a choice: the Together page and Emacs show marked text in
  green (:ok), amber (:debt) or red (:bad), as the Emacs catalogue view does."
  [choice]
  (case (get-in choice [:enforcement-status :status])
    :enforced "ok"
    :violation "bad"
    "debt"))

(defn- choice-lines [{:keys [statement kind lifecycle sources checks] :as choice}]
  (let [key (:choice/key choice)
        {:keys [repo path section]} (first sources)
        m (mark choice)]
    [{:mark m :text (str "• " (name key) " — " statement)}
     {:mark m
      :text (str "  " (name kind) ", " (name lifecycle) ". " (status-line choice)
                 (when (seq checks) (str " Check: " (str/join ", " (map #(subs (str (:check/key %)) 1) checks)) "."))
                 (when repo (str " Source: " repo " " path (when section (str " (" section ")")) ".")))}
     {:text ""}]))

(defn lines
  "The catalogue as a vector of {:text line, :mark \"ok\"|\"debt\"|\"bad\"} maps;
  :mark is present only on a choice's own lines. VIEW must have :status :ok;
  anything else gives one line saying so, never a partial catalogue."
  [{:keys [status choices valid-time]}]
  (if (not= :ok status)
    [{:text (str "The catalogue could not be read completely (" status "); nothing is shown.")}]
    (let [known (set (keep first sections))
          group (fn [choice] (let [n (namespace (:choice/key choice))] (when (known n) n)))
          grouped (group-by group choices)
          enforced (count (filter #(= :enforced (get-in % [:enforcement-status :status])) choices))
          debt (remove #(= :enforced (get-in % [:enforcement-status :status])) choices)
          plain (fn [text] {:text text})]
      (vec
       (concat
        (map plain
             ["Arxana documents itself"
              ""
              (str "This is Demo A as a document: the design choices behind Arxana, each a record in the store with the checks that hold it to its word. "
                   "Read from the store at " valid-time ": " (count choices) " choices, " enforced " enforced, " (count debt) " not.")
              "Each entry gives the choice in one sentence, then its kind and standing, whether a check enforces it, and where it comes from. \"Enforced\" is never stored: it is worked out from the latest run of each check."
              "Green entries are enforced by a passing check; amber ones have no check or no run; red ones have a failing check."
              "This text was generated from the records. Editing it here changes this document only, not the records."
              ""])
        (mapcat (fn [[n heading intro]]
                  (when-let [cs (seq (sort-by :choice/key (grouped n)))]
                    (concat (map plain [(str "== " heading " ==") intro ""]) (mapcat choice-lines cs))))
                sections)
        (map plain
             ["== Debt =="
              (if (seq debt)
                "Choices that nothing enforces yet:"
                "Every choice is enforced by a passing check.")])
        (for [c (sort-by :choice/key debt)]
          {:mark (mark c) :text (str "• " (subs (str (:choice/key c)) 1) " — " (status-line c))})
        [(plain "")])))))

(defn render
  "Text of the whole catalogue, one paragraph per line."
  [view]
  (str/join "\n" (map :text (lines view))))
