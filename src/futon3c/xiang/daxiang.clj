(ns futon3c.xiang.daxiang
  "大象 (big elephant): the classical reader of agent turns.

   Tangled from notebooks/daxiang_live.clj; edit it there and run
   `-m futon3c.notebook.render tangle notebooks/daxiang_live.clj`.

   An agent writes each paragraph of a reply under a proforma mark that
   declares its intent, so reading it needs no model: the marks give the
   acts, `<mark>:` paragraphs point at what they answer, and a 🈸 or 🈯
   paragraph (not a pointer) means the turn ends waiting on the operator."
  (:require [futon3c.xiang.reply-target :as rt]
            [futon3c.xiang.turn-acts :as ta]
            [futon3c.xiang.turn-record :as tr]))

(defn asks-operator?
  "Whether reply TEXT ends its turn waiting on the operator: a paragraph
   opening 🈸 (ask-action) or 🈯 (clarify, including a question) that is
   not a `<mark>:` pointer answering the operator's own paragraph."
  [text]
  (boolean (and (string? text)
                (re-find #"(?m)^\s*(?:🈸|🈯)(?!\s*:)" text))))

(defn read-agent-turn
  "One agent turn, read classically. TURN is {:turn-id :agent-id
   :session-id :at :text :commits}. Returns {:acts [...] :skipped n
   :marks [{:mark :intent}] :answers [{:mark :index}] :asks-operator? b}:
   the kernel acts of its marked paragraphs and commits, every mark in
   order, the paragraphs that answer the operator's marked paragraphs,
   and whether it ends waiting on the operator."
  [{:keys [turn-id agent-id session-id at text commits]}]
  (let [record {:turn_id turn-id :agent_id agent-id :session_id session-id :created_at at}
        acts (ta/turn->acts record nil text (or commits []))]
    {:acts acts
     :skipped (:skipped (meta acts))
     :marks (mapv #(select-keys % [:mark :intent]) (tr/reply-marks text))
     :answers (mapv #(select-keys % [:mark :index]) (filter :pointer? (rt/operator-marks text)))
     :asks-operator? (asks-operator? text)}))
