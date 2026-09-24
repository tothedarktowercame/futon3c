(ns futon3c.agents.kimi-api
  "Kimi coding-plan provider for the shared OpenAI-style agent harness.

   Kimi (api.kimi.com, the Kimi For Coding subscription) speaks the same
   OpenAI-compatible chat-completions dialect as Z.AI, including tool calls, so
   the agent loop, tool set, evidence wiring and interrupt handling all come
   from futon3c.agents.zai-api. Only the provider-shaped parts live here: the
   endpoint, the model names, where the key lives, and the sampling block Kimi
   will accept.

   Verified against the live account 2026-09-23:
   - GET /coding/v1/models -> k3, k3-256k, kimi-for-coding (K2.8 Preview),
     kimi-for-coding-highspeed (K2.7).
   - An explicit temperature is REFUSED (\"invalid temperature: only 0.6 is
     allowed for this model\", and the one allowed value moves with the
     reasoning effort), so this provider omits the field and takes the model's
     own default. This is why zai-api's sampling block is pluggable.
   - thinking/reasoning_effort are accepted; these models reason by default at
     effort \"high\" (k3) or \"max\" (kimi-for-coding). A multi-round tool loop
     pays that on every round, so the default here is \"low\": reasoning stays
     on, the per-round tax does not."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agents.zai-api :as zai-api]))

(def default-base-url
  "https://api.kimi.com/coding/v1")

(def default-model "k3")

(def key-files
  ["~/.kimikey" "~/.kimi-key"])

(def default-sampling
  "Kimi's accepted sampling block. nil omits the field from the request body."
  {:temperature nil
   :thinking nil
   :reasoning-effort "low"})

(def default-context-policy
  "Requisition gate for kimi seats (zai-api/requisition-decision and
   context-carry-decision). Every call carries a line
   `Requisition: <M-*|E-*|T-*> — <purpose>`; when the target changes, the
   seat's conversation is cleared before the job runs. :cap-tokens is a
   placeholder: a same-target conversation past it is cleared until
   same-target compaction exists. k3's context is 1,048,576 tokens, so 512k
   leaves a long job ~500k of room to grow. On 2026-09-24 the seats carried
   one session across all their dispatches and opened jobs at 166k-335k
   tokens, which exhausted the 5-hour quota twice
   (holes/labs/kimi-5h-limit-2026-09-24.md)."
  {:cap-tokens 512000})

(def api-key-hint
  "Kimi API key missing; set KIMI_API_KEY or create ~/.kimikey")

(defn- getenv [k]
  (some-> (System/getenv k) str/trim not-empty))

(defn- read-key-file [path]
  (try
    (let [f (io/file (str/replace-first path #"^~" (System/getProperty "user.home")))]
      (when (.exists f)
        (some-> f slurp str/trim not-empty)))
    (catch Throwable _ nil)))

(defn resolve-api-key
  []
  (or (getenv "KIMI_API_KEY")
      (some read-key-file key-files)))

(defn make-invoke-fn
  "Return an Agency invoke-fn backed by Kimi tool calling.

   Takes the same option map as `zai-api/make-invoke-fn`; anything the caller
   supplies wins over the Kimi defaults applied here."
  [{:keys [agent-id base-url model sampling context-policy] :as opts
    :or {agent-id "kimi"}}]
  (zai-api/make-invoke-fn
   (merge opts
          {:agent-id agent-id
           :api-key-fn resolve-api-key
           :api-key-hint api-key-hint
           :session-id-prefix "kimi-"
           :env-prefix "KIMI"
           :base-url (or base-url (getenv "KIMI_BASE_URL") default-base-url)
           :model (or model (getenv "KIMI_MODEL") default-model)
           ;; Every model on this plan reports supports_image_in, and the
           ;; endpoint accepts image parts inside a tool-role result (verified
           ;; live 2026-09-23), so a kimi seat gets view_image.
           :vision? true
           :sampling (merge default-sampling sampling)
           :context-policy (merge default-context-policy context-policy)})))
