(ns futon3c.agency.invoke-activity
  "What an invoking lane is actually doing, in one line.

  2026-09-09: claude-5 sat at `invoke-activity \"using Bash\"` for eight
  minutes. The string was fresh and the lane was healthy, so staleness (see
  `invoke-activity-test`) said nothing — but the operator still had to go
  read the session transcript to learn it was running the queue-supervisor
  tests. A tool NAME is not an activity. The tool's own arguments carry the
  answer, and the adapters were throwing them away.

  Claude's `tool_use` blocks already carry a `description` written for a
  human; when they don't, one salient input (the path, the pattern, the
  command) is still worth more than the bare verb."
  (:require [clojure.string :as str]))

(def ^:private max-detail-chars
  "A detail longer than this is a paragraph, not a status line."
  56)

(def ^:private max-activity-chars
  "The roster renders activity on its own indented line in an 80-col buffer."
  72)

(defn- clip
  [s limit]
  (let [s (some-> s str str/trim)]
    (when-not (str/blank? s)
      (let [flat (-> s
                     (str/replace #"\s*\n\s*" " ")
                     (str/replace #"\s{2,}" " "))]
        (if (<= (count flat) limit)
          flat
          (str (str/trimr (subs flat 0 (dec limit))) "…"))))))

(defn- basename
  [path]
  (when-let [p (some-> path str str/trim not-empty)]
    (last (str/split p #"/"))))

(defn- first-command-line
  "The first line of the command as the model wrote it — heredoc bodies and
   multi-line pipelines below it are detail, not identity."
  [command]
  (when-let [c (some-> command str not-empty)]
    (let [line (-> c str/trim (str/split #"\n") first str/trim)]
      (not-empty line))))

(defn- host-of
  [url]
  (when-let [u (some-> url str not-empty)]
    (second (re-find #"^[a-zA-Z]+://([^/]+)" u))))

(defn detail-for
  "One salient argument of a tool call, as a human would name it. nil when the
   call carries nothing worth surfacing."
  [{:keys [name input]}]
  (let [tool (some-> name str)
        lower (some-> tool str/lower-case)
        {:keys [description command file_path pattern path url query
                prompt subagent_type notebook_path to]} input
        described (clip description max-detail-chars)]
    (case lower
      "bash" (or described (clip (first-command-line command) max-detail-chars))
      ("read" "notebookedit") (clip (basename (or file_path notebook_path)) max-detail-chars)
      ("edit" "write") (clip (basename file_path) max-detail-chars)
      ("grep" "glob") (clip (str pattern
                                 (when-let [p (not-empty (str (or path "")))]
                                   (str " in " (basename p))))
                            max-detail-chars)
      ("task" "agent") (or described
                           (clip (or subagent_type prompt) max-detail-chars))
      "webfetch" (clip (or (host-of url) url) max-detail-chars)
      "websearch" (clip query max-detail-chars)
      "sendmessage" (clip (str to) max-detail-chars)
      ;; An MCP tool names its own arguments; take the first scalar that a
      ;; reader would recognise as the object of the verb.
      (or described
          (clip (or command file_path pattern query url to) max-detail-chars)))))

(defn- label-for
  [{:keys [name]}]
  (let [tool (some-> name str not-empty)]
    (cond
      (nil? tool) "tool"
      ;; mcp__gmail__send_message reads as gmail/send_message
      (str/starts-with? tool "mcp__")
      (let [[_ server & rest*] (str/split tool #"__")]
        (str server "/" (str/join "/" rest*)))
      :else (str/lower-case tool))))

(defn describe-tool-call
  "`bash: Run queue supervisor tests`, or just `bash` when the call says
   nothing more."
  [detail-map]
  (let [label (label-for detail-map)]
    (if-let [detail (detail-for detail-map)]
      (str label ": " detail)
      label)))

(defn tool-details->activity
  "An activity string for one assistant message's tool calls.

   Falls back to the old `using <tool>` shape when the calls carry no
   arguments at all, so a lane never reads as blank."
  [tool-details]
  (let [calls (filter map? tool-details)]
    (when (seq calls)
      (let [described (map describe-tool-call calls)
            joined (str/join " + " described)]
        (or (clip joined max-activity-chars)
            (str "using " (str/join ", " (keep :name calls))))))))
