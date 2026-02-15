# M-sliding-blackboard: Code Block Extraction for Emacs Chat

## Origin

Conversation between Joe and Claude in `*futon3c-chat*` (Emacs chat interface),
2026-02-15. Joe observed that code blocks render inline in the chat buffer and
proposed extracting them to a separate "sliding blackboard" — multiple stacked
side panels with proper fontification.

## The Idea

When Claude emits code blocks in the chat, instead of rendering them inline
(where they clutter the conversation flow), extract them to side buffers:

```
+---------------------+------------------+
|  *futon3c-chat*     | *code-1* (clj)   | <- Panel 1
|  (main convo)       | (defn foo ...)   |
|                     +------------------+
|  joe: Can you...    | *code-2* (py)    | <- Panel 2
|  claude: Sure...    | def bar():       |
|                     +------------------+
|  [code-1: clojure]  | *code-3* (bash)  | <- Panel 3
|  [code-2: python]   | git status       |
|                     +------------------+
+---------------------+
```

### Features

1. **Code blocks don't render inline** — replaced with clickable reference
   like `[code-1: clojure]`
2. **Multiple panels** stacked vertically on the right (or configurable)
3. **Navigate between panels**: `M-n` / `M-p` to slide through them
4. **Each panel is fontified** with the proper major mode (clojure-mode,
   python-mode, sh-mode, etc.)
5. **Optionally editable** — copy, modify, then paste back or save to file
6. **Panel management**: Close individual panels, close all, cycle visibility

### Implementation Approach

- Hook into chat display to **intercept code blocks before rendering**
  (detect markdown fence syntax: triple-backtick with language tag)
- Replace them with a clickable reference
- Maintain a **code block registry** (index, language, content, buffer)
- Use **window management** to stack panels in a dedicated side window
- Add **navigation commands** to cycle through the blackboard panels

### Prior Art

- `~/code/futon3/contrib/aob-chatgpt.el` — has an "intercept" pattern for
  catching code blocks from ChatGPT responses before they render. This could
  inform the interception mechanism.
- Emacs `display-buffer-in-side-window` for panel placement
- `indirect-buffer` or dedicated buffers with appropriate major modes

### Integration

- Works as a **minor mode** enabled in the chat buffer:
  ```elisp
  (with-current-buffer "*futon3c-chat*"
    (futon3c-code-blocks-mode 1))
  ```
- Should be a separate file (`futon3c-code-blocks.el`) to keep concerns clean
- Transport-aware: the Emacs transport could have special handling for
  structured content, similar to how the IRC transport handles message types

## Status

Idea captured from live conversation. Not yet implemented.
