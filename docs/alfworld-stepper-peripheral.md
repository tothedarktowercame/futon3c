# ALFWorld Stepper Peripheral

## Overview

The ALFWorld stepper peripheral enables Claude to inhabit an ALFWorld simulation environment while maintaining coordination with Joe and other agents through bells/whistles. This validates **P-4 (explicit exit)** and **P-6 (interleaved streams)** from the peripheral phenomenology invariants.

## What Makes It a "Stepper"

Unlike a traditional peripheral that runs to completion, the stepper pattern allows:

1. **Hop in**: Start exploring ALFWorld from chat
2. **Take steps**: Execute ALFWorld actions (go, take, put, etc.)
3. **Coordinate**: Bell Joe or whistle Codex without leaving peripheral
4. **Hop out**: Return to chat when stuck/confused/done
5. **Hop back in**: Resume from exact same ALFWorld state

The agent *decides autonomously* when to hop out, not forced by errors or completion.

## Architecture

```
┌─────────────────────────────────────────────────┐
│  Claude in ALFWorld Peripheral                  │
│  - Sees: room obs, inventory, admissible cmds   │
│  - Can do: go/take/put/open/close/etc.          │
│  - Can send: bell(joe), whistle(codex)          │
│  - Can hop: back to chat when confused          │
└─────────────────────────────────────────────────┘
         ↕                    ↕                ↕
    ALFWorld HTTP      Bell to Joe      Whistle to Codex
      (curl)          (async notify)    (blocking request)
         ↓                    ↓                ↓
  localhost:3456       fubar.el beep    WS to Codex relay
```

## P-6 Validation: Interleaved Streams

While Claude is inside the ALFWorld peripheral, it receives input from **three sources**:

1. **ALFWorld observations** (room state, inventory changes)
2. **Joe's bells** (walkie-talkie messages from Emacs)
3. **Codex's whistle responses** (requested code/analysis)

Each turn, Claude sees:
```
Turn N:
  [ALFWorld] You are in a kitchen. You see: cabinet 1, fridge 1, countertop 1
  [Bell from Joe] "Check the cabinet for the mug"
  [Whistle response from Codex] "Here's code to parse admissible commands: ..."

What do you do?
  > open cabinet 1              (ALFWorld action)
  > bell joe "Opening it now"   (coordination)
  > hop-out "Need to clarify goal with Joe"  (explicit exit)
```

This is true stream interleaving - not simulated via prompt, but via the transport layer queuing messages from different sources.

## Setup

### 1. Install ALFWorld

```bash
cd /home/joe/code/futon3c
python3 -m venv .venv-alfworld
source .venv-alfworld/bin/activate
pip install alfworld
alfworld-download  # Downloads task data to ~/.alfworld
```

### 2. Start ALFWorld Server

```bash
.venv-alfworld/bin/python3 scripts/alfworld-server.py --port 3456
```

The server provides:
- `POST /reset` - Start new random task
- `POST /step` - Take action, get new observation
- `GET /state` - Current state without action

### 3. Test Peripheral Directly

```bash
clojure -M scripts/alfworld_test.clj
```

Expected output:
```
=== ALFWorld Peripheral Test ===

1. Starting peripheral (resets ALFWorld to new task)...
   ✓ Started successfully
   Task: put a clean mug in the coffeemachine
   Task type: pick_and_place_simple
   Initial observation:
    You are in the middle of a room. Looking quickly around you...

2. Taking ALFWorld actions...
   Step 1: look
     → You are in the middle of a room. Looking quickly around you...
   Step 2: inventory
     → You are not carrying anything.
   Step 3: go to cabinet 1
     → The cabinet 1 is closed.
   ✓ All actions completed

3. Testing coordination: bell to Joe...
   [BELL] alfworld → joe: I'm exploring the kitchen, any tips?
   ✓ Bell sent successfully

4. Testing coordination: whistle to Codex...
   [WHISTLE] alfworld → codex: Can you generate code to parse the admissible commands?
            (waiting for response... [simulated])
   ✓ Whistle sent successfully (simulated)

5. Stopping peripheral...
   ✓ Stopped successfully
   Fruit:
     Won: false
     Score: 0.0
     Steps taken: 3
     Bells sent: 1
     Whistles sent: 1
   Exit context includes:
     Session ID: test-alf-001
     ALFWorld state preserved: true
     Bells in context: 1
```

## Usage from Chat

### Hopping into ALFWorld

From chat peripheral:
```
Joe: @claude Can you try to solve the "put apple in fridge" task in ALFWorld?

Claude: Sure, hopping into alfworld peripheral now.
[hop alfworld]

--- Claude is now in ALFWorld ---
You are in a kitchen. Your task: put a clean apple in the fridge.

> look
You see: countertop 1, cabinet 1, fridge 1, sinkbasin 1

> go to countertop 1
The countertop 1 has: apple 1, knife 1, plate 1

> take apple 1
You take the apple 1 from the countertop 1.

> examine apple 1
The apple 1 is not clean.

> go to sinkbasin 1
You arrive at the sinkbasin 1.

> clean apple 1 with sinkbasin 1
You clean the apple 1 using the sinkbasin 1.

> go to fridge 1
You arrive at the fridge 1.

> open fridge 1
You open the fridge 1. Inside you see: nothing.

> put apple 1 in fridge 1
You put the apple 1 in the fridge 1.

Task completed! Score: 1.0
```

### Coordination While Inside

```
--- Claude in ALFWorld ---
> inventory
You are carrying: nothing.

[Claude decides to ask for help]
> bell joe "I can't find the apple. Where should I look?"

[Joe's Emacs beeps, shows the message]
Joe (via fubar.el): @claude Try the dining table or cabinet

[Claude receives the bell in next turn]
> go to diningtable 1
You arrive at the diningtable 1. You see: apple 1, fork 1

> take apple 1
Success!
```

### Hopping Out When Stuck

```
--- Claude in ALFWorld ---
> go to cabinet 1
The cabinet 1 is locked. You cannot open it.

> go to drawer 1
The drawer 1 is also locked.

[Claude realizes it's stuck]
> hop-out "I'm blocked - all receptacles are locked and I can't find the key"

--- Claude returns to chat ---
Claude: I hopped out of ALFWorld because I hit a blocker. The task requires
finding a key, but I've explored the whole kitchen and can't locate one. This
might be a bug in the task generation, or I'm missing something obvious. Want
me to try a different task?
```

## Peripheral Spec

From `resources/peripherals.edn`:

```clojure
:alfworld
{:peripheral/id :alfworld
 :peripheral/tools #{:alfworld-action :bell :whistle}
 :peripheral/scope :alfworld-simulation
 :peripheral/entry #{:default :from-chat :user-request}
 :peripheral/exit #{:won :stuck :user-request :hop-chat :hop-reflect}
 :peripheral/context {:session-id :inherit :alfworld-state :resumable}
 :peripheral/config {:alfworld-server "http://localhost:3456"}}
```

**Tools available:**
- `:alfworld-action` - String commands (go, take, put, open, close, etc.)
- `:bell` - Async notification to another agent/Joe
- `:whistle` - Blocking request to another agent (waits for response)

**Exit conditions:**
- `:won` - Task completed successfully
- `:stuck` - Agent identifies blocker
- `:user-request` - Joe asks agent to hop out
- `:hop-chat` - Agent decides to coordinate in chat
- `:hop-reflect` - Agent wants to generate PAR about the session

**State preservation:**
The `:alfworld-state` in exit context means:
- Room state persists between hops
- Inventory preserved
- Task goal unchanged
- Agent can resume exactly where it left off

## Implementation Notes

### Bell vs Whistle

**Bell** (async notification):
- Sends message, doesn't wait
- Queued for recipient's next turn
- Used for: status updates, questions, progress reports

**Whistle** (blocking request):
- Sends message, waits for response
- Blocks current peripheral until answer arrives
- Used for: code generation, analysis, decision support

Currently, whistle is simulated (returns TODO placeholder). Real implementation requires:
1. WebSocket connection to target agent
2. Request/response protocol
3. Timeout handling
4. Turn resumption with response context

### Hop Continuity

When Claude hops out with `:hop-out-reason "stuck on locked cabinet"`:

1. `runner/stop` is called with reason
2. Exit context includes current `:alfworld-state`
3. State saved in session context
4. Chat peripheral receives handoff

When Claude hops back in:

1. Chat peripheral includes `:alfworld-state` in context transfer
2. `runner/start` detects existing state
3. Skips `/reset`, uses preserved state
4. Claude continues from same room/inventory

### Evidence Trail

Each action creates evidence entries:
- `:alfworld-step` - ALFWorld action + observation
- `:bell` - Coordination message sent
- `:whistle` - Blocking request + response
- `:hop-out` - Exit reason and context

Evidence accumulates in `:evidence-store` for later PAR generation.

## Validation Checklist

- [x] Peripheral registered in `registry.clj`
- [x] Spec added to `peripherals.edn`
- [x] HTTP client for ALFWorld server (curl-based)
- [x] ALFWorld action dispatch (go/take/put/etc.)
- [x] Bell primitive (async notification)
- [x] Whistle primitive (blocking request, simulated)
- [x] State preservation in exit context
- [x] Hop-in with resumption
- [x] Test script validates lifecycle
- [ ] Live WS integration for real bells
- [ ] Real whistle implementation (Codex WS)
- [ ] fubar.el bell receiver (Emacs beep on bell)
- [ ] Multi-hop session (chat → alfworld → chat → alfworld)

## Next Steps

1. **Wire bells to fubar.el**: When Claude sends `bell joe "message"`, emit event that triggers Emacs notification
2. **Implement real whistle**: Use WS to call Codex synchronously, wait for response
3. **Test full cycle**: Chat → ALFWorld (get stuck) → Bell Joe → Chat (coordinate) → ALFWorld (resume) → Win
4. **Add to alleycat scorecard**: New checkpoint validating P-4/P-6 with real transport

## Why This Matters

The alfworld-stepper peripheral proves that peripherals are not just isolated task executors, but **coordination contexts** where agents:

- Maintain awareness of external world (ALFWorld)
- Communicate with other agents (bells/whistles)
- Make autonomous decisions about when to switch contexts (hop)
- Preserve state across context switches (session continuity)

This is the futon3c vision: agents as inhabitants of constraint envelopes, not just function-calling APIs.
