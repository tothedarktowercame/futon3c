# E-repl-lost-dictated-turn: a dictated turn that never reached claude-17

**VERDICT (2026-10-09, provisional):** ACTIVE — Live bug investigation with a new dated finding added 2026-09-27 and the diagnosis still narrowing on the REPL delivery path. _(WM status classification by zai-2, medium confidence; not yet confirmed by the author.)_

Opened 2026-09-27 by claude-17 at Joe's request. Minor; outside M-象-2000.

**Symptom.** During the voice session on 2026-09-27 (~18:45-18:50Z), Joe dictated a turn
to `*claude-repl:claude-17*` (about the speech bar reading "listening" while the phone
spoke) that never arrived as a turn; claude-17 first saw its text when Joe re-sent it
by typing. Joe also had to submit one turn twice. He may be able to add buffer line
numbers.

**Suspected cause, not established.** Two park resumes were being delivered into the
same buffer around then (codex-4's P6o-2 origin probes, released 18:49:21Z and
18:52:03Z). A resume and a dictated submit landing together could drop one or merge
it into the other ("woven in with a continuation", Joe).

**Where to look.** The voxterm server log (what was transcribed and handed to
`voxterm-insert`, and when); the evidence store's chat-turn records for claude-17's
session 564c8e50… between 18:40 and 18:55Z (origin now distinguishes operator from
harness); agent-repl-park.el's resume insertion versus claude-repl's input marker
(see memory note "REPL Argument list too long = marker drift").

**Second case (same day).** The park wake for codex-4's job
invoke-1790535920570-25608-872383eb was released at 19:17:30Z ("park
park-ec690381-c891-4e76-9242-3b96321d81fc will wake claude-17") and never arrived as a
turn in `*claude-repl:claude-17*`; the result was found only by polling the job. At
that time the store was slow (reads >5 s at 19:17:44-45Z) and one claude-17 chat-turn
append was rejected. So the lost deliveries are not only dictated turns: a park resume
went missing too, which points at the REPL's delivery path, not at voxterm.
