---
name: kinesis-vdrive-editing
description: "How to edit the Kinesis Advantage2 layout files from the server, over the phone tunnel, without touching My Files"
metadata:
  type: project
---

**FIRST, EVERY TIME: before asking Joe to press Progm+F1, remind him to turn on Power User Mode with Progm+Shift+Esc** (hold Progm, then Shift, tap Esc; 4 LED flashes). Joe asked for this reminder on 2026-09-24. Then follow the checklist at the top of README-kinesis.

CANONICAL DOC: `~/code/futon0/README-kinesis.md`, with the live layout backed up at
`~/code/futon0/kinesis/dvorak.txt`. Read that first — this memory is the index to it.

Joe's Kinesis Advantage2 (firmware 1.0.431.us, 4MB) is plugged into the Samsung
phone. `Progm`+`F1` mounts its v-Drive as a USB volume; the chord is handled by the
keyboard's own processor, so it still works while HID output is disabled — you can
always toggle back out.

The keyboard sends **QWERTY** keycodes and the phone converts to Dvorak. So layout
files are written entirely in QWERTY terms, in *both* directions: `[kp-s]` is the key
that types `o`, and `[hyphen]` is what emits `[`. Getting this backwards is the
recurring bug — it cost an afternoon hunting a missing `?`, which turned out to live
on the QWERTY `[` key. Dvorak's `[ ] \` sit on QWERTY `- = \`.

Termux cannot reach the v-Drive by path (`/storage`, `/mnt/media_rw` are both denied,
no USB volume in `/proc/mounts`). It reaches it via SAF, and the `termux-saf-*`
tools are installed and working. Volume UUID is `2E35-0346`; the granted tree is the
`active/` directory:

    content://com.android.externalstorage.documents/tree/2E35-0346%3Aactive/document/2E35-0346%3Aactive

Grant it once with `termux-saf-managedir` (needs Termux foregrounded and a tap or two
on the picker — touch still works while the v-Drive is mounted). After that
`termux-saf-ls` / `-read` / `-write` are non-interactive and scriptable, so the whole
edit can be driven from the server over the reverse tunnel in
[[server-remote-access-setup]]. `termux-saf-write URI` takes stdin and truncates.

`active/state.txt` has `startup_file=qwerty.txt` — that's the live layout.
`dvorak.txt` is 0 bytes, i.e. the keyboard's onboard Dvorak is completely stock, so
`Progm`+`F4` remains a clean fallback if Joe ever moves Dvorak into the keyboard.

SAF grants do NOT survive ejecting the v-Drive — re-run `termux-saf-managedir` each
time. Fire the picker *before* Joe mounts, since he can't type once the v-Drive is up;
the picker notices the volume when it appears.

Authoritative token reference: Advantage2 User's Manual, Appendix 13.2, saved locally
at `/home/joe/adv2-manual.pdf` (from
https://kinesis-ergo.com/wp-content/uploads/Adv2-Users-Manual-US_6-22-21.pdf).
The `SmartSet App Help.pdf` on the v-Drive itself is only the Windows app manual — no
token table. Hard-won specifics:

- Punctuation action tokens are mostly the bare character: `=`, `\`, `/`, `;`, `` ` ``.
  Only a few are words — `HYPHEN`, `OBRACK`, `CBRACK`. There is no `[equal]`,
  `[equals]` or `[bslash]`; those silently do nothing.
- HID decimal codes work anywhere a token does (`[enter]>[88]`), which sidesteps
  token-name guessing entirely.
- Tap-and-hold: `[lshift]>[esc][t&h250][lshift]` — tap sends the first action, hold
  past N ms sends the second. Range 1–999ms.
- **Layer-shift tokens must be mapped to BOTH layers.** `[delete]>[...][kpshift]`
  alone makes the layer latch on with no way back; you also need `[kp-delete]>...`.
  This was the cause of the "sticky Delete".
- Invalid syntax falls back to the key's default action, so bad lines fail quietly
  rather than breaking the file. An `*` at line start comments a line out.
- Changes apply on v-Drive close (eject in Android first, then `Progm`+`F1`), or by
  re-selecting the layout with `Progm`+`F3`.

ARCHITECTURE CHANGED 2026-08-28 (evening). Dvorak now lives IN THE KEYBOARD
(`Progm`+`F4`, `active/dvorak.txt`, and `state.txt` has `startup_file=dvorak.txt`).
The phone's physical-keyboard layout is set to plain **English (US)** and does no
translation. Leave it that way — and specifically NOT the "international style"
variant, whose dead keys turn `'` into a composing accent (`'`+`w` -> w-acute), which
breaks apostrophe in Emacs and backtick in the shell.

The rule that explains every failure today: **exactly one of keyboard or phone should
do Dvorak — never both, never neither.** Both at once yields double-Dvorak; the
letters look like line noise but decode cleanly if you apply the map twice.

Why it changed: mounting the v-Drive re-enumerates the keyboard over USB, and Android
drops the per-device layout choice back to Default. Under the old scheme (keyboard
QWERTY + phone doing Dvorak) that silently broke every mapping after each mount. With
Dvorak onboard, re-enumeration is harmless. It also kills the double-translation trap
that ate an afternoon: in `dvorak.txt` the location tokens are native Dvorak, so a key
is named by the letter it actually types.

Location tokens are confirmed from Appendix 13.1 Figs 32/33 (diagrams, not text —
render with `pdftoppm -r 200 -f 37 -l 37 -png adv2-manual.pdf`). Dvorak keypad layer,
left keywell:

    [kp-']  [kp-,]  [kp-.]  [kp-P]  [kp-Y]
    [kp-A]  [kp-O]  [kp-E]  [kp-U]  [kp-I]
    [kp-;]  [kp-Q]  [kp-J]  [kp-K]  [kp-X]

Current layout: see README-kinesis §6 and `kinesis/dvorak.txt` (updated 2026-09-24: right-hand arrows on [kp4]-[kp9]; the right hand's keypad-layer tokens are numeric-pad names, not letters). AFTER ANY EDIT: Progm+F4 to force reload, then Progm+Esc and check `Keys remapped>`/`Stored macros>` equal the file's line counts — eject+Progm+F1 left the old layout running on 2026-09-24. Snapshot below is as of end of 2026-08-28. Both outer thumb keys are pure
momentary keypad shifts (no tap action, so neither can delete by accident); the inner
ones keep Backspace and Space. Joe also rearranged some top-layer keys onboard.

    [delete]>[kpshift]  [kp-delete]>[kpshift]   [enter]>[kpshift]  [kp-enter]>[kpshift]
    [rctrl]>[enter]     [hyphen]>[/]            [\]>[hyphen]
    [up]>[lalt]  [down]>[lctrl]  [right]>[lalt]  [left]>[lctrl]
    [kp-a]>[obrack] [   [kp-i]>[cbrack] ]   [kp-']>[\] \   [kp-y]>[`]
    [kp-o]>[left]  [kp-e]>[down]  [kp-u]>[right]  [kp-.]>[up]
    [kp-,]>[bspace]  [kp-p]>[delete]
    {kp-/}>{-lshift}{`}{+lshift}        tilde
    {kp-hyphen}>{-lshift}{`}{+lshift}   tilde (the key Joe actually uses: / ? ~)
    {kp-\}>{-lctrl}{c}{+lctrl}{m}       em dash, via Emacs
    {kp\}>{-lctrl}{c}{+lctrl}{m}        same, alternate spelling in case kp-\ is wrong

The general pattern for adding a third-layer character, in increasing difficulty:
1. **Unshifted character** — plain remap, `[kp-x]>[token]`. Just works.
2. **Shifted character** (`~`, `?`, capitals) — impossible by remap; HID keycodes carry
   no shift state. Needs a macro: `{kp-x}>{-lshift}{key}{+lshift}`.
3. **Character with no HID keycode at all** (em dash, anything non-ASCII) — the keyboard
   fundamentally cannot emit it. Have the macro send an unused chord and let the host
   insert the character. Em dash does this: keyboard sends `C-c m`, and
   `my/insert-em-dash` in `~/.emacs-graph/init.el` inserts U+2014. Note that is the
   *live* config — `~/.emacs` is a chemacs bootstrap and Joe runs
   `emacs -nw --with-profile graph`.

**Onboard programming destroys macros.** `Progm`+`F12` makes the keyboard rewrite the
whole layout file in its own order, and it truncated a macro line mid-way rather than
round-tripping it (file ended at a bare `{kp-`). So after any onboard remap, re-read
the file and restore the macro lines. Prefer doing remaps through the file.

Also: always read the drive file before writing. The keyboard's rewrite reorders
everything and adds Joe's onboard changes, so a blind overwrite clobbers his work.

Gotcha when driving this remotely: `termux-saf-write` returns success even when the
v-Drive has been ejected, writing into nothing. Always read back and treat an empty
result as failure — one write was silently lost this way.

2026-09-24: `Progm`+`F1` silently does nothing unless Power User Mode is on (`Progm`+`Shift`+`Esc`, 4 LED flashes). A hard reset clears it. The status report shows a "Power User Mode" line only while PUM is on. Before firing the picker, check which phone has the keyboard with `termux-usb -l` over each tunnel: it was on phone1, and phone2 listed `[]`. Right-hand keypad-layer tokens are numeric (`g c r` = `[kp7] [kp8] [kp9]`, `h t n` = `[kp4] [kp5] [kp6]`); see README §6.
