"""Regression test: a cascade dispatch refuses at the sender, not at the receiver.

library/象/言即行 puts the illocutionary type on the envelope and the content in
the cascade, and requires that a receiver who cannot act on what arrived refuse
with a typed reason rather than guess. --cascade applies that rule one step
earlier: a cascade whose ids do not resolve, whose parens do not balance, or
whose holes are untyped never leaves the sender.

The failure this guards against is the one that cost a dispatch on 2026-09-23:
a bell that arrived looking fine, was acted on as far as it could be, and
reported done.

Run: python3 test/agency_send_cascade_test.py
"""
import json, os, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SEND = os.path.join(HERE, os.pardir, "scripts", "agency_send.py")

GOOD = """(cascade demo
  (join reserve-and-hand-over
    (cascade-construction/hand-over-when-acting-is-worth-more :to zai-2)
    (translation/route-the-untranslatable :on-refusal typed))
  (HOLE-1 :wanted "a pattern for queueing behind another agent's work"
          :in-hand "one recorded instance"
          :discharge "a second independent occurrence"))
"""

BAD_ID = "(cascade x (cascade-construction/no-such-pattern))\n"
BAD_PARENS = "(cascade x (translation/route-the-untranslatable)\n"
BAD_HOLE = '(cascade x (HOLE-1 :wanted "something unnamed"))\n'


def run(cascade, stdin=""):
    with tempfile.NamedTemporaryFile("w", suffix=".cascade", delete=False) as fh:
        fh.write(cascade)
        path = fh.name
    try:
        return subprocess.run(
            [sys.executable, SEND, "--to", "zai-2", "--from", "claude-1",
             "--kind", "bell", "--type", "request", "--cascade", path, "--dry-run"],
            input=stdin, capture_output=True, text=True)
    finally:
        os.unlink(path)


def main():
    bad = 0

    ok = run(GOOD)
    if ok.returncode != 0:
        bad += 1; print("FAIL  a linting cascade was refused:\n" + ok.stderr)
    # --dry-run prints the payload as JSON, so decode it rather than matching
    # escaped text: json.dumps writes 言即行 as \u8a00\u5373\u884c.
    payload = json.loads(next(l for l in ok.stdout.splitlines()
                              if l.startswith("{")))
    # the cascade rides in the payload, not only inside the rendered prompt --
    # a job whose cascade exists only as prose cannot be re-linted later
    if "cascade" not in payload:
        bad += 1; print("FAIL  payload carries no cascade field")
    if "言即行" not in payload.get("prompt", ""):
        bad += 1; print("FAIL  the receiver is not told how to read the body")

    for name, text, expect in (("unresolved id", BAD_ID, "unresolved id"),
                               ("unbalanced parens", BAD_PARENS, "unbalanced parens"),
                               ("untyped hole", BAD_HOLE, "untyped hole")):
        r = run(text)
        if r.returncode == 0:
            bad += 1; print(f"FAIL  {name} was sent instead of refused")
        elif expect not in r.stderr:
            bad += 1; print(f"FAIL  {name} refused without naming it:\n{r.stderr}")

    # prose on stdin is optional when a cascade is supplied, and required when not
    r = run(GOOD, stdin="")
    if r.returncode != 0:
        bad += 1; print("FAIL  empty stdin rejected alongside --cascade")

    print("cascade dispatch: " + ("FAILED" if bad else "all checks pass"))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
