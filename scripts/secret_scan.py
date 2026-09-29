#!/usr/bin/env python3
"""Standalone classical secret detector and redactor.

This standard-library-only file detects common private-key blocks, cloud and
service tokens, JWTs, bearer credentials, URL passwords, values assigned to
secret-like keywords, and long high-entropy tokens.  It deliberately ignores
Git/SHA hashes, UUIDs, paths, job identifiers, placeholders, and ordinary prose.
It reads secrets as they appear in agent session logs as well as plain text:
JSON-escaped strings in .jsonl files (\\"password\\": ..., a token after \\n),
quoted passphrases with spaces, and Markdown labels such as **Password:**.
A bare password with no label or recognizable structure cannot be distinguished
reliably from an ordinary word and is therefore not detected.

The module is intended to be copied verbatim into other Python 3.10+ projects.
It never stores secret text in a Finding or emits it in diagnostic output.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
import math
from pathlib import Path
import re
import sys
from typing import Iterable, Match, Pattern


@dataclass(frozen=True, slots=True)
class Finding:
    """The type and character range of a suspected secret, never its value."""

    kind: str
    start: int
    end: int


@dataclass(frozen=True, slots=True)
class _Rule:
    kind: str
    pattern: Pattern[str]
    group: int | str = 0


def _start(chars: str) -> str:
    """Left boundary: not preceded by CHARS, or preceded by a \\n/\\t/\\r escape."""
    return rf"(?:(?<=\\[ntr])|(?<![{chars}]))"


_RULES = (
    _Rule(
        "private-key",
        re.compile(
            r"-----BEGIN(?: [A-Z0-9]+)* PRIVATE KEY-----.*?"
            r"-----END(?: [A-Z0-9]+)* PRIVATE KEY-----",
            re.DOTALL,
        ),
    ),
    _Rule("aws-access-key", re.compile(_start("A-Z0-9") + r"(?:AKIA|ASIA)[A-Z0-9]{16}(?![A-Z0-9])")),
    _Rule("github-token", re.compile(_start("A-Za-z0-9_") + r"(?:gh[opusr]_[A-Za-z0-9]{20,255}|github_pat_[A-Za-z0-9_]{20,255})(?![A-Za-z0-9_])")),
    _Rule("anthropic-key", re.compile(_start("A-Za-z0-9_-") + r"sk-ant-[A-Za-z0-9_-]{20,}(?![A-Za-z0-9_-])")),
    _Rule("openai-key", re.compile(_start("A-Za-z0-9_-") + r"(?:sk-proj-[A-Za-z0-9_-]{20,}|sk-[A-Za-z0-9_-]{20,})(?![A-Za-z0-9_-])")),
    _Rule("slack-token", re.compile(_start("A-Za-z0-9-") + r"xox[abprs]-[A-Za-z0-9-]{10,}(?![A-Za-z0-9-])")),
    _Rule("google-api-key", re.compile(_start("A-Za-z0-9_-") + r"AIza[A-Za-z0-9_-]{30,}(?![A-Za-z0-9_-])")),
    _Rule("jwt", re.compile(_start("A-Za-z0-9_-") + r"eyJ[A-Za-z0-9_-]*\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+(?![A-Za-z0-9_-])")),
    _Rule("bearer", re.compile(r"(?i)(?:authorization\s*:\s*)?bearer\s+(?P<value>(?=[A-Za-z._~+/-]*[0-9])[A-Za-z0-9._~+/-]{8,})"), "value"),
    _Rule(
        "url-credentials",
        re.compile(r"[A-Za-z][A-Za-z0-9+.-]*://[^\s/@:]+:(?P<password>[^\s/@]+)@[^\s/]+"),
        "password",
    ),
)

# The label may be JSON-quoted, JSON-escaped (\" inside a .jsonl string) or
# wrapped in Markdown emphasis (**Password:**).  The value itself is read by
# _keyword_span, so a quoted value runs to its closing quote, spaces included.
_KEYWORD = re.compile(
    r"(?ix)"
    r"(?P<em>[*_]{1,3})?(?:\\?[\"'`])?(?:[A-Za-z0-9]+[_-])*"
    r"(?P<keyword>password|passwd|pwd|secret|token|api[_-]?key|access[_-]?key|client[_-]?secret)"
    r"(?:\\?[\"'`])?(?(em)[*_]{0,3})\s*[:=](?(em)[*_]{0,3})\s*"
    r"(?P<quote>\\?[\"'`])?"
)
# Where a _KEYWORD match can begin: the keyword itself, reached back over the
# characters its optional prefix may contain.  Searching only there gives the
# same matches as _KEYWORD.finditer; running that regex from every position
# was quadratic in long alphanumeric runs (base64 payloads in agent logs).
_KEYWORD_WORD = re.compile(  # a lookahead, so overlapping keywords all count
    r"(?i)(?=password|passwd|pwd|secret|token|api[_-]?key|access[_-]?key|client[_-]?secret)")
_KEYWORD_PREFIX_CHAR = re.compile(r"[A-Za-z0-9_\-*\\\"'`]")
_UNQUOTED_VALUE = re.compile(r"[^\s,;\}\]\\\"'`]+")
_HIGH_ENTROPY = re.compile(_start("A-Za-z0-9+/_=-") + r"[A-Za-z0-9+/_=-]{32,}(?![A-Za-z0-9+/_=-])")
# Longer runs are encoded payloads (images, archives), not credentials; keys long
# enough to exceed this (PEM blocks, JWTs) have their own structural rules.
_HIGH_ENTROPY_MAX = 256
_PLACEHOLDERS = re.compile(
    r"(?i)^(?:<redacted>|\*{3,}|x{3,}|change(?:me)?|changeme\??|none|null|n/?a|placeholder|example)$"
)
_HEX = re.compile(r"^[0-9a-fA-F]+$")
_WORDLIKE = re.compile(r"[a-z]+[0-9]*|(?:[A-Z][a-z]{2,})+[0-9]*|[A-Z]{1,4}|[0-9]+|[0-9a-f]+")
_UUID = re.compile(r"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")


def _keyword_span(text: str, match: Match[str]) -> tuple[int, int] | None:
    start = match.end()
    quote = match.group("quote")
    # A quoted value runs to the same closing quote (escaped the same way), so
    # a passphrase with spaces is redacted whole.  Without a closing quote on
    # the same line, fall back to the unquoted reading.
    end = text.find(quote, start) if quote else -1
    if end < 0 or "\n" in text[start:end]:
        value_match = _UNQUOTED_VALUE.match(text, start)
        end = value_match.end() if value_match else start
    value = text[start:end]
    if not value.strip() or _PLACEHOLDERS.fullmatch(value.strip()):
        return None
    # A working directory (PWD=/home/…, OLDPWD=~/…) is a path, not a password.
    if value.startswith(("/", "~")):
        return None
    # An EDN/Clojure map key ({:token :foo/bar}) names a field, not a secret.
    if match.start("keyword") and text[match.start("keyword") - 1] == ":":
        return None
    # "token" is also an ordinary word (max_token=4096, "per token: n"), so a
    # token value must look like one: a digit that is not the whole value, or length.
    if match.group("keyword").lower() == "token" and not (
            len(value) >= 16 or (re.search(r"[0-9]", value) and not value.isdigit())):
        return None
    return start, end


def _keyword_matches(text: str) -> Iterable[Match[str]]:
    """The matches _KEYWORD.finditer(text) would return, found by trying only
    the start positions that can reach a keyword."""
    resume = 0
    for word in _KEYWORD_WORD.finditer(text):
        if word.start() < resume:
            continue
        start = word.start()
        while start > resume and _KEYWORD_PREFIX_CHAR.match(text, start - 1):
            start -= 1
        for position in range(start, word.start() + 1):
            match = _KEYWORD.match(text, position)
            if match:
                yield match
                resume = match.end()
                break


def _entropy(value: str) -> float:
    length = len(value)
    return -sum((count / length) * math.log2(count / length)
                for count in (value.count(char) for char in set(value)))


def _high_entropy_candidate(text: str, match: Match[str]) -> bool:
    value = match.group(0)
    if len(value) > _HIGH_ENTROPY_MAX:
        return False
    # Paths and hyphenated identifiers (claude/projects/-home-joe-code/6799e67a-…,
    # M-futon-seams) join ordinary words; a random token split the same way
    # gives mixed-case fragments.  Skip when most segments read as words.
    segments = [part for part in re.split(r"[/_\-.=+]", value) if part]
    wordlike = [part for part in segments if _WORDLIKE.fullmatch(part)]
    if len(segments) >= 3 and len(wordlike) >= 0.6 * len(segments):
        return False
    if _HEX.fullmatch(value) and (7 <= len(value) <= 40 or len(value) == 64):
        return False
    if _UUID.fullmatch(value):
        return False
    if not (re.search(r"[A-Z]", value) and re.search(r"[a-z]", value)
            and re.search(r"[0-9]", value)):
        return False
    before = text[max(0, match.start() - 16):match.start()].lower()
    if before.endswith("data:") or ";base64," in before:
        return False
    if match.start() and text[match.start() - 1] in "/\\":
        return False
    return _entropy(value) >= 4.0


def _merge(findings: Iterable[Finding]) -> list[Finding]:
    ordered = sorted(findings, key=lambda item: (item.start, item.end, item.kind))
    merged: list[Finding] = []
    for item in ordered:
        if not merged or item.start >= merged[-1].end:
            merged.append(item)
            continue
        previous = merged[-1]
        # Structural signatures take precedence over the generic entropy net.
        # Otherwise prefer the match covering more text.
        if previous.kind == "high-entropy" and item.kind != "high-entropy":
            kind = item.kind
        elif item.kind == "high-entropy" and previous.kind != "high-entropy":
            kind = previous.kind
        else:
            kind = (item.kind if item.end - item.start > previous.end - previous.start
                    else previous.kind)
        merged[-1] = Finding(kind, previous.start, max(previous.end, item.end))
    return merged


def scan(text: str) -> list[Finding]:
    """Return non-overlapping suspected-secret ranges without secret values."""

    found: list[Finding] = []
    for rule in _RULES:
        for match in rule.pattern.finditer(text):
            start, end = match.span(rule.group)
            found.append(Finding(rule.kind, start, end))
    for match in _keyword_matches(text):
        span = _keyword_span(text, match)
        if span:
            found.append(Finding("keyword-assignment", *span))
    for match in _HIGH_ENTROPY.finditer(text):
        if _high_entropy_candidate(text, match):
            found.append(Finding("high-entropy", match.start(), match.end()))
    return _merge(found)


def redact(text: str) -> tuple[str, list[Finding]]:
    """Replace suspected secrets and return the redacted text and findings."""

    findings = scan(text)
    pieces: list[str] = []
    cursor = 0
    for finding in findings:
        pieces.extend((text[cursor:finding.start], f"[REDACTED:{finding.kind}]"))
        cursor = finding.end
    pieces.append(text[cursor:])
    return "".join(pieces), findings


def _read_inputs(files: list[str]) -> str:
    if not files:
        return sys.stdin.read()
    return "".join(Path(name).read_text(encoding="utf-8") for name in files)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument("--check", action="store_true", help="report only count and kinds to stderr")
    modes.add_argument("--json", action="store_true", help="write finding metadata as JSON")
    parser.add_argument("files", nargs="*")
    try:
        args = parser.parse_args(argv)
        text = _read_inputs(args.files)
    except (OSError, UnicodeError) as error:
        parser.error(str(error))
    redacted, findings = redact(text)
    if args.check:
        kinds = sorted({finding.kind for finding in findings})
        print(f"{len(findings)} finding(s): {','.join(kinds)}", file=sys.stderr)
    elif args.json:
        print(json.dumps([{"kind": item.kind, "start": item.start, "end": item.end}
                          for item in findings], sort_keys=True))
    else:
        sys.stdout.write(redacted)
    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
