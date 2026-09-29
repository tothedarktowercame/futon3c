#!/usr/bin/env python3
"""Standalone classical secret detector and redactor.

This standard-library-only file detects common private-key blocks, cloud and
service tokens, JWTs, bearer credentials, URL passwords, values assigned to
secret-like keywords, and long high-entropy tokens.  It deliberately ignores
Git/SHA hashes, UUIDs, paths, job identifiers, placeholders, and ordinary prose.
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


_RULES = (
    _Rule(
        "private-key",
        re.compile(
            r"-----BEGIN(?: [A-Z0-9]+)* PRIVATE KEY-----.*?"
            r"-----END(?: [A-Z0-9]+)* PRIVATE KEY-----",
            re.DOTALL,
        ),
    ),
    _Rule("aws-access-key", re.compile(r"(?<![A-Z0-9])(?:AKIA|ASIA)[A-Z0-9]{16}(?![A-Z0-9])")),
    _Rule("github-token", re.compile(r"(?<![A-Za-z0-9_])(?:gh[opusr]_[A-Za-z0-9]{20,255}|github_pat_[A-Za-z0-9_]{20,255})(?![A-Za-z0-9_])")),
    _Rule("anthropic-key", re.compile(r"(?<![A-Za-z0-9_-])sk-ant-[A-Za-z0-9_-]{20,}(?![A-Za-z0-9_-])")),
    _Rule("openai-key", re.compile(r"(?<![A-Za-z0-9_-])(?:sk-proj-[A-Za-z0-9_-]{20,}|sk-[A-Za-z0-9_-]{20,})(?![A-Za-z0-9_-])")),
    _Rule("slack-token", re.compile(r"(?<![A-Za-z0-9-])xox[abprs]-[A-Za-z0-9-]{10,}(?![A-Za-z0-9-])")),
    _Rule("google-api-key", re.compile(r"(?<![A-Za-z0-9_-])AIza[A-Za-z0-9_-]{30,}(?![A-Za-z0-9_-])")),
    _Rule("jwt", re.compile(r"(?<![A-Za-z0-9_-])eyJ[A-Za-z0-9_-]*\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+(?![A-Za-z0-9_-])")),
    _Rule("bearer", re.compile(r"(?i)(?:authorization\s*:\s*)?bearer\s+(?P<value>[A-Za-z0-9._~+/-]{8,})"), "value"),
    _Rule(
        "url-credentials",
        re.compile(r"[A-Za-z][A-Za-z0-9+.-]*://[^\s/@:]+:(?P<password>[^\s/@]+)@[^\s/]+"),
        "password",
    ),
)

_KEYWORD = re.compile(
    r"(?ix)"
    r"(?:"
    r"\"?(?:[A-Za-z0-9]+[_-])*"
    r"(?:password|passwd|pwd|secret|token|api[_-]?key|access[_-]?key|client[_-]?secret)\"?"
    r")\s*[:=]\s*"
    r"(?P<quote>[\"']?)"
    r"(?P<value>[^\s,;\}\]]+)"
)
_HIGH_ENTROPY = re.compile(r"(?<![A-Za-z0-9+/_=-])[A-Za-z0-9+/_=-]{32,}(?![A-Za-z0-9+/_=-])")
_PLACEHOLDERS = re.compile(
    r"(?i)^(?:<redacted>|\*{3,}|x{3,}|change(?:me)?|changeme\??|none|null|n/?a|placeholder|example)$"
)
_HEX = re.compile(r"^[0-9a-fA-F]+$")
_UUID = re.compile(r"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")


def _keyword_span(match: Match[str]) -> tuple[int, int] | None:
    value = match.group("value")
    quote = match.group("quote")
    if quote and value.endswith(quote):
        value = value[:-1]
    value = value.rstrip("\"'")
    if not value or _PLACEHOLDERS.fullmatch(value):
        return None
    start = match.start("value")
    return start, start + len(value)


def _entropy(value: str) -> float:
    length = len(value)
    return -sum((count / length) * math.log2(count / length)
                for count in (value.count(char) for char in set(value)))


def _high_entropy_candidate(text: str, match: Match[str]) -> bool:
    value = match.group(0)
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
    for match in _KEYWORD.finditer(text):
        span = _keyword_span(match)
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
