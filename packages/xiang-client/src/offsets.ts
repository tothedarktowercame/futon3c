/**
 * Codepoint offsets at the UTF-16 boundary.
 *
 * The seam states its offset unit rather than assuming it: a UTF-16 client
 * and a codepoint client disagree on any turn containing an emoji or CJK, and
 * the disagreement is silent. Everything here converts explicitly, and every
 * span check compares text, not just numbers.
 */

/** Length of `s` in Unicode codepoints. */
export function codepointLength(s: string): number {
  let n = 0;
  for (const _ of s) n++;
  return n;
}

/** UTF-16 index of codepoint offset `cp` in `s`; clamps at the end. */
export function cpToUtf16(s: string, cp: number): number {
  if (cp <= 0) return 0;
  let i = 0;
  let seen = 0;
  while (i < s.length && seen < cp) {
    const code = s.codePointAt(i)!;
    i += code > 0xffff ? 2 : 1;
    seen++;
  }
  return i;
}

/** Codepoint offset of UTF-16 index `idx` in `s`. */
export function utf16ToCp(s: string, idx: number): number {
  let i = 0;
  let cp = 0;
  const limit = Math.min(idx, s.length);
  while (i < limit) {
    const code = s.codePointAt(i)!;
    i += code > 0xffff ? 2 : 1;
    cp++;
  }
  return cp;
}

/** Substring of `s` between codepoint offsets, end-exclusive. */
export function cpSubstring(s: string, start: number, end: number): string {
  return s.slice(cpToUtf16(s, start), cpToUtf16(s, end));
}

export interface Utf16Span {
  start: number;
  end: number;
}

/** A codepoint span of `s` as a UTF-16 span, for DOM ranges and slicing. */
export function toUtf16Span(s: string, start: number, end: number): Utf16Span {
  return { start: cpToUtf16(s, start), end: cpToUtf16(s, end) };
}

/** True when `[start, end)` is a well-formed codepoint span of `s` whose text is `text`. */
export function exactSpan(s: string, start: number, end: number, text: string): boolean {
  if (!Number.isInteger(start) || !Number.isInteger(end)) return false;
  if (start < 0 || start >= end) return false;
  if (end > codepointLength(s)) return false;
  return cpSubstring(s, start, end) === text;
}

/** Whitespace-separated words, as Python's str.split() counts them. */
export function wordCount(s: string): number {
  return s.split(/\s+/).filter((w) => w.length > 0).length;
}
