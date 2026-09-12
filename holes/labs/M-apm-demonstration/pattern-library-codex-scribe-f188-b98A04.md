# Codex Scribe pattern library — f188

The reviewed mathematics-memory search found the earlier cover-extension
pattern, but no pattern for retaining factor identities while constructing a
composition series across a short exact sequence.

## math-formalization/build-composition-series-across-short-exact-sequences

- **Trigger:** A finite-length middle module occurs in a short exact sequence,
  and a later proof must know where a particular factor of either endpoint
  appears in a composition series of the middle module; a theorem about length
  alone loses too much information.
- **Move:** Map a composition series of the kernel-side module into the middle
  module, comap a composition series of the quotient-side module into the
  middle module, and join the two `RelSeries` values with `smash`.  Exactness
  identifies the last term of the mapped series with the first term of the
  comapped series.  Use the `smash_castAdd` and `smash_natAdd` index lemmas to
  recover factors from the left and right blocks, respectively.
- **Why it works:** Injective maps preserve covers under `Submodule.map`,
  surjective maps preserve covers under `Submodule.comap`, and exactness gives
  precisely the equality needed at the splice.  The explicit two-block index
  decomposition retains factor-level linear equivalences for later
  Jordan--Hölder transport.
