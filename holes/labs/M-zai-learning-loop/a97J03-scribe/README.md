# a97J03 scribe and promotion pass

Source jobs: `invoke-1785931097145-40-484817cb`,
`invoke-1785931218106-42-c36f29f8`, `invoke-1785931526692-44-a2147f71`.
Compiled commit: `1f93f8652da95ff5b41f407b30dc1743d0ab1a2b`.

| Draft | Confidence | Memory ID |
| --- | --- | --- |
| Lp translation through pairing | `n=2` documented reuse | `e-a97j03-lp-translation-continuity-through-pairing` |
| density/pairing cocompact vanishing | `n=2` documented reuse | `e-a97j03-compact-support-density-pairing-cocompact` |
| cocompact to both ends | `n=1` compiled | `e-a97j03-cocompact-limit-to-atTop-atBot` |

## Near-duplicate check

The broad strategy duplicates
`e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density`,
so it was not redrafted. `e-j02-steinhaus-positive-convolution-open-locus`
concerns positive-locus continuity and sumsets, not Lp/Lq pairing. The three
drafts isolate narrower compiled APIs/adapters not found in the store scan.

## Hunger audit

No result was degraded-under-load.

| Literal query/pull | Result | Grounded | Disposition |
| --- | --- | --- | --- |
| `convolution Holder Lp translation` | empty | yes | translation/pairing memory; literal tags |
| `translation continuity Lp symmetric-difference` | empty | yes | translation/pairing memory; literal tags |
| `Holder convolution Mathlib API integral product Lp Lq conjugate exponent bounded bilinear` | noise | yes | translation/pairing memory; literal tags |
| `convolution Lp Lq Holder inequality continuous vanishes infinity translation` | noise | yes | density/pairing memory; literal tags |
| subject `e-codexpilot-prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density` (twice) | empty | yes, by solved-neighbor consultation | density and adapter memories; literal identifier tagged |

The Cc-density query returned `math-informal/monotone-approximation`, and the
plain `convolution` tag query returned the Steinhaus memory; both were relevant
mixed/successful pulls rather than hunger.

## Consultations

1. Mathlib `Analysis/Convolution.lean`: general Lp/Lq theorem is a TODO;
   discarded as a direct theorem source.
2. Solved `a03J03`: exact compiled continuity and cocompact proof found and reused.
3. `Function/Holder.lean`: `lpPairing` APIs found and used.
4. `ConjExponents.lean`: `Real.holderConjugate_iff` found and used.
5. Order compactness: `atTop_le_cocompact`/`atBot_le_cocompact` found and used.
6. Existing broad convolution strategy memory: duplicate; cited, not promoted again.

## Proposed attachments

| Memory | Pattern | Reason |
| --- | --- | --- |
| translation/pairing | `math/measure-integration-api` | Packages the measure-preserving Lp translation and Hölder pairing bridge. |
| density/cocompact | `math/measure-integration-api` | Packages compact-support density and uniform bilinear error control. |
| cocompact adapter | `math/measure-integration-api` | Connects C₀-style convergence to the two one-sided real filters. |

All remain `:proposed`; reviewer calls are in `APPROVALS.md`.
