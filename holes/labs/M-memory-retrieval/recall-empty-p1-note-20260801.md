# P1 — recall-empty mechanism and bias

Preregistered in `E-memory-whitepaper-v2-programme.md` §5 before this analysis.
The analysis used 129 offered halves from
`receipts-export-20260731-all-authors.edn` (SHA-256
`0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`).

## Result

| hypothesis | verdict | result |
|---|---|---|
| H1: empty recall rises with rarity of the rarest recorded term | **FALSIFIED** | Among the 126 receipts with recorded terms, empty rates were 71.4% in the rare bucket (min DF ≤ 11), 44.4% in the middle bucket (12–56), and 74.4% in the common bucket (> 56). Rare minus common was −2.9 percentage points; one-sided permutation p = 0.618. The preregistered monotone relationship is absent. Three receipts lacked `:recall-query`, contrary to the preregistration's field census, and could not enter H1. |
| H2: singles surface more but are used less than triples | **UNTESTABLE** | Zero of 129 receipts records the fired rung. The implementation attaches `:recall/tier` and `:recall/query-used` to `proposals` (`dispatch_with_recall.clj:607–610`) but returns and persists the unaugmented `query-data` (`:702–705`, `:925–934`). `:recall-query` and `:term-sources` describe the candidate query, not the rung that succeeded. Replaying today's index would invent historical rung labels, so no proxy was substituted. |
| H3: empty recall is non-uniform across problem family | **CONFIRMED** | 109 receipts matched 73 `aNNXNN` families; 20 non-family rows were excluded by the preregistered family definition. The weighted between-family variance statistic was 21.278. In 10,000 fixed-seed label permutations, p = 0.000400 (< 0.05). |

**H2 caption required by the preregistration:** any use rate computed from
`used-ids` would be a **FLOOR**, because outcome `used-ids` is populated only
about 16% of the time. No H2 use rate is reported because the comparison rung
is not recorded.

**H1/index caption:** document frequencies came from a private copy made on
2026-08-01 of `migration-store-21/fts5-evidence.db`; the live database was
never opened with SQLite. The copied index contained 121,589 rows. These are
current-index DFs, while the dispatches are historical, and the service reports
`index-as-of` 2026-07-31T04:44:43Z. They are not dispatch-time DF snapshots.

## Method

- H1 used tertiles of observed minimum DF. Ties stayed in the lower-DF bucket,
  yielding n = 42 / 45 / 39. Its preregistered expectation was called confirmed
  only if `rare > middle > common`; the permutation statistic was rare minus
  common.
- H3 extracted the case-insensitive `aNNXNN` prefix and used
  `Σ n_family (rate_family − overall_rate)²`. Labels were shuffled 10,000 times.
- Independent deterministic RNG streams used seed 20260731. Runtime was Python
  3.12.3, `edn_format` 0.7.5, and SQLite 3.45.1.
- Two full reruns were byte-identical. Result SHA-256:
  `6379a2db129f786576cad809be784f887045f64717cda3838dc514f17c9fef78`.

The machine-readable result is
`recall-empty-p1-results-20260801.json`; the analysis and its unit tests are
`recall_empty_p1_20260801.py` and `test_recall_empty_p1_20260801.py`.

## Frozen-input check and defects left unfixed

The analysis made no changes to the frozen inputs. Their post-run SHA-256s are:

- `receipts-export-20260728.edn`:
  `7bc57433eec7b42452ec1ab63bc34b713c9ff20328381ae251750b7367f590e4`
- `receipts-export-20260731-all-authors.edn`:
  `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`
- `damage-state-results-20260730.edn`:
  `554da6b6800b58e6767645c157762986d9ea3adc1a3f27e91cef92027f329e46`
- `psi-v2-replay-results-20260728.edn`:
  `2b9f6e283690f8d21ee6c30c626d51911dc44c10914cb277261e87e173d0b2bd`

As required by staging §A, this study did not repair statement-order term
selection, sparse `used-ids`, the missing lane-scoped rarity ranking, or the
newly demonstrated omission of fired-rung fields from persisted receipts.
