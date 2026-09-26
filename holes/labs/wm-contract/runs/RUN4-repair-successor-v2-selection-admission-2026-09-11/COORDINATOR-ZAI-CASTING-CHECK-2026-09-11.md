# Actual historical casting consumer check

Joe requests Zai agents for the next run. Read-only check used the current canonical finding and exact retained verification artifact through `repair/historical-verification-candidate`, then the real `full-loop-runner/historical-revalidation-entry`.

Result (assertions passed, process exit 0):

```edn
{:canonical-candidate true
 :original-casting-selected? true
 :zai-casting-selected? false
 :actors {:author "codex-10" :reviewer "codex-12"}
 :mutation :none}
```

Current selector source lines 1208–1210 requires admission author/reviewer to equal execution author/repair-reviewer and requires their separation. Therefore merely replacing packet casting with zai-2/zai-1 cannot select this historical revalidation. The existing artifact is retained with its true actors; no actor relabeling or admission occurred.

The independent packet/source review is accepted (5f3a06e3; coordinator current hashes f6a5dd89). Zai staffing job invoke-1789148820027-20225-df620e28 is active and must address this actual contract before next execution. A fresh Zai-backed verification path must satisfy the executed-review and qualification authority requirements; this check does not itself create those inputs. It also does not authorize falling through to ordinary repair when historical selection returns nil.

Only read-only candidate and selection functions were invoked. No runner opportunity, cohort start, store transition, queue tick, or live reload was invoked. The held initialization queue and all historical receipts remain unchanged.
