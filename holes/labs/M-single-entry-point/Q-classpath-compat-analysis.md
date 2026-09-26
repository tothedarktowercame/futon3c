# Q-classpath-compat Analysis

**Mission:** M-single-entry-point  
**Question:** Does adding `thheller/shadow-cljs` as a futon3c dep introduce conflicts?  
**Flight:** B6-F4 (lab artifact, not production code)

## Answer

**No conflicts.** `thheller/shadow-cljs` is already on the futon3c classpath
(in `deps.edn` `:dev` alias, version `2.28.20`), the JVM is running with it
loaded (the live system carries this bell exchange on :7070 with shadow-cljs
available), and the version is pinned to match the per-app `package.json` files.
The classpath surface that could conflict is small, version-pinned, and
deliberately managed.

## Evidence from real configs

### 1. futon3c/deps.edn — the host classpath

The `:dev` alias already declares shadow-cljs and its CLJS compile-time deps:

```clojure
;; deps.edn :dev alias
:dev {:extra-paths ["dev"]
      :extra-deps {futon5/futon5           {:local/root "../futon5"}
                   webarxana/webarxana     {:local/root "../futon4/dev/web/webarxana"}
                   war-machine/war-machine {:local/root "../futon2/web/war-machine"}
                   thheller/shadow-cljs    {:mvn/version "2.28.20"}   ;; ← already here
                   reagent/reagent         {:mvn/version "1.2.0"}     ;; ← CLJS compile-time
                   cljs-http/cljs-http     {:mvn/version "0.1.46"}    ;; ← CLJS compile-time
                   datascript/datascript   {:mvn/version "1.7.3"}}    ;; ← CLJS compile-time
      :main-opts ["-m" "futon3c.dev"]}
```

The comment in deps.edn explains the design: the embedded shadow-cljs server
uses the host JVM classpath, so CLJS compile-time deps that standalone
shadow-cljs reads from its own `:dependencies` must be mirrored here.

### 2. futon3c/shadow-cljs.edn — the embedded multi-build config

```clojure
{:source-paths ["../futon2/web/war-machine/src"
                "../futon4/dev/web/webarxana/src"]
 :dependencies [[reagent/reagent       "1.2.0"]
                [cljs-http/cljs-http   "0.1.46"]
                [datascript/datascript "1.7.3"]]
 :compiler-options {:warnings {:redef false}}   ;; no.en.core redef — known, suppressed
 ...}
```

The `:dependencies` here mirror the deps.edn `:dev` alias versions exactly.
The `:compiler-options {:warnings {:redef false}}` suppresses a known
`no.en.core` vs `cljs.core` redefinition warning (parse-long/parse-double) —
this is a warning-level annoyance, not a conflict.

### 3. Version alignment across configs

| Dependency | deps.edn `:dev` | shadow-cljs.edn `:dependencies` | war-machine package.json | webarxana package.json |
|---|---|---|---|---|
| thheller/shadow-cljs | 2.28.20 | (host provides) | 2.28.20 | 2.28.20 |
| reagent | 1.2.0 | 1.2.0 | (npm: react) | (npm: react) |
| cljs-http | 0.1.46 | 0.1.46 | — | — |
| datascript | 1.7.3 | 1.7.3 | — | — |

All versions are pinned and aligned. No version skew.

### 4. Transitive deps from war-machine and webarxana local/roots

The `:dev` alias loads `war-machine` and `webarxana` as `:local/root` deps.
Both bring their own deps.edn with JVM-side dependencies:

- **war-machine/deps.edn**: `http-kit 2.8.0`, `ring-core 1.12.2`, `cheshire 5.13.0`
- **webarxana/deps.edn**: `http-kit 2.8.0`, `ring-core 1.12.2`, `buddy-hashers`, `buddy-sign`
- **futon3c/deps.edn** (host): `http-kit 2.8.1`, `ring-core 1.11.0`, `cheshire 5.11.0`

Minor version skew exists on `http-kit` (2.8.0 vs 2.8.1) and `ring-core`
(1.11.0 vs 1.12.2) and `cheshire` (5.11.0 vs 5.13.0). tools.deps resolves
these to the highest version (2.8.1, 1.12.2, 5.13.0). These are JVM-side
deps, not CLJS-side, and the skew is independent of shadow-cljs — it
exists whether or not shadow-cljs is on the classpath. shadow-cljs itself
has no JVM-side dependency on http-kit, ring, or cheshire.

### 5. The dev-laptop-env script's role

The script (`scripts/dev-laptop-env`) sets `FUTON3C_SHADOW_AUTOSTART=false`
by default — the in-JVM shadow watches are gated OFF. The operative path
is Path B (sidecar shadow watches via `start_shadow_watch_sidecar()` with
PID files), not Path A (in-JVM shadow via `start-shadow!`). The classpath
compat question is therefore about *capability* (can shadow-cljs coexist
on the classpath?) not *practice* (is it running embedded?). The answer
to both is yes — the embedded path works when enabled (the deps and
shadow-cljs.edn are configured for it), and the sidecar path works as
the default.

## Conclusion

Q-classpath-compat is **resolved: green**. `thheller/shadow-cljs 2.28.20`
on the futon3c `:dev` classpath introduces no conflicts:

1. CLJS compile-time deps (reagent, cljs-http, datascript) are version-pinned
   and mirrored between `deps.edn` and `shadow-cljs.edn`.
2. The JVM-side version skew (http-kit, ring-core, cheshire) is pre-existing
   and independent of shadow-cljs — tools.deps resolves to highest.
3. The `no.en.core` redef warning is suppressed via `:compiler-options` —
   a known annoyance, not a conflict.
4. The embedded shadow path (Path A) is configured and available but gated
   OFF by default; the sidecar path (Path B) is the operative choice.
5. The live system (pid 4806 on :7070) is running with shadow-cljs on the
   classpath right now — empirical proof of compatibility.

## Citations

- `futon3c/deps.edn` lines 28-39 (`:dev` alias with shadow-cljs + CLJS deps)
- `futon3c/shadow-cljs.edn` full file (multi-build config, `:source-paths`, `:dependencies`, `:compiler-options`)
- `futon2/web/war-machine/deps.edn` (transitive dep versions: http-kit 2.8.0, ring-core 1.12.2)
- `futon4/dev/web/webarxana/deps.edn` (transitive dep versions: http-kit 2.8.0, ring-core 1.12.2, buddy-*)
- `futon2/web/war-machine/package.json` line 13 (`"shadow-cljs": "2.28.20"`)
- `futon4/dev/web/webarxana/package.json` line 12 (`"shadow-cljs": "2.28.20"`)
- `scripts/dev-laptop-env` (FUTON3C_SHADOW_AUTOSTART=false default; start_shadow_watch_sidecar function)
- `futon3c/holes/missions/M-single-entry-point.md` line 85 (Q-classpath-compat question), line 110 (DERIVE resolution path)
