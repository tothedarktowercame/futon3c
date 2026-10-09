# Lane D panic/error runtime inventory

Lean 4.29.0 source locations are the definition of this list. `size` is the
inclusive source-line span; `observed` means the symbol was present in the
blank-path startup/phase evidence, not that the function's failure branch was
entered.

| symbol | definition | size | observed |
|---|---|---:|---|
| `lean_internal_panic` | `src/runtime/object.cpp:85-89` | 5 | no |
| `lean_internal_panic_out_of_memory` | `src/runtime/object.cpp:91-93` | 3 | no |
| `lean_internal_panic_unreachable` | `src/runtime/object.cpp:95-97` | 3 | no |
| `lean_internal_panic_rc_overflow` | `src/runtime/object.cpp:99-101` | 3 | no |
| `lean_set_exit_on_panic` | `src/runtime/object.cpp:106-108` | 3 | no |
| `lean_internal_set_exit_on_panic` | `src/runtime/object.cpp:111-114` | 4 | no |
| `lean_set_panic_messages` | `src/runtime/object.cpp:116-118` | 3 | yes (startup) |
| `lean_panic` | `src/runtime/object.cpp:178-180` | 3 | no |
| `lean_panic_fn` | `src/runtime/object.cpp:182-186` | 5 | no |
| `lean_array_get_panic` | `src/runtime/object.cpp:457-459` | 3 | no |
| `lean_array_set_panic` | `src/runtime/object.cpp:461-464` | 4 | no |
| `lean_dbg_trace` | `src/runtime/object.cpp:2714-2717` | 4 | no |
| `lean_dbg_sleep` | `src/runtime/object.cpp:2719-2723` | 5 | no |
| `lean_dbg_trace_if_shared` | `src/runtime/object.cpp:2725-2730` | 6 | no |
| `lean_dbg_stack_trace` | `src/runtime/object.cpp:2732-2735` | 4 | no |
| `lean_decode_io_error` | `src/runtime/io.cpp:161-256` | 96 | no |
| `lean_decode_uv_error` | `src/runtime/io.cpp:258-` (same mapping through the file's end) | — | no |

The call tables contain no `lean_panic*`, `lean_dbg_*`, `lean_decode_*`, or
array-panic call. `lean_set_panic_messages` is the only scoped symbol found in
`startup.cg` (one call at record 301240).
