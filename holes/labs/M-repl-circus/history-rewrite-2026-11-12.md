# History rewrite of 2026-11-12: commit id map

Joe, 2026-11-12: "probably best to remove server-remote-access-setup from public view".
The `server-remote-access-setup` memory had been copied whole into two test fixtures
(test/fixtures/dog/turn-WHNTlI.record.json and
test/fixtures/dog-helps/memories/-home-joe__server-remote-access-setup.md). claude-13
replayed the 16 commits from 141a8f97f to e5bf958fa with those two blobs replaced by
their header-only versions, keeping each commit's author, committer, dates and message;
the tip tree is unchanged. Remote master was force-pushed (with lease) from
e5bf958fa to 486285a1d. Commit messages written before the rewrite still cite the old ids;
this map translates them.

| old | new |
|---|---|
| `141a8f97f` | `4c14c996f` |
| `4fc17816f` | `9a3e83ac9` |
| `a8d3e3fdc` | `2aa511ee8` |
| `1f600fb36` | `190c9a180` |
| `bca608795` | `c15b97540` |
| `49623161b` | `d8f29d2ce` |
| `b1a03ee25` | `6f81903df` |
| `d83289ed0` | `cce343d82` |
| `3108d2c26` | `ae1a4c632` |
| `3ad1a056e` | `38075d9aa` |
| `8c009dbed` | `68a760209` |
| `657bc9eef` | `604dc1811` |
| `a25df6a38` | `71df7c606` |
| `50900dcfd` | `532bb6799` |
| `c182bd03b` | `ec1c5e7ab` |
| `e5bf958fa` | `486285a1d` |
