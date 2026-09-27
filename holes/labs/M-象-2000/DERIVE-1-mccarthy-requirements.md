# DERIVE-1 — Elephant 2000 requirements, translated into the stack

claude-14, 2026-09-26. Source: `storage/references/mccarthy-elephant-2000/elephant.tex`
(line numbers below are from that file; the paper's main body is l.130–1530, the
rest is McCarthy's appended notes and drafts, cited where they add something).
Target language: Clojure + XTDB (futon1b) + library patterns + Agency agents.
Existing coverage: the six `futon3/library/象/` patterns (claude-1, 2026-09-23,
written without the paper, all `尚未评审`).

Format per row: **R-n** — McCarthy (tex line) → requirement in the stack →
existing 象 coverage → gap.

2026-09-27 P22 补记：末列依 `translation/declare-what-is-lost` 声明翻译的删减、固定选择与成立条件；“丢失”不等于尚未实现的 Gap，“无实质丢失”也不声称实现已合规。旧列保留原样（包括当时的 coverage 判断）。TeX 行号按换行符计数，不把文件中的换页符另算一行。

## A. Acts (speech acts and abstract performatives)

| R | McCarthy | In the stack | 象 coverage | Gap | 丢失（Dropped） |
|---|---|---|---|---|---|
| R1 | I/O distinguishes requests, questions, offers, acceptances, permissions, answers, assertions, promises, commitments (l.151–158) | Every record of a turn, bell, notice or wake carries an act type and an origin (operator / agent / harness) | 言即行 (type on the envelope) | Act types in use are the miners' 19 intents; no offer, acceptance, permission or withdraw; harness notices are stored as Joe's `:question` | 丢失：I/O 句子的意义由语言与程序共同规定（l.152–160）；envelope 的 act type 与 origin 不等于这些句子的语义。 |
| R2 | Abstract performatives: internal commitments, not necessarily output, on which correctness depends (l.243–252, 405–413, 796) | Installing a gate, parking, clocking in, loading code are commitments of the system and are recorded as such, even when no message goes out | 诺必践 covers spoken promises only | The 09-24 requisition gate was a standing commitment that existed only as code | 无实质丢失：保留“不必输出但影响正确性”的内部承诺；gate、park 等是实例，不应被当成穷尽清单（l.398–415）。 |
| R3 | Intrinsic correctness conditions generated from the program text: answers truthful, promises kept, commitments fulfilled, authorised commands obeyed (l.172–178, 1880–1886) | From the typed act stream, generate the checks mechanically: each promise yields a fulfilment check, each answer a truth-and-responsiveness check | none (each 象 pattern states one condition; nothing generates them) | No generator | 丢失：从程序文本与行为的形式理论推导正确性命题，缩成从已发生的 act stream 生成检查；未发生或漏记的行为不在此检查域内（l.165–178）。 |
| R4 | Assertions truthful; sincerity weaker; the programmer may omit a condition, and should say so (l.356–362) | Agent assertions cite the record they rest on; where truth is not checked, the record says "unchecked" rather than nothing | 答必真且中的, 两种规格 | — | 丢失：基于领域公理的真值证明，以及基于程序信念理论的 sincerity；引用记录与标记 unchecked 均不能替代这两者（l.356–363）。 |
| R5 | Answers truthful *and* responsive; "I don't know" admissible; false presuppositions; responsive = questioner then knows (l.368–378, 838–893) | An answer gives the value in the form asked (the sha, the count), not where it could be found; "unknown" is a typed answer | 答必真且中的 | The knows-what test (l.882) is not stated | 丢失：接收者的 knows-what 条件、对象与概念的区分，以及纠正错误预设的回答；返回指定格式只是特殊情形（l.368–386、867–890）。 |
| R6 | Simple promise = internal commitment + truthful output that it exists; publicly it creates an obligation (l.398–410) | Making a promise writes a durable record (beneficiary, content, deadline, test); its outcome is a second act | 诺必践 | Parks are the promises and live in `/tmp`; released records are deleted (MAP Q4) | 丢失：公开承诺本身创设义务，且“内部承诺存在”的输出须为真；持久化记录与第二个 outcome act 未给出这两项语义（l.398–415）。 |
| R7 | Two kinds of obligation: the program's, and the operating organisation's (l.418–429, 1750–1756) | An agent's commitment vs a commitment it creates for Joe or for futon; an act under Joe's name commits Joe | none | Notices sent under Joe's name committed him to rules he never made | 丢失：两类义务的法律内容、与其他考虑冲突时的处理及违约后果由制度定义；区分 agent 与 Joe/futon 的账目尚未承载这些规则（l.418–427）。 |
| R8 | Illocutionary vs perlocutionary; on input, "hearing that" vs "learning that" (l.265–272, 816–836, sharp version l.2172–2180) | Delivered vs done (outputs); received vs understood (inputs — 象's annotation is the "learning that" record) | 两种规格 | Input side not stated | 丢失：learning that 要求输入确实给出世界事实，perlocutionary 成功涉及世界而非仅有 understood/done 标记；annotation 不能充当该事实的证明（l.816–834）。 |
| R9 | Three levels of specification: internal, input-output, accomplishment; accomplishment needs axioms about the world (l.1476–1479, 604–611, 2230–2240) | A rule states all three: e.g. requisition gate — internal (installed), I/O (refuses untargeted jobs), accomplishment (quota not exhausted), with the world assumption named | 两种规格 has two of the three | The accomplishment level is what an incident's clearing proof checks; nothing records it | 无实质丢失：三层规格及从 I/O 到 accomplishment 所需的世界假设均保留；quota 案例不穷尽世界约束（l.604–611、1476–1479）。 |
| R10 | Authority: the program does only what it is authorised to do; an order is proper only if the speaker has authority; authority tree up to people; delegation (l.169, 952–967, 1515–1518, 1784–1788, 2002) | Each act records under whose authority it is made; delegation via bells forms the tree; nobody acts under another's name without a recorded grant | none | New pattern needed | 丢失：从“只做获授权之事”（l.1784–1788）收窄到 signatory mismatch，未承载范围、有效时间与逐级委托链检查；署名一致仍可越权。`futon3/library/象/名分有据.flexiarg` 已于 2026-09-27 补回这三项，属于模式修订，并非本行已证明或实现。 |
| R11 | Requests for permission and giving permission (l.151, 2190) | Joe's go-aheads are permissions, attached to what they permit | none | Matches the mined `operator/grant-the-go-ahead` family | 丢失：双方均可请求及授予 permission 的一般性；这里只实例化 Joe 的 go-ahead，未承载程序向人或其他程序授权（l.2190、1784–1789）。 |
| R12 | Offers and acceptances; joint acts (agreements) where who offered last may be unknown (l.151–153, 1484–1488) | Agent lists options, Joe accepts one: an agreement recorded as a joint act | none | Matches `operator/accept-the-agents-listed-options` | 丢失：可以知道 agreement 已成立而不知道谁最后 offer/accept；“agent 列选项、Joe 接受”固定了原文允许未知的角色与次序（l.1484–1488）。 |
| R13 | Speech acts are relative to institutions, which change and are designed (l.761–763, 1453–1470, 1779–1781) | Protocols (bell, park, gates, CLAUDE.md rules) are institutions; each is versioned and dated, and an act is judged under the institution in force at its time | none | Red tape = an institution nobody re-examined | 丢失：制度规定行为创设哪些权利义务，并可证明程序满足法律要求；版本与日期只定位制度，未承载制度的这些实质规则（l.1453–1474）。 |
| R14 | Commitments hold nonmonotonically: valid unless there is a specific reason not to (l.766–770, 1766–1772) | A commitment is defeated only by a recorded reason; the reason is itself an act | none | Pairs with R19 revoke | 丢失：非单调公理只部分刻画承诺，例外理由可来自机场或值班主管；“理由必须是已记录 act”增添记录完备性条件，并非原文的全部例外语义（l.765–769、1769–1778）。 |
| R15 | Pick and choose among philosophers' conditions: fulfilment need not be *caused* by the promise (l.1506–1522) | A park is kept if the awaited job completes, whatever made it complete | — | Design freedom, not a gap | 无实质丢失：保留“无需证明履约由承诺所致”；park 是该自由的实例，不意味着免去履约证明（l.1511–1515）。 |
| R16 | Non-Elephant programs can be read as if they were (l.802–806) | Legacy outputs (commits, bells, parks, notices) are interpreted as acts by 象 and the miners | 翻译契约 | This is what the mining is | 无实质丢失：保留把非 Elephant 程序的 I/O 按行为解释的设计立场；解释为 promise 不等于证明它已履行（l.801–805）。 |

## B. Reference to the past

| R | McCarthy | In the stack | 象 coverage | Gap | 丢失（Dropped） |
|---|---|---|---|---|---|
| R17 | One virtual history list; recording is a side effect of acting; the program's own actions are included (l.431–441, 676–688) | The evidence store is the history; every act the stack takes is written there, including its own internal ones | 象不忘 | Reloads, queue entries, park records are not in it (rewind finding) | 丢失：history 是虚拟语义，编译实现可不记录保证不会再引用的信息；每个 act 都实际写 evidence store 固定了更强的存储条件（l.431–441、677–682）。 |
| R18 | Functions of the past: value at a time, time of an event, first/last time, time-valued functions, sets of intervals (l.452–497) | As-of reads plus aggregate queries ("last time this incident class was cleared") | 象不忘 (as-of view) | `/evidence` has no as-of (MAP Q2) | 丢失：整个过去上的时间值函数、作为对象的区间集合，以及子程序返回时变量恢复的历史语义；as-of 与“最后一次”查询未覆盖这些表达能力（l.452–494）。 |
| R19 | `exists(t, commitment x)` ≡ arose before t and not revoked since; `make`, `cancel`, `exists` language-level (l.545–551, 1398–1402) | XTDB valid-time write (arises), valid-time delete (revoke), read at T (exists) | 象不忘 in part | Only hyperedges do this today; revoke has no act type (R-withdraw) | 丢失：make/cancel/exists 对抽象对象的独立公理及 arises/revoke 的严格时间区间条件；XTDB 的 valid-time 写删读需另证这些语义，不能直接等同（l.545–551、1395–1398）。 |
| R20 | Parsing the past: pattern-match the history to bind variables; Prolog-style matching may suffice (l.693–700, 2044–2080) | core.logic relations over the act history (elephantKanren's approach); the same matcher parses turns into pattern cascades | none | New pattern; shared with the cascade-parse work | 丢失：连续时间历史不必由原子事件组成，可能需要广义匹配；core.logic 上的离散 act 关系固定了历史载体，未覆盖该扩展（l.2071–2080）。 |
| R21 | Modify the program without knowing its data structures ("don't seat Iranians next to Iraqis", l.2050–2064; also l.216–221) | Joe's constraints ("no notices under my name") are rules over acts, stated without knowing `followup_queue.clj` | none | The strongest practical requirement in the paper for this stack | 无实质丢失：保留“不知道数据结构也能提出程序修改”的要求；以 act 规则表达不等于已经实现原文由历史导出数据结构的方案（l.2049–2061）。 |
| R22 | Full set theory in references to the past (l.600, 1789–1791) | Counts and sets over acts (open commitments, capacity-style limits) | — | Query-language capability | 丢失：full set theory 的按性质构造集合，以及时间、事件集合；counts 与 open-act sets 只是其有限查询子集（l.1791–1793、1994–1996）。 |
| R23 | Interpreted and compiled forms have the same I/O behaviour; compiled data structures remember only what is needed (l.672–711) | Caches and queues (park file, follow-up queue, clock store) are compiled forms of the history and must be rebuildable from it | none | `/tmp/futon3c-parked-on.edn` is not derivable from the store | 丢失：解释与编译形式的 I/O 行为等价要求，及编译后可完全没有显式 history 的自由；可从 history 重建缓存本身不证明行为等价（l.704–711）。 |

## C. Program as logic, and operation

| R | McCarthy | In the stack | 象 coverage | Gap | 丢失（Dropped） |
|---|---|---|---|---|---|
| R24 | One input at a time, serialised by the runtime; inputs matching no statement are rejected by the runtime (l.511–516, 687–692) | Per-agent turn serialisation (exists); untranslatable input returned with a typed reason | 翻译契约 (route-the-untranslatable) | — | 丢失：一个 Elephant 程序的一次一个输入，缩成 per-agent 串行；多 agent 组成同一程序时，尚无共同输入次序保证（l.511–516、687–693）。 |
| R25 | The program is a logical sentence; properties follow from it plus domain axioms; `arises`, `outputs`, `revoke` are circumscribed (l.1304–1416, 1436–1446) | Rules as relations; the store is taken as complete for act predicates, so proofs over it assume every act of those types was recorded | none | Circumscription makes R17's completeness a soundness condition for clearing proofs | 丢失：程序与外部世界的状态演化、未知世界函数的公理，以及对 arises/outputs/revoke 的 circumscription；store 完备假设不是这套逻辑语义本身（l.1304–1346、1413–1416）。 |
| R26 | A program should be able to answer what its commitments are in a given state (l.1428–1436) | `GET` open commitments of agent X as of T: parks, promises, gates it installed, owed and owing | none | Nothing answers this today | 无实质丢失：保留在给定状态回答当前动态承诺的能力；as-of T 是具体查询接口，列举的 park/gate 不应排除其他内部承诺（l.1432–1437）。 |
| R27 | The compiler makes assumptions, reports them, and the user can reply `maybe(not p)` (l.1976–1990) | An agent implementing a request states the assumptions it made; Joe's correction is a recorded act that forces the alternative | none | claude-11's "notices go out as the caller" was an unreported assumption | 丢失：maybe(not p) 要求纳入非 p 的可能性，并不断言非 p 为真；“forces the alternative”可能把非单调的可能性修订误译为单一反向决定（l.1976–1986）。 |
| R28 | Committed future actions not triggered by an input: at a promised time, or long-running (l.1998–2000) | Parks with deadlines, timers, scheduled jobs (MAP Q4) | 诺必践 | See R6 | 丢失：请求启动后持续很久的动作及其持续履约；deadline/timer/scheduled job 说明何时触发，未说明如何执行这种长程动作（l.1998–2001）。 |
| R29 | Communications among parts of a program may be speech acts (l.1481–1482) | Agent-to-agent bells and harness-internal messages are acts, typed like Joe's | 言即行 | Only Joe's turns are mined | 无实质丢失：保留把程序内部通信视为 speech act 的选择；推广到 harness 消息是 stack 的范围选择，不是 McCarthy 的强制规定（l.1481–1482）。 |
| R30 | Outputs with long-term meaning, not display updates (l.979–996) | Typed records, not only rendered turn text | 翻译契约 | — | 丢失：跨应用仍有确定意义、可供其他程序使用的输出；typed record 不自动保证语义独立于应用或显示格式（l.979–996）。 |
| R31 | Don't require too much intelligence of the programs you interact with (l.905–909) | Obligations placed on agents and on Joe stay cheap: one-line requisition, cheap incident reports | none | Constraint on every new rule | 丢失：逐用途分析所需智能，并适配交互程序实际具备的能力；“一行、便宜”只约束操作成本，未检验理解与推理负担（l.897–909）。 |
| R32 | Mental state includes intentions, authorisations, obligations and "generalized accounts receivable" (l.2004–2008, 2265) | Per-agent ledger: what it owes (promises, parks) and what is owed to it (awaited bellbacks) | none | Same query as R26, both directions | 丢失：非命题的 intentions、authorizations，以及世界中的授权和义务状态；owed/owing ledger 只保留其中的债务面（l.2006–2010、2265–2266）。 |

## Coverage by the six 象 patterns

- 言即行 — R1, R29 (act type on every message). Goes beyond McCarthy with force levels 轻/平/强.
- 诺必践 — R6, R28 (spoken promises with deadlines). Misses R2 (unspoken commitments).
- 两种规格 — R8, two of R9's three levels.
- 答必真且中的 — R4, R5 (lacks the knows-what test).
- 象不忘 — R17, R18, part of R19 (reference by id, as-of reads). Revoke side missing.
- 翻译契约 — R16, R24, R30 (the mining contract).

Uncovered: R3, R7, R10–R14, R20–R23, R25–R27, R31–R32.

## Proposed patterns (names for Joe to judge; not yet written)

| proposed | covers | conclusion (draft) |
|---|---|---|
| 两种规格 → **三层规格** (amend) | R8, R9 | A rule states its internal, I/O and accomplishment specs, and names the world assumption linking the last two; the accomplishment spec is what an incident's clearing proof checks |
| **收回亦是行** | R14, R19, Q6 | Withdrawal is its own act, citing what it withdraws; commitments end only by such an act or a recorded defeating reason (chip `op-drop-stack`) |
| **名分有据** | R7, R10, R11 | Every act records whose authority it is made under; nobody speaks in another's name without a recorded grant |
| **欠与被欠** | R2, R26, R32 | Every agent can answer, as of any T, what it owes and what is owed to it, including commitments it never announced |
| **以史为据** | R20, R21, R22, R25 | Rules are relations over the act history, so a constraint can be stated without knowing the data structures |
| **视图出于史** | R17, R23, R25 | Every cache and queue is rebuildable from the history; an act the history does not record did not happen for proof purposes |
| **制度有时** | R13 | Protocols are dated and versioned; an act is judged under the protocol in force at its time |
| **明言假设** | R27, R31 | An implementation reports the assumptions it made, and a correction to one is a recorded act |
| **要约与接受** | R11, R12 | An offer and its acceptance form an agreement recorded as one joint act |
