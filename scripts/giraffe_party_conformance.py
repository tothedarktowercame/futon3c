#!/usr/bin/env python3
"""Read-only source audit using stdlib. Run with python3 -I.
Optional --json-out atomically writes a report; it never modifies sources.

Checks source values; character reason checks also parse displayed reasons. Missing inputs produce
explicit ABSENT results; malformed inputs FAIL. An absence is not verification.
Lean references are relative to futon5/lean-arxana. Sortedness theorems have
sorted-input hypotheses: a failed check does not refute their Lean proofs.
The two Questor statements overlap necessarily on a unique-item source.
"""
import argparse
from contextlib import closing
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import sys
import sqlite3
import tempfile
import time

CHARACTERS = ('thor', 'merlin', 'questor', 'thyra')
READY = 'ready (one small handoff)'
CHECKS = {
    'thor_monotone_unblocks': 'Circus.Characters.thor_monotone_unblocks',
    'thyra_only_harm_items': 'Circus.Characters.thyra_only_harm_items',
    'thyra_monotone': 'Circus.Characters.thyra_monotone',
    'merlin_monotone_score': 'Circus.Characters.merlin_monotone_score',
    'questor_cost_gate': 'Circus.Characters.questor_cost_gate',
    'questor_never_lists_unready': 'Circus.Characters.questor_never_lists_unready',
    'party_nodup': 'Circus.Retro.partyVote_fresh_and_nodup',
    'party_turns': 'Circus.Retro.sweep_head_contributes; partyVote; Circus.Cascade.cascadeVote_pulled_chain',
    'party_relative_order': 'Circus.drawFresh_survivor; Circus.Retro.drawFresh',
    'run_file_digests': 'Circus.Reproduction.Receipt.passInputDigest',
    'one_current_reading': 'Circus.Supersession.OneCurrent',
    'corpus_generation_digest': 'Circus.Reproduction.Receipt.corpusGeneration',
    'thor_missing_counts': 'Circus.Characters.missing_count_is_violation',
    'thyra_reasons_faithful': 'Circus.Characters.thyra_terms_faithful',
    'questor_reasons_faithful': 'Circus.Characters.questor_terms_faithful',
    'merlin_reasons_faithful': 'Circus.Characters.merlin_terms_faithful',
    'reading_source_current': 'Circus.Freshness.readingMatches; reason',
    'needs_supplied': 'Circus.Cascade.NeedsSupplied',
    'cascade_strong': 'Circus.Cascade.cascadeVote_strong; cascadeVote_exceptions_real',
}


class Missing(ValueError):
    pass


def convergence(rows):
    """Independent source graph construction: undirected, two-hop, unfinished."""
    entities, aliases = {}, {}
    for row in rows:
        entities.setdefault(row['entity'], row)
        for alias in [row.get('entity'), row.get('name'),
                      *str(row.get('aka') or '').split(',')]:
            if alias and alias.strip():
                aliases.setdefault(alias.strip().lower(), row['entity'])
    graph = {key: set() for key in entities}
    for row in rows:
        for relation in str(row.get('relations') or '').split(';'):
            match = re.match(r'\s*[a-z-]+\s+(.*)', relation)
            if match:
                for name in re.sub(r'\([^)]*\)', '', match[1]).split(','):
                    target = aliases.get(name.strip().lower())
                    if target in graph and target != row['entity']:
                        graph[target].add(row['entity'])
                        graph[row['entity']].add(target)
    unfinished = {key for key, row in entities.items()
                  if row.get('status') in ('partly', 'hypothesised')}
    scores = {}
    for key, adjacent in graph.items():
        reachable = adjacent.copy()
        for neighbor in adjacent:
            reachable.update(graph[neighbor])
        scores[key] = len((reachable & unfinished) - {key})
    # Producer's eligibility uses the last row for duplicate entity statuses.
    last_status = {row['entity']: row.get('status') for row in rows}
    return scores, {key for key, value in last_status.items()
                    if value in ('partly', 'hypothesised')}


def audit(party, pass_data=None, pending=None, lexicon=None, run=None):
    """Return one independently evaluated result per named check.

    None means an unavailable source; empty lists mean known empty sources.
    Each checker reports all detected offenses (with source values).
    """
    def character(name):
        rows = [c for c in party['characters'] if c['id'] == name]
        if len(rows) != 1:
            raise ValueError('expected exactly one character: ' + name)
        return rows[0]['top']

    def source(value, name):
        if value is None:
            raise Missing('source-unavailable: ' + name)
        return value

    def pending_map():
        rows = source(pending, 'pending index')
        result = {r['id']: r for r in rows}
        if len(result) != len(rows):
            raise ValueError('pending-item-not-unique: Lean hdet not established')
        return result

    def pass_map():
        rows = source(pass_data, 'pass.edn')['order']
        result = {r['id']: r for r in rows}
        if len(result) != len(rows):
            raise ValueError('pass-item-not-unique')
        return result

    def need(mapping, item):
        if item not in mapping:
            raise Missing('source-item-unavailable: ' + item)
        return mapping[item]

    def natural(value, label):
        if value is None:
            raise Missing('source-value-unavailable: ' + label)
        if type(value) is not int or value < 0:
            raise ValueError('not a Nat: ' + label + '=' + repr(value))
        return value

    def pairs(values):
        return [{'left': a, 'right': b} for a, b in zip(values, values[1:])
                if a['key'] > b['key']]

    def thor():
        data = pass_map()
        return pairs([{'item': r['item'], 'key': [
            -natural(need(data, r['item']).get('unblocks'), r['item'] + '.unblocks'),
            -natural(need(data, r['item']).get('cone'), r['item'] + '.cone')]}
                      for r in character('thor')])

    def harm():
        data = pending_map()
        return [{'item': r['item'], 'harm': need(data, r['item']).get('harm')}
                for r in character('thyra')
                if not str(need(data, r['item']).get('harm') or '').strip()]

    def thyra():
        data = pass_map()
        return pairs([{'item': r['item'], 'key':
                       natural(data[r['item']]['rank'], r['item'] + '.rank')
                       if r['item'] in data and data[r['item']].get('rank') is not None
                       else 10**6} for r in character('thyra')])

    def merlin_values():
        data = pending_map()
        scores, unfinished = convergence(source(lexicon, 'lexicon'))
        values = []
        for row in character('merlin'):
            item = row['item']
            p = need(data, item)
            feature = re.sub(r'^[MET]-', '', item)
            own = need(scores, feature)
            named = max((scores[e] for e in p.get('entities', [])
                         if e in scores and e in unfinished), default=0)
            values.append({'item': item, 'own': own, 'named': named,
                           'score': own + named / 2,
                           'displayed_score': row.get('score'),
                           'label': p.get('pending_label'),
                           'key': [-(2 * own + named), item]})
        return values

    def merlin():
        values = merlin_values()
        return (pairs(values) + [v for v in values
                                if v['displayed_score'] != v['score']
                                or v['label'] == 'may be no longer wanted'])

    def questor():
        data = pending_map()
        return [{'item': r['item'], 'pending_label': need(data, r['item']).get('pending_label')}
                for r in character('questor')
                if need(data, r['item']).get('pending_label') != READY]

    def chosen():
        return party['party']['top']

    def nodup():
        seen, bad = set(), []
        for row in chosen():
            if row['item'] in seen:
                bad.append({'item': row['item'], 'chosen_by': row['chosen_by']})
            seen.add(row['item'])
        return bad

    def turns():
        """Replay complete-chain turns (giraffe_party.py:263-324 party_vote).

        Pulled entries are exempt from ordinary fresh-head rotation, but must
        form the declared need chain immediately before their ordinary pick.
        They enter seen before subsequent turns. Deferred turns consume their
        head without emitting it. Dependency replay uses the pinned run when
        available; supplied needs support fixture/legacy audits only.
        """
        block = party['party']
        capacity = block.get('capacity', len(chosen()))
        if type(capacity) is not int or capacity < 0:
            raise ValueError('invalid-party-capacity')
        needs = {r['item']: {n['item'] for n in r.get('needs', [])} for r in chosen()}
        if run is not None and 'capacity' in block:
            inputs = load(Path(run) / 'input.edn', edn=True)
            if inputs is None or 'edges' not in inputs:
                raise Missing('pinned-input-edges')
            needs = {}
            for edge in inputs['edges']:
                if edge.get('class') != 'alternative':
                    needs.setdefault(edge['from'], set()).add(edge['to'])
        elif block.get('cascade_deferred'):
            raise Missing('pinned-input-edges-for-deferred-turns')
        positions = {}
        remaining = []
        for ci, name in enumerate(CHARACTERS):
            items = [r['item'] for r in character(name)]
            remaining.append((name, items))
            for i, item in enumerate(items):
                positions[item] = min(positions.get(item, (i, ci, item)), (i, ci, item))
        seen, expected, deferred, exceptions = set(), [], [], []
        while remaining and len(expected) < capacity:
            name, items = remaining.pop(0)
            fresh = [item for item in items if item not in seen]
            if not fresh:
                continue
            root = fresh[0]
            remaining.append((name, fresh[1:]))
            path, cursor = [], root
            while cursor not in path:
                path.append(cursor)
                prerequisites = needs.get(cursor, set())
                if not prerequisites or prerequisites.intersection(seen):
                    cursor = None
                    break
                cursor = min(prerequisites, key=lambda x: positions.get(x, (float('inf'), float('inf'), x)))
            if cursor is not None:
                exceptions.append({'item': root, 'cycle': path[path.index(cursor):]})
                path = [root]
            if len(path) > capacity - len(expected):
                deferred.append({'item': root, 'needs_slots': len(path)})
                continue
            for item in reversed(path):
                row = {'item': item, 'chosen_by': name}
                if item != root:
                    row['pulled_for'] = root
                expected.append(row)
                seen.add(item)
        actual = [{k: r[k] for k in ('item', 'chosen_by', 'pulled_for') if k in r}
                  for r in chosen()]
        bad = []
        if expected != actual:
            bad.append({'expected': expected, 'actual': actual})
        if deferred != block.get('cascade_deferred', []):
            bad.append({'rule': 'deferred-turns', 'expected': deferred})
        if exceptions != block.get('cascade_exceptions', []):
            bad.append({'rule': 'cycle-exceptions', 'expected': exceptions})
        # Check the supplied chain too: the pinned graph must not conceal a
        # missing/incorrect needs field on a pulled run's displayed entries.
        pulled = []
        for row in chosen():
            if 'pulled_for' in row:
                pulled.append(row)
            else:
                for left, right in zip(pulled, pulled[1:] + [row]):
                    if (left['pulled_for'] != row['item'] or
                            left['chosen_by'] != row['chosen_by'] or
                            left['item'] not in {n['item'] for n in right.get('needs', [])}):
                        bad.append({'rule': 'pulled-chain', 'item': left['item']})
                pulled = []
        if pulled:
            bad.append({'rule': 'unfinished-pulled-chain'})
        return bad

    def relative():
        """Ordinary picks preserve character order; giraffe_party.py:263-324 chains
        are exempt here and validated by turns instead."""
        bad = []
        for name in CHARACTERS:
            source_items = [r['item'] for r in character(name)]
            offset = 0
            for row in chosen():
                if row['chosen_by'] == name and 'pulled_for' not in row:
                    try:
                        offset = source_items.index(row['item'], offset) + 1
                    except ValueError:
                        bad.append({'item': row['item'], 'chosen_by': name,
                                    'remaining_source_order': source_items[offset:]})
        return bad

    # Merlin's half-integral scores are exact in the Lean model since futon5
    # afdb6eb (MerlinRow.score is the doubled score), so no representation check.
    functions = [thor, harm, thyra, merlin, questor, questor, nodup, turns,
                 relative]
    results = []
    for (name, theorem), check in zip(CHECKS.items(), functions):
        try:
            offenses = check()
            status, detail = ('FAIL', offenses) if offenses else ('PASS', [])
        except Missing as exc:
            status, detail = 'ABSENT', {'kind': 'missing-source', 'reason': str(exc)}
        except (IndexError, KeyError, TypeError, ValueError) as exc:
            status, detail = 'FAIL', {'kind': 'invalid-input', 'reason': str(exc)}
        results.append({'check': name, 'theorem': theorem, 'status': status, 'detail': detail})
    results.append(run_file_digests(run))
    results.append(one_current_reading(run))
    results.append(corpus_generation_digest(run))
    results.append(thor_missing_counts(party, pass_data))
    results.append(thyra_reasons_faithful(party, pass_data, pending))
    results.append(questor_reasons_faithful(party, pending, lexicon))
    results.append(merlin_reasons_faithful(party, pending, lexicon))
    results.append(reading_source_current(run))
    results.append(needs_supplied(party, run))
    results.append(cascade_strong(party, run))
    return results


def run_needs(run):
    """Independent pinned input reader; never uses displayed needs or producer.

    Mirror giraffe_party.py:253-260,335-351: from needs to, alternatives
    excluded, external/completed prerequisites retained. Direction originates
    in readings.clj:110-114 (unblocks reversed), pass.clj:56-61,337.
    """
    if run is None:
        raise Missing('run/input.edn')
    inputs = load(Path(run) / 'input.edn', edn=True)
    if inputs is None:
        raise Missing('run/input.edn')
    if 'edges' not in inputs or 'items' not in inputs:
        raise Missing('input.edn edges/items')
    needs, states = {}, {}
    for item in inputs['items']:
        states[item['id']] = item.get('state-group')
    for edge in inputs['edges']:
        if edge.get('class') != 'alternative':
            needs.setdefault(edge['from'], set()).add(edge['to'])
    return needs, states


def needs_supplied(party, run):
    """Every displayed need and its rank/state agrees with the pinned run.

    Mirror the producer's party_dependencies (giraffe_party.py:335-351),
    independently parsed via run_needs. An explicit [] is required for roots.
    """
    def result(status, detail):
        return {'check': 'needs_supplied', 'theorem': CHECKS['needs_supplied'],
                'status': status, 'detail': detail}
    try:
        needs, states = run_needs(run)
        rows = party['party']['top']
        if not rows:
            raise Missing('party top rows')
        ranks = {r['item']: r['rank'] for r in rows}
        bad = []
        for row in rows:
            item = row['item']
            displayed = row.get('needs')
            if not isinstance(displayed, list):
                bad.append({'item': item, 'rule': 'needs-list-required'})
                continue
            if {n['item'] for n in displayed} != needs.get(item, set()):
                bad.append({'item': item, 'rule': 'needs-set'})
            for need in displayed:
                target = need['item']
                if 'shown_rank' not in need or need['shown_rank'] != ranks.get(target):
                    bad.append({'item': item, 'need': target, 'rule': 'shown-rank'})
                if 'state' not in need or need['state'] != states.get(target):
                    bad.append({'item': item, 'need': target, 'rule': 'state'})
        return result('FAIL' if bad else 'PASS',
                      {'entries': len(rows), 'violations': bad} if bad else {'entries': len(rows)})
    except Missing as exc:
        return result('ABSENT', {'missing': str(exc)})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError) as exc:
        return result('FAIL', {'kind': 'invalid-input', 'reason': str(exc)})


def cascade_strong(party, run):
    """After the first row, need an earlier row or nothing, using RUN edges.

    Cycle exceptions are explicitly reported, and must be genuine directed
    cycles reachable from the excepted item, not unrelated cycles used as an
    excuse. No displayed needs are trusted by this check.
    """
    def result(status, detail):
        return {'check': 'cascade_strong', 'theorem': CHECKS['cascade_strong'],
                'status': status, 'detail': detail}
    try:
        needs, _ = run_needs(run)
        rows = party['party']['top']
        if not rows:
            raise Missing('party top rows')
        exceptions = party['party'].get('cascade_exceptions', [])
        shown = {r['item'] for r in rows}
        valid, bad = set(), []
        for exception in exceptions:
            item, cycle = exception['item'], exception['cycle']
            reached, todo = set(), [item]
            while todo:
                node = todo.pop()
                if node not in reached:
                    reached.add(node)
                    todo.extend(needs.get(node, set()) - reached)
            if (item not in shown or item in valid or not isinstance(cycle, list) or
                    not cycle or len(set(cycle)) != len(cycle) or
                    cycle[0] not in reached or
                    not all(b in needs.get(a, set())
                            for a, b in zip(cycle, cycle[1:] + cycle[:1]))):
                bad.append({'item': item, 'rule': 'invalid-cycle-exception'})
            else:
                valid.add(item)
        seen = set()
        for index, row in enumerate(rows):
            item = row['item']
            required = needs.get(item, set())
            if index and required and not required & seen and item not in valid:
                bad.append({'item': item, 'rule': 'no-earlier-need'})
            seen.add(item)
        detail = {'entries': len(rows), 'pulled': sum('pulled_for' in r for r in rows),
                  'standalone': sum(not needs.get(r['item']) for r in rows),
                  'exceptions': exceptions}
        if bad:
            detail['violations'] = bad
        return result('FAIL' if bad else 'PASS', detail)
    except Missing as exc:
        return result('ABSENT', {'missing': str(exc)})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError) as exc:
        return result('FAIL', {'kind': 'invalid-input', 'reason': str(exc)})


def thor_missing_counts(party, pass_data):
    """Parse displayed clauses, unlike the older source-only ordering checks.

    Match futon5 lean-arxana/Circus/Basic.lean:301-306 Term.render exactly,
    with semicolon-delimited clauses from giraffe_party.py:122-155. Nat counts
    include measured zero; only absent/null counts require missing disclosure.
    Absence multiplicity and scope match Characters.passViolations.
    """
    def result(status, detail):
        return {'check': 'thor_missing_counts',
                'theorem': CHECKS['thor_missing_counts'], 'status': status, 'detail': detail}

    if pass_data is None:
        return result('ABSENT', {'missing': 'pass.edn'})
    try:
        characters = [ch for ch in party['characters'] if ch['id'] == 'thor']
        if len(characters) != 1:
            raise ValueError('invalid Thor character')
        rows = characters[0]['top']
        if not rows:
            return result('ABSENT', {'missing': 'Thor top rows'})
        entries = pass_data['order']
        by_id = {entry['id']: entry for entry in entries}
        if len(by_id) != len(entries):
            raise ValueError('duplicate pass item')
        top_ids = {row['item'] for row in rows}
        absences = {}
        for absence in party['absences']:
            what = absence['what']
            if not isinstance(what, str):
                raise ValueError('invalid absence')
            for field in ('unblocks', 'cone'):
                prefix = field + ' for '
                if what.startswith(prefix):
                    key = (what[len(prefix):], field)
                    absences[key] = absences.get(key, 0) + 1
        violations, complete, missing = [], 0, 0
        def bad(item, field, rule):
            violations.append({'item': item, 'field': field, 'rule': rule})
        for (item, field) in absences:
            if item not in top_ids:
                bad(item, field, 'absence-outside-thor-top')
        for row in rows:
            item = row['item']
            if item not in by_id:
                bad(item, 'basis', 'item-not-in-pass')
                continue
            reason = row.get('reason')
            if not isinstance(reason, str):
                reason = ''
            clauses = [clause.strip() for clause in reason.split(';')]
            for field in ('unblocks', 'cone'):
                value = by_id[item].get(field)
                count = absences.get((item, field), 0)
                if value is not None and (type(value) is not int or value < 0):
                    bad(item, field, 'source-count-not-nat')
                    continue
                if value is None:
                    missing += 1
                    number, suffix = 0, ' (count missing)'
                    if count != 1:
                        bad(item, field, 'missing-count-requires-exactly-one-absence')
                else:
                    complete += 1
                    number, suffix = value, ''
                    if count != 0:
                        bad(item, field, 'measured-count-has-absence')
                expected = (f'unblocks {number} item(s)' if field == 'unblocks'
                            else f'cone of {number} behind it') + suffix
                # Whole clauses prevent a measured-zero prefix from accepting
                # the missing-count wording, or contradictory duplicate clauses.
                field_clauses = [clause for clause in clauses
                                 if clause.startswith(field + ' ')]
                if field_clauses != [expected]:
                    bad(item, field, 'rendered-count-clause-mismatch')
        if violations:
            return result('FAIL', violations)
        return result('PASS', {'rows': len(rows), 'complete_counts': complete,
                               'missing_counts': missing})
    except (IndexError, KeyError, TypeError, ValueError):
        return result('FAIL', {'kind': 'invalid-thor-count-input'})


def thyra_reasons_faithful(party, pass_data, pending):
    """Compare whole displayed semicolon clauses with source facts.

    Lean: Circus/Basic.lean:307-310 (Term.render), :341-344 (Term.faithful);
    Circus/Characters.lean:229 (thyraTerms), :427 (thyraFacts), :444
    (thyra_terms_faithful). The pending index path is the CURRENT file, not
    a pinned digest: PASS is against that file as it is now. Source rank is
    the stored pass rank, never list position. Missing pass data is ABSENT,
    not evidence that an item has no rank. notInPass renders as two clauses.
    """
    def result(status, detail):
        return {'check': 'thyra_reasons_faithful',
                'theorem': CHECKS['thyra_reasons_faithful'], 'status': status, 'detail': detail}

    if pending is None or pass_data is None:
        return result('ABSENT', {'missing': 'pending index' if pending is None else 'pass.edn'})
    try:
        characters = [ch for ch in party['characters'] if ch['id'] == 'thyra']
        if len(characters) != 1:
            raise ValueError('invalid Thyra character')
        rows = characters[0]['top']
        if not rows:
            return result('ABSENT', {'missing': 'Thyra top rows'})
        pending_by_id = {row['id']: row for row in pending}
        entries = pass_data['order']
        pass_by_id = {row['id']: row for row in entries}
        if len(pending_by_id) != len(pending) or len(pass_by_id) != len(entries):
            raise ValueError('duplicate source item')
        violations, ranked, not_in_pass = [], 0, 0
        def bad(item, rule):
            violations.append({'item': item, 'rule': rule})
        for row in rows:
            item = row['item']
            if item not in pending_by_id:
                bad(item, 'item-not-in-pending-index')
                continue
            source = pending_by_id[item]
            harm = source.get('harm')
            if not isinstance(harm, str) or not harm.strip():
                bad(item, 'harm-line-missing')
                continue
            expected = ['on harm line ' + harm.strip()]
            rank = pass_by_id.get(item, {}).get('rank')
            if rank is not None:
                if type(rank) is not int or rank < 0:
                    bad(item, 'source-rank-not-nat')
                    continue
                ranked += 1
                expected.append(f'Giraffe pass rank {rank}')
            else:
                level = source.get('pending')
                if type(level) is not int or level < 0:
                    bad(item, 'source-level-not-nat')
                    continue
                not_in_pass += 1
                expected.extend(['not in the Giraffe pass', f'pending level {level}'])
            reason = row.get('reason')
            actual = [part.strip() for part in reason.split(';')] if isinstance(reason, str) else []
            if actual != expected:
                bad(item, 'rendered-clauses-mismatch')
        if violations:
            return result('FAIL', violations)
        return result('PASS', {'rows': len(rows), 'ranked': ranked, 'not_in_pass': not_in_pass})
    except (IndexError, KeyError, TypeError, ValueError):
        return result('FAIL', {'kind': 'invalid-thyra-reason-input'})


def questor_reasons_faithful(party, pending, lexicon):
    """Parse exact whole clauses; pending/lexicon are current, unpinned files.

    scripts/giraffe_party.py:198-209 questor_order chooses the highest-scoring
    entity in pending.entities, considering ALL scored features (not only
    unfinished ones). Strict > replacement retains the FIRST entity on ties.
    Do not use best_named: that helper filters unfinished features for Merlin.
    Scores here come from this checker's independent convergence calculation.
    Lean Circus/Basic.lean:315-317 renders the terms; :349-351 defines their
    faithfulness. Characters.lean:342,432,473 defines Questor terms/facts/proof.
    This check verifies the ready clause; questor_cost_gate separately verifies
    the pending ready label. PASS is against the current source files only.
    """
    def result(status, detail):
        return {'check': 'questor_reasons_faithful',
                'theorem': CHECKS['questor_reasons_faithful'], 'status': status, 'detail': detail}

    if pending is None or lexicon is None:
        return result('ABSENT', {'missing': 'pending index' if pending is None else 'lexicon'})
    try:
        characters = [ch for ch in party['characters'] if ch['id'] == 'questor']
        if len(characters) != 1:
            raise ValueError('invalid Questor character')
        rows = characters[0]['top']
        if not rows:
            return result('ABSENT', {'missing': 'Questor top rows'})
        sources = {row['id']: row for row in pending}
        if len(sources) != len(pending):
            raise ValueError('duplicate pending item')
        scores, _ = convergence(lexicon)
        violations, named, no_feature = [], 0, 0
        for row in rows:
            item = row['item']
            if item not in sources:
                violations.append({'item': item, 'rule': 'item-not-in-pending-index'})
                continue
            candidates = [entity for entity in (sources[item].get('entities') or [])
                          if entity in scores]
            # Python max is stable on ties, independently of producer's loop.
            best = max(candidates, key=scores.__getitem__) if candidates else None
            if best is None:
                no_feature += 1
                clause = 'names no scored feature'
            else:
                named += 1
                clause = f'names {best} (convergence {scores[best]})'
            reason = row.get('reason')
            actual = [part.strip() for part in reason.split(';')] if isinstance(reason, str) else []
            if actual != [READY, clause]:
                violations.append({'item': item, 'rule': 'rendered-clauses-mismatch'})
        if violations:
            return result('FAIL', violations)
        return result('PASS', {'rows': len(rows), 'named': named, 'no_feature': no_feature})
    except (IndexError, KeyError, TypeError, ValueError):
        return result('FAIL', {'kind': 'invalid-questor-reason-input'})


def merlin_reasons_faithful(party, pending, lexicon):
    """Check the EXACT producer surface, using independent source convergence.

    giraffe_party.py:178-183 groups featureIs/convergence/withinTwoHops
    (Circus/Characters.lean:304-309, Basic.lean:311-314) into one parenthesized
    clause: that grouping is the producer's, not the model's term list.
    best_named (:188-191) considers only unfinished scored entities and keeps
    the first maximum in entities order. The optional clause appears iff the
    named count is nonzero (:183), agreeing with Lean on nonnegative counts.
    M is the FULL named convergence; only the score contribution is halved.
    merlinKept (:301-302) excludes unwanted items and absent own features.
    Pending and lexicon are current files, not pinned digests.
    """
    def result(status, detail):
        return {'check': 'merlin_reasons_faithful',
                'theorem': CHECKS['merlin_reasons_faithful'], 'status': status, 'detail': detail}

    if pending is None or lexicon is None:
        return result('ABSENT', {'missing': 'pending index' if pending is None else 'lexicon'})
    try:
        characters = [ch for ch in party['characters'] if ch['id'] == 'merlin']
        if len(characters) != 1:
            raise ValueError('invalid Merlin character')
        rows = characters[0]['top']
        if not rows:
            return result('ABSENT', {'missing': 'Merlin top rows'})
        sources = {row['id']: row for row in pending}
        if len(sources) != len(pending):
            raise ValueError('duplicate pending item')
        scores, unfinished = convergence(lexicon)
        violations, with_named, without_named = [], 0, 0
        for row in rows:
            item = row['item']
            if item not in sources:
                violations.append({'item': item, 'rule': 'item-not-in-pending-index'})
                continue
            source = sources[item]
            match = re.fullmatch(r'[MET]-(.+)', item)
            feature = match.group(1) if match else item
            if source.get('pending_label') == 'may be no longer wanted' or feature not in scores:
                violations.append({'item': item, 'rule': 'merlin-kept-excludes-row'})
                continue
            candidates = [entity for entity in (source.get('entities') or [])
                          if entity in scores and entity in unfinished]
            best = max(candidates, key=scores.__getitem__) if candidates else None
            named = scores[best] if best is not None else 0
            expected = f'is {feature} (convergence {scores[feature]}, unfinished work within two hops)'
            if named != 0:
                with_named += 1
                expected += f'; names {best} (convergence {named}, halved)'
            else:
                without_named += 1
            if row.get('reason') != expected:
                violations.append({'item': item, 'rule': 'rendered-reason-mismatch'})
        if violations:
            return result('FAIL', violations)
        return result('PASS', {'rows': len(rows), 'with_named': with_named,
                               'without_named': without_named})
    except (IndexError, KeyError, TypeError, ValueError):
        return result('FAIL', {'kind': 'invalid-merlin-reason-input'})


def run_file_digests(run):
    """Hash only manifest-listed files contained in the resolved run directory."""
    def result(status, detail):
        return {'check': 'run_file_digests',
                'theorem': CHECKS['run_file_digests'], 'status': status, 'detail': detail}

    if run is None:
        return result('ABSENT', {'files': ['manifest.edn']})
    try:
        root = Path(run).resolve()

        def contained(name):
            if not isinstance(name, str) or not name or Path(name).is_absolute():
                raise ValueError('invalid run-relative file name')
            path = (root / name).resolve()
            if not path.is_relative_to(root):
                raise ValueError('file escapes run directory')
            return path

        manifest_path = contained('manifest.edn')
        if not manifest_path.exists():
            return result('ABSENT', {'files': ['manifest.edn']})
        manifest = load(manifest_path, edn=True, strict=True)
        if not isinstance(manifest, dict) or not isinstance(manifest.get('files'), list):
            raise ValueError('manifest files must be a list')
        entries = manifest['files']
        # Validate every path before opening any listed file, including symlinks.
        paths = []
        for entry in entries:
            if not isinstance(entry, dict):
                raise ValueError('invalid manifest file entry')
            name, recorded = entry.get('file'), entry.get('sha256')
            path = contained(name)
            if not isinstance(recorded, str) or not re.fullmatch(r'[0-9a-f]{64}', recorded):
                raise ValueError('invalid recorded SHA-256')
            paths.append((name, path, recorded))
        missing, mismatches = [], []
        for name, path, recorded in paths:
            if not path.is_file():
                missing.append(name)
                continue
            actual = hashlib.sha256(path.read_bytes()).hexdigest()
            if actual != recorded:
                mismatches.append({'file': name, 'recorded': recorded, 'actual': actual})
        if mismatches:
            return result('FAIL', {'mismatches': mismatches, 'missing': missing})
        # passInputDigest is about input.edn: an empty list, or a manifest that
        # does not list the pass input, verifies nothing about it.
        if 'input.edn' not in {name for name, _, _ in paths}:
            return result('ABSENT', {'files': ['input.edn'], 'reason': 'not listed in manifest'})
        if missing:
            return result('ABSENT', {'files': missing})
        return result('PASS', {'files': len(entries)})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError):
        # Do not echo malformed payloads or exception strings containing them.
        return result('FAIL', {'kind': 'invalid-manifest-or-file', 'file': 'manifest.edn'})


def pinned_generation(run, result):
    """Resolve manifest readings-generation.path once; never use its pointer."""
    if run is None:
        return None, None, result('ABSENT', {'missing': 'manifest.edn'})
    run = Path(run).resolve()
    manifest_path = (run / 'manifest.edn').resolve()
    if not manifest_path.is_relative_to(run):
        return None, None, result('FAIL', {'kind': 'manifest-outside-run'})
    if not manifest_path.is_file():
        return None, None, result('ABSENT', {'missing': 'manifest.edn'})
    manifest = load(manifest_path, edn=True, strict=True)
    if not isinstance(manifest, dict):
        return None, None, result('FAIL', {'kind': 'invalid-manifest'})
    pinned = manifest.get('readings-generation')
    if not isinstance(pinned, dict) or not pinned.get('path'):
        return None, None, result('ABSENT', {'missing': 'readings-generation.path'})
    path = pinned['path']
    if not isinstance(path, str) or not Path(path).is_absolute():
        return None, None, result('FAIL', {'kind': 'invalid-generation-path'})
    generation = Path(path).resolve()
    if not generation.is_dir():
        return None, None, result('ABSENT', {'missing': path})
    return generation, pinned, None


def reading_source_current(run, queue_path=None):
    """Compare current document bytes, not a pinned historical document.

    Freshness.lean:81-88 readingMatches/reason scopes freshness to loop inputs.
    Scope is recorded by the pinned run, NOT recomputed now: input.edn's
    graph-coverage.drift.members-not-open, built by excursions.clj:82-106,
    :130-141 and installed at :157; run.clj:252 writes that input. Its digest
    is checked separately by run_file_digests. Inactive readings are reported,
    never passed; gcc_preview.py withholds stale reading links. Document
    hashes below are taken NOW, independently of the recorded scope.
    Retired members are reported from the pinned inventory, never PASS-counted;
    retired.jsonl is lineage, not a current batch of reading records.
    reread_worker.clj:272-278 requires the exact claim path/hash. Paths are
    absolute from futon2/aif/mission_registry.clj:1164 (also census :623);
    do not resolve relative paths against this process or expand shell syntax.
    The corpus check_edges.py:56-57 hashes Path(p).read_bytes(), checked at
    :121-123 before landing (corpus_generation.clj:158-218). This agrees with
    mission_registry.clj:465-474's raw-byte hash; its census text hash at
    :167-170 is equivalent for UTF-8 documents.
    reread_queue.clj:12-14 defines the default queue path; :95, :193-211 and
    :229-256 distinguish pending/claimed from landed/failed/superseded.
    Open SQLite read-only, never initialize it. Hashes are taken NOW, so results
    may change without producer action. Superseded history lives in older
    generations; scan all batch records in the manifest-pinned generation.
    """
    def result(status, detail):
        return dict(check='reading_source_current',
                    theorem=CHECKS['reading_source_current'], status=status, detail=detail)
    counts = dict(records=0, in_view=0, current=0, pending=0, source_changed_unqueued=0,
                  source_missing=0, inactive_current=0, inactive_changed=0, inactive_missing=0,
                  retired=0, retired_ids=[])
    inactive_ids = {kind: [] for kind in ('current', 'changed', 'missing')}
    counts['inactive_ids'] = inactive_ids
    inactive_unknown = []
    errors, stale = [], []
    try:
        generation, _, error = pinned_generation(run, result)
        if error is not None:
            return error
        try:
            scope = load(Path(run) / 'input.edn', edn=True, strict=True)
            inactive = scope['graph-coverage']['drift']['members-not-open']
            if not isinstance(inactive, list) or not all(isinstance(x, str) for x in inactive):
                raise ValueError('invalid scope')
            inactive = set(inactive)
        except (OSError, ValueError, TypeError, KeyError, IndexError, RuntimeError):
            return result('ABSENT', dict(counts, missing='input.edn graph-coverage.drift.members-not-open'))
        members = load(generation / 'members.json')
        if members is None:
            return result('ABSENT', dict(counts, missing='members.json'))
        if not isinstance(members, list) or not all(isinstance(m, dict) for m in members):
            return result('FAIL', dict(counts, rule='invalid-members'))
        retired = {m['id'] for m in members if m.get('retired')}
        counts['retired'] = len(retired)
        counts['retired_ids'] = sorted(retired)[:20]
        for batch in sorted(generation.rglob('batch-*.jsonl')):
            if not batch.resolve().is_relative_to(generation):
                return result('FAIL', {'rule': 'batch-outside-generation'})
            for line in batch.read_text(encoding='utf-8').splitlines():
                if not line.strip():
                    continue
                counts['records'] += 1
                try:
                    record = json.loads(line)
                except ValueError:
                    errors.append({'item': None, 'rule': 'unparseable-record'})
                    continue
                item = record.get('id') if isinstance(record, dict) else None
                pin = record.get('source') if isinstance(record, dict) else None
                if not isinstance(item, str) or not item.strip():
                    errors.append({'item': None, 'rule': 'missing-item-id'})
                    continue
                # Coverage refuses retired+record contradictions as ambiguous.
                # A retired identity cannot earn a source-freshness PASS here.
                if item in retired:
                    continue
                in_view = item not in inactive
                counts['in_view'] += in_view
                if (not isinstance(pin, dict) or not isinstance(pin.get('path'), str)
                        or not Path(pin['path']).is_absolute()
                        or not isinstance(pin.get('sha256'), str)
                        or not re.fullmatch(r'[0-9a-f]{64}', pin['sha256'])):
                    if in_view:
                        errors.append({'item': item, 'rule': 'no-source-pin'})
                    else:
                        inactive_unknown.append(item)
                    continue
                try:
                    actual = hashlib.sha256(Path(pin['path']).read_bytes()).hexdigest()
                except FileNotFoundError:
                    actual = None
                except OSError:
                    if in_view:
                        raise
                    inactive_unknown.append(item)
                    continue
                if not in_view:
                    kind = ('current' if actual == pin['sha256'] else
                            'missing' if actual is None else 'changed')
                    counts['inactive_' + kind] += 1
                    if len(inactive_ids[kind]) < 20:
                        inactive_ids[kind].append(item)
                    continue
                if actual == pin['sha256']:
                    counts['current'] += 1
                else:
                    counts['source_missing'] += actual is None
                    stale.append((item, actual))
        if not counts['records']:
            return result('ABSENT', dict(counts, missing='reading records'))
        queue_missing = False
        if stale:
            queue_path = Path(queue_path) if queue_path is not None else (
                Path.home() / '.local/state/futon3c/giraffe-reread/requests.sqlite')
            queue_missing = not queue_path.is_file()
            if not queue_missing:
                with closing(sqlite3.connect(queue_path.resolve().as_uri() + '?mode=ro', uri=True)) as db:
                    for item, actual in stale:
                        query = ("SELECT 1 FROM reread_requests WHERE target_id=? "
                                 "AND state IN ('pending','claimed')")
                        args = [item]
                        if actual is not None:
                            query += ' AND source_sha=?'
                            args.append(actual)
                        if db.execute(query, args).fetchone():
                            counts['pending'] += 1
                        else:
                            counts['source_changed_unqueued'] += actual is not None
                            errors.append({'item': item, 'rule': 'source-missing' if actual is None
                                           else 'source-changed'})
        if errors:
            return result('FAIL', dict(counts, violations=errors[:20], total=len(errors)))
        if queue_missing:
            return result('ABSENT', dict(counts, missing='reread queue', unchecked=len(stale)))
        if inactive_unknown:
            return result('ABSENT', dict(counts, missing='inactive source pins or readable documents',
                                        items=inactive_unknown[:20], total=len(inactive_unknown)))
        if not counts['in_view']:
            return result('ABSENT', dict(counts, missing='in-view reading records'))
        return result('PASS', counts)
    except (OSError, ValueError, TypeError, KeyError, IndexError, RuntimeError, sqlite3.Error):
        return result('FAIL', dict(counts, rule='invalid-generation-source-or-queue'))


def one_current_reading(run):
    """Check only the generation pinned by manifest readings-generation.path.

    src/futon3c/giraffe/corpus_generation.clj:75-95 removes every target :id
    from batch-*.jsonl before appending its replacement. Lines 158-218 group
    ALL staged batch records by :id and refuse duplicates, without filtering
    a status field. A generation thus holds current records; superseded
    versions remain in older generations, not alongside their replacements.
    The record's id IS the item id (there is no separate reading-record id).
    No current-pointer fallback; resolve the pinned path once before scanning.
    """
    def result(status, detail):
        return {'check': 'one_current_reading',
                'theorem': CHECKS['one_current_reading'], 'status': status, 'detail': detail}

    try:
        generation, pinned, error = pinned_generation(run, result)
        if error is not None:
            return error
        path = pinned['path']
        batches = sorted(generation.rglob('batch-*.jsonl'))
        if not batches:
            return result('ABSENT', {'missing': 'batch-*.jsonl', 'generation': path})
        grouped, errors, records = {}, [], 0
        for batch in batches:
            if not batch.resolve().is_relative_to(generation):
                return result('FAIL', {'kind': 'batch-outside-generation'})
            with batch.open(encoding='utf-8') as stream:
                for line_number, line in enumerate(stream, 1):
                    if not line.strip():
                        continue
                    records += 1
                    location = {'file': str(batch.relative_to(generation)), 'line': line_number}
                    try:
                        record = json.loads(line)
                    except ValueError:
                        errors.append(dict(location, kind='unparseable-record'))
                        continue
                    item = record.get('id') if isinstance(record, dict) else None
                    if not isinstance(item, str) or not item.strip():
                        errors.append(dict(location, kind='missing-item-id'))
                        continue
                    grouped.setdefault(item, []).append(item)
        duplicates = [{'item': item, 'record_ids': ids}
                      for item, ids in sorted(grouped.items()) if len(ids) > 1]
        if errors or duplicates:
            return result('FAIL', {'duplicates': duplicates, 'errors': errors})
        if records == 0:
            # Batch files with no records verify nothing about OneCurrent.
            return result('ABSENT', {'missing': 'reading records', 'generation': path})
        return result('PASS', {'records': records, 'items': len(grouped)})
    except FileNotFoundError:
        return result('ABSENT', {'missing': 'pinned-generation-or-file'})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError):
        return result('FAIL', {'kind': 'invalid-manifest-or-generation'})


def corpus_generation_digest(run):
    """Match corpus_generation.clj:25-37 generation-digest on this platform.

    Java String.compareTo orders relative Path.toString names by UTF-16 code
    units (not Python Unicode code points): use UTF-16BE bytes as the sort key.
    Hash each relative name's UTF-8 bytes, NUL, then file bytes. Files.walk
    without FOLLOW_LINKS does not descend symlink directories; isRegularFile
    without NOFOLLOW_LINKS DOES include symlinks to regular files, hashing
    target bytes under the symlink's relative name, even for external targets.
    Broken links are not regular files. os.walk(followlinks=False) + is_file
    matches these choices; traversal/read errors are failures, never omissions.
    """
    def result(status, detail):
        return {'check': 'corpus_generation_digest',
                'theorem': CHECKS['corpus_generation_digest'],
                'status': status, 'detail': detail}

    try:
        generation, pinned, error = pinned_generation(run, result)
        if error is not None:
            return error
        recorded = pinned.get('digest')
        if not recorded:
            return result('ABSENT', {'missing': 'readings-generation.digest'})
        if not isinstance(recorded, str) or not re.fullmatch('[0-9a-fA-F]{64}', recorded):
            return result('FAIL', {'kind': 'invalid-generation-digest'})
        def walk_error(error):
            raise error
        files = []
        for directory, _, names in os.walk(generation, followlinks=False, onerror=walk_error):
            for name in names:
                path = Path(directory) / name
                if path.is_file():
                    files.append(path)
        if not files:
            return result('ABSENT', {'missing': 'regular files'})
        files.sort(key=lambda p: str(p.relative_to(generation)).encode('utf-16-be'))
        digest = hashlib.sha256()
        for path in files:
            digest.update(str(path.relative_to(generation)).encode('utf-8'))
            digest.update(b'\0')
            digest.update(path.read_bytes())
        actual = digest.hexdigest()
        dirname = generation.name if re.fullmatch('[0-9a-fA-F]{64}', generation.name) else None
        if actual != recorded or (dirname is not None and actual != dirname):
            return result('FAIL', {'recorded': recorded, 'actual': actual, 'directory': dirname})
        return result('PASS', {'files': len(files)})
    except FileNotFoundError:
        return result('ABSENT', {'missing': 'pinned-generation-or-file'})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError):
        return result('FAIL', {'kind': 'invalid-manifest-or-generation'})


def load(path, edn=False, strict=False):
    if not path or not Path(path).is_file():
        return None
    text = Path(path).read_text(encoding='utf-8')
    if not edn:
        return json.loads(text)
    # Reuse only the existing stdlib EDN decoder, never producer/check logic.
    spec = importlib.util.spec_from_file_location('gcc_edn', Path(__file__).with_name('gcc_preview.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    if strict:
        reader = module._EdnReader(text)
        value = reader.read()
        if reader.pos != len(reader.tokens):
            raise ValueError('trailing EDN input')
        return value
    return module.edn_load(text)


def write_report(path, party, run, results):
    report = {'generated_at': (party or {}).get('generated_at'), 'run': run,
              'checked_at_ms': int(time.time() * 1000),
              'summary': {status: sum(r['status'] == status for r in results)
                          for status in ('PASS', 'FAIL', 'ABSENT')},
              'failing': [r['check'] for r in results if r['status'] == 'FAIL'],
              'results': results}
    path = Path(path)
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(mode='w', encoding='utf-8', dir=path.parent,
                                         prefix='.' + path.name, delete=False) as stream:
            temporary = stream.name
            json.dump(report, stream, ensure_ascii=False)
            stream.write('\n')
        os.replace(temporary, path)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('party', type=Path, nargs='?',
                        default=Path.home() / 'storage/gcc-preview/party.json')
    parser.add_argument('--run', type=Path)
    parser.add_argument('--pending', type=Path)
    parser.add_argument('--lexicon', type=Path)
    parser.add_argument('--json-out', type=Path)
    args = parser.parse_args(argv)
    party, run = None, None
    try:
        party = load(args.party)
        if party is None:
            results = [{'check': k, 'theorem': t, 'status': 'ABSENT',
                        'detail': {'kind': 'missing-party', 'path': str(args.party)}}
                       for k, t in CHECKS.items()]
        else:
            sources = party['sources']
            run = args.run or Path.home() / 'storage/giraffe' / sources['giraffe_run']
            if run.name != sources['giraffe_run']:
                raise ValueError('run-identity-mismatch: ' + str(run))
            pending = args.pending or sources.get('pending_index')
            lexicon = args.lexicon or sources.get('lexicon', '').split(' (convergence')[0]
            results = audit(party, load(run / 'pass.edn', edn=True), load(pending), load(lexicon), run=run)
            print('generated_at:', party.get('generated_at'), 'run:', run.name)
            print('sources:', json.dumps({'pending': str(pending), 'lexicon': str(lexicon)}))
            print('Scope: current source values; party.json does not retain source digests.')
        print('CHECK                        RESULT  LEAN')
        for row in results:
            print(f"{row['check']:28} {row['status']:7} {row['theorem']}")
            if row['detail']:
                print(json.dumps(row['detail'], ensure_ascii=False, sort_keys=True))
        if args.json_out:
            write_report(args.json_out, party, run.name if run else None, results)
        return int(any(row['status'] == 'FAIL' for row in results))
    except (OSError, IndexError, ValueError, KeyError, TypeError) as exc:
        print('FAIL invalid-input:', str(exc))
        if args.json_out:
            write_report(args.json_out, party if isinstance(party, dict) else None,
                         run.name if isinstance(run, Path) else None,
                         [{'check': 'invalid-input', 'theorem': '', 'status': 'FAIL',
                           'detail': {'error': str(exc)}}])
        return 1


if __name__ == '__main__':
    sys.dont_write_bytecode = True
    raise SystemExit(main())
