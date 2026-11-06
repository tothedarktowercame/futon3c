#!/usr/bin/env python3
"""Bad-case tests for the selection-reason probe (selection-gives-a-reason way).

Invariant per claude-6 (M-ways-of-working, 2026-11-06), against the selection
records the serving JVM wrote (never recomputed):
  (a) an indexed click's selection record exists;
  (b) a chosen outcome names a target and another cascade's pick;
  (c) an abstention carries a typed :case.
The bad cases below construct each violation exactly, plus the two passing
fixtures claude-6 named (abstained ca5cf289 shape; chosen with runner-up).
"""
import datetime
import importlib.util
import pathlib
import tempfile
import unittest

REPO = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = REPO / "scripts" / "ways-of-working-probe.py"

NOW = datetime.datetime(2026, 11, 6, 18, 0, 0, tzinfo=datetime.timezone.utc)


def load():
    spec = importlib.util.spec_from_file_location("wow_probe", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def entry(run, path, started="2026-11-06T09:39:00Z"):
    return {"run": run, "started-at": started, "path": str(path)}


ABSTAINED = ('{:schema :wm/selection-record-v1 :outcome {:status :abstained, '
             ':case :insufficient-comparison} :outer/selection {:selection nil '
             ':cascade-picks ["E-queries-and-scopes" :no-pick "E-queries-and-scopes" :no-pick]}}')
CHOSEN = ('{:schema :wm/selection-record-v1 :outcome {:status :chosen} '
          ':outer/selection {:selection {:selected {:target "M-action-cost-modelling"}} '
          ':cascade-picks ["E-comprehension-foundation" "M-action-cost-modelling" '
          '"E-comprehension-foundation" :no-pick] :why {:schema :wm/selection-why-v1}}}')


class ClassifySelectionRecordsTest(unittest.TestCase):
    def setUp(self):
        self.mod = load()
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = pathlib.Path(self.tmp.name)
        self.addCleanup(self.tmp.cleanup)

    def write(self, name, text):
        p = self.dir / name
        p.write_text(text, encoding="utf-8")
        return p

    def test_bad_case_a_missing_record(self):
        # a click whose indexed selection record does not exist
        checked, rows = self.mod.classify_selection_records(
            [entry("run-missing", self.dir / "absent.edn")], now=NOW)
        self.assertEqual(checked, 1)
        self.assertEqual([r["defect"] for r in rows], ["no-selection-record"])

    def test_bad_case_c_abstention_without_case(self):
        p = self.write("no-case.edn", ABSTAINED.replace(", :case :insufficient-comparison", ""))
        checked, rows = self.mod.classify_selection_records([entry("run-no-case", p)], now=NOW)
        self.assertEqual([r["defect"] for r in rows], ["abstained-without-reason"])

    def test_bad_case_b_chosen_with_no_distinct_runner_up(self):
        # claude-6 review 2026-11-06: two cascades picking the SAME target is
        # a collapsed one-option field, not a runner-up — the 10:4x loop's
        # clicks 2 and 3 today. The old pick-counting check let this pass.
        p = self.write("collapsed.edn",
                       CHOSEN.replace(':cascade-picks ["E-comprehension-foundation" "M-action-cost-modelling" '
                                      '"E-comprehension-foundation" :no-pick]',
                                      ':cascade-picks ["E-queries-and-scopes" "E-queries-and-scopes" :no-pick :no-pick]'))
        checked, rows = self.mod.classify_selection_records([entry("run-collapsed", p)], now=NOW)
        self.assertEqual([r["defect"] for r in rows], ["no-distinct-runner-up"])

    def test_bad_case_b_chosen_with_no_runner_up(self):
        # chosen, target named, but only one cascade picked and no typed absence
        p = self.write("one-pick.edn",
                       CHOSEN.replace(':cascade-picks ["E-comprehension-foundation" "M-action-cost-modelling" '
                                      '"E-comprehension-foundation" :no-pick]',
                                      ':cascade-picks [:no-pick "M-action-cost-modelling" :no-pick :no-pick]'))
        checked, rows = self.mod.classify_selection_records([entry("run-one-pick", p)], now=NOW)
        self.assertEqual([r["defect"] for r in rows], ["no-distinct-runner-up"])

    def test_bad_case_b_chosen_without_target(self):
        p = self.write("no-target.edn", CHOSEN.replace(':selected {:target "M-action-cost-modelling"}', "nil"))
        checked, rows = self.mod.classify_selection_records([entry("run-no-target", p)], now=NOW)
        self.assertEqual([r["defect"] for r in rows], ["chosen-without-target"])

    def test_passing_abstention_with_typed_case(self):
        p = self.write("abstained.edn", ABSTAINED)
        checked, rows = self.mod.classify_selection_records([entry("run-abstain", p)], now=NOW)
        self.assertEqual((checked, rows), (1, []))

    def test_passing_chosen_with_runner_up(self):
        p = self.write("chosen.edn", CHOSEN)
        checked, rows = self.mod.classify_selection_records([entry("run-chosen", p)], now=NOW)
        self.assertEqual((checked, rows), (1, []))

    def test_stale_click_out_of_population(self):
        p = self.write("old.edn", ABSTAINED)
        checked, rows = self.mod.classify_selection_records(
            [entry("run-old", p, started="2026-10-01T00:00:00Z")], now=NOW)
        self.assertEqual((checked, rows), (0, []))

    def test_index_reader_and_probe_end_to_end(self):
        # index shape as the serving JVM writes it; one good click, one whose
        # file is absent -> probe on/failing naming the absent run
        good = self.write("good.edn", ABSTAINED)
        idx = self.dir / "selection-record-index.edn"
        idx.write_text('{:run/id "run-good" :startedAt "2026-11-06T09:39:00Z" :chain-index 1 '
                       ':reference {:path "%s" :sha256 "abc"}}\n'
                       '{:run/id "run-missing" :startedAt "2026-11-06T10:00:00Z" :chain-index 1 '
                       ':reference {:path "%s" :sha256 "def"}}\n' % (good, self.dir / "absent.edn"), encoding="utf-8")
        entries = self.mod.read_selection_index(root=self.dir)
        self.assertEqual(len(entries), 2)
        live = self.mod.probe_selection_reason(now=NOW, root=self.dir)
        self.assertEqual(live["monitor"], "on")
        self.assertEqual(live["invariant"], "failing")
        self.assertEqual(live["violations"][0]["run"], "run-missing")
        self.assertIn("of 2 clicks", live["denominator"])

    def test_missing_index_reads_broken(self):
        live = self.mod.probe_selection_reason(now=NOW, root=self.dir / "nowhere")
        self.assertEqual(live["monitor"], "broken")
        self.assertEqual(live["invariant"], "unknown")

    def test_legacy_refused_without_kind_is_flagged(self):
        # pre-2026-11-06 records carry :selection-outcome, typed by :kind
        p = self.write("legacy-refused.edn",
                       '{:schema :wm/selection-record-v1 :selection-outcome {:status :refused}}')
        checked, rows = self.mod.classify_selection_records([entry("run-legacy", p)], now=NOW)
        self.assertEqual([r["defect"] for r in rows], ["refused-without-reason"])

    def test_legacy_refused_with_kind_passes(self):
        p = self.write("legacy-ok.edn",
                       '{:schema :wm/selection-record-v1 :selection-outcome {:status :refused, :kind :makeup-changed}}')
        checked, rows = self.mod.classify_selection_records([entry("run-legacy-ok", p)], now=NOW)
        self.assertEqual((checked, rows), (1, []))


if __name__ == "__main__":
    unittest.main()
