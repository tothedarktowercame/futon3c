"""draw-pile probe tests: punched machine tickets + anchored closure regex.

Joe's rulings 2026-11-05 (via claude-16 fix handoff):
- T-repair-occ DONE with a repair-store witness citing a dismissal or a
  landing sha is punched (completed, not drawable).
- E-kimi-task files that existed by KIMI_AMNESTY_CUTOFF are punched
  (dated amnesty); later ones only with their own "DONE — confirmed"
  status line.
- "Status: open. Diagnosis complete" must stay OPEN (anchored closure).
"""
import datetime
import importlib.util
import json, os, pathlib, subprocess, sys, tempfile, unittest

SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location(
    "ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
wow = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(wow)
probe_draw_pile = wow.probe_draw_pile

OCC_SHA = "a" * 64
OCC_NOLINKS = "b" * 64
OCC_OPEN = "c" * 64
SHA = "0" * 40


class DrawPilePunchedTest(unittest.TestCase):
    def setUp(self):
        self.td = pathlib.Path(tempfile.mkdtemp(prefix="wow-draw-pile-"))
        self.repo = self.td / "futon2"
        for d in ("holes/tickets", "holes/excursions", "holes/missions"):
            (self.repo / d).mkdir(parents=True)
        subprocess.run(["git", "init", "-q"], cwd=self.repo, check=True)
        subprocess.run(["git", "config", "user.email", "t@t"], cwd=self.repo, check=True)
        subprocess.run(["git", "config", "user.name", "t"], cwd=self.repo, check=True)
        # repair store
        self.store = self.td / "wm-repair-obligations"
        (self.store / "ticket-links").mkdir(parents=True)
        (self.store / "dismissals").mkdir()
        (self.store / "ticket-links" / f"repair-occ-{OCC_SHA}.edn").write_text("{}\n")
        (self.store / "ticket-links" / f"repair-occ-{OCC_OPEN}.edn").write_text("{}\n")
        # commons-map placements + ant-reads (one ant row so committed=True)
        self.cm = self.td / "commons-map"
        self.cm.mkdir()
        (self.cm / "placements-1.tsv").write_text("id\trepo\tstatus\nM-placed\tfuton2\tlive\n")
        self.ar = self.td / "ant-reads"
        self.ar.mkdir()
        (self.ar / "X.A1.json").write_text(json.dumps(
            {"row": {"stem": "M-placed", "what": {"value": "x"}, "status": {"value": "y"},
                     "way": {"value": "z", "cite": "ways-of-working/backlog-understood"}}}))

    def _commit(self, when):
        env = dict(os.environ, GIT_AUTHOR_DATE=when, GIT_COMMITTER_DATE=when)
        subprocess.run(["git", "add", "-A"], cwd=self.repo, check=True, env=env,
                       capture_output=True)
        subprocess.run(["git", "commit", "-q", "-m", when], cwd=self.repo, check=True,
                       env=env, capture_output=True)

    def _write(self, rel, text):
        p = self.repo / rel
        p.write_text(text)
        return p

    def _graph(self):
        items = []
        for p in sorted(self.repo.rglob("*.md")):
            items.append({"stem": p.stem, "kind": "ticket" if "tickets" in str(p) else "excursion",
                          "repo": "futon2", "path": str(p)})
        return {"tickets": {"items": items, "count-total": len(items)}}

    def _probe(self):
        return probe_draw_pile(self._graph(), commons_map_dir=self.cm,
                               ant_reads_dir=self.ar, repair_store_dir=self.store)

    def _punch_line(self, r):
        return next(e for e in r["evidence"] if e.startswith("punched"))

    def test_done_repair_with_witness_is_punched(self):
        self._write(f"holes/tickets/T-repair-occ-{OCC_SHA}.md",
                    f"# t\n\n**Status:** DONE\n\nLanded in `{SHA}`.\n")
        self._commit("2026-11-05T01:00:00+00:00")
        r = self._probe()
        self.assertIn("repair-witness: 1", self._punch_line(r))

    def test_done_repair_without_witness_is_not_punched(self):
        self._write(f"holes/tickets/T-repair-occ-{OCC_NOLINKS}.md",
                    "# t\n\n**Status:** DONE\n\nNo store row, no sha.\n")
        self._commit("2026-11-05T01:00:00+00:00")
        r = self._probe()
        self.assertIn("repair-witness: 0", self._punch_line(r))
        # it stays a drawable closed-unattested item, not a violation-free punch
        self.assertEqual(r["violations"][0]["unplaced-closed-unattested"], 1)

    def test_open_repair_ticket_stays_drawable(self):
        self._write(f"holes/tickets/T-repair-occ-{OCC_OPEN}.md",
                    "# t\n\n**Status:** OPEN\n\n")
        self._commit("2026-11-05T01:00:00+00:00")
        r = self._probe()
        self.assertIn("repair-witness: 0", self._punch_line(r))
        self.assertEqual(r["violations"][0]["unplaced-open"], 1)

    def test_old_no_status_kimi_file_is_punched_by_amnesty(self):
        self._write("holes/excursions/E-kimi-task-old.md", "# packet\n\n## Packet\nbody\n")
        self._commit("2026-11-04T12:00:00+00:00")  # before the cutoff
        r = self._probe()
        self.assertIn("kimi-amnesty-2026-11-05: 1", self._punch_line(r))

    def test_new_no_status_kimi_file_is_not_punched(self):
        self._write("holes/excursions/E-kimi-task-new.md", "# packet\n\n## Packet\nbody\n")
        self._commit("2026-11-05T01:00:00+00:00")  # after the cutoff
        r = self._probe()
        self.assertIn("kimi-amnesty-2026-11-05: 0", self._punch_line(r))
        self.assertEqual(r["violations"][0]["unplaced-open"], 1)

    def test_new_confirmed_kimi_file_is_punched_by_its_own_line(self):
        self._write("holes/excursions/E-kimi-task-newdone.md",
                    "# packet\n\n**Status:** DONE — confirmed 2026-11-05 (Giraffe closing check): it landed\n")
        self._commit("2026-11-05T01:00:00+00:00")
        r = self._probe()
        self.assertIn("kimi-confirmed: 1", self._punch_line(r))

    def test_midline_complete_stays_open(self):
        self._write("holes/tickets/T-midline.md",
                    "# t\n\n**Status:** open. Diagnosis complete and reduced.\n")
        self._commit("2026-11-05T01:00:00+00:00")
        r = self._probe()
        self.assertEqual(r["violations"][0], {"unplaced-open": 1, "unplaced-closed-unattested": 0})


if __name__ == "__main__":
    unittest.main()
