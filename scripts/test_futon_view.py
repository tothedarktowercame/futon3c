import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import futon_view as fv  # noqa: E402

JOB = {"caller": "claude-3", "started-at": "2026-10-19T02:08:11.123456789Z", "state": "done",
       "events": [{"type": "accepted"},
                  {"type": "prompt", "text": "\nFrom claude-3 (Joe's go, 2026-10-19): one small job.\nmore"}]}


class WithJobs(unittest.TestCase):
    def agents(self, card_at):
        return {"zai-1": {"purpose": "Prove t95J03_torus_bridge", "purpose_at": card_at}}

    def test_job_line_drops_from_prefix(self):
        j = fv.job_line(JOB)
        self.assertEqual(j, {"by": "claude-3", "at": JOB["started-at"], "state": "done", "line": "one small job."})

    def test_job_line_drops_from_prefix_ending_in_full_stop(self):
        job = dict(JOB, events=[{"type": "prompt", "text": "From claude-3 (Joe's go, 2026-10-19). The stepper shows it."}])
        self.assertEqual(fv.job_line(job)["line"], "The stepper shows it.")

    def test_card_older_than_job_loses_purpose(self):
        v = fv.with_jobs(self.agents("2026-10-17T20:00:00.000000000Z"), lambda a: JOB)["zai-1"]
        self.assertIsNone(v["purpose"])
        self.assertEqual(v["job"]["by"], "claude-3")
        self.assertNotIn("purpose_at", v)

    def test_card_newer_than_job_keeps_purpose(self):
        v = fv.with_jobs(self.agents("2026-10-19T02:20:00.000000000Z"), lambda a: JOB)["zai-1"]
        self.assertEqual(v["purpose"], "Prove t95J03_torus_bridge")

    def test_unknown_card_time_loses_purpose(self):
        v = fv.with_jobs(self.agents(None), lambda a: JOB)["zai-1"]
        self.assertIsNone(v["purpose"])

    def test_no_job_keeps_purpose(self):
        v = fv.with_jobs(self.agents(None), lambda a: None)["zai-1"]
        self.assertEqual(v["purpose"], "Prove t95J03_torus_bridge")
        self.assertNotIn("job", v)


class PurposeSince(unittest.TestCase):
    @staticmethod
    def card(text):
        return {"card": {"standing": {"purpose": {"text": text}}}}

    def test_carried_purpose_dates_from_its_first_card(self):
        cards = [("1", self.card("old")), ("2", self.card("torus")), ("3", self.card("torus")), ("4", self.card("torus"))]
        self.assertEqual(fv.purpose_since(cards), "2")

    def test_new_purpose_dates_from_newest_card(self):
        cards = [("1", self.card("torus")), ("2", self.card("torus")), ("3", self.card("new job"))]
        self.assertEqual(fv.purpose_since(cards), "3")

    def test_no_cards(self):
        self.assertIsNone(fv.purpose_since([]))


class CurrentJobTest(unittest.TestCase):
    def test_skips_auto_bellback(self):
        jobs = [{"caller": "auto-bellback", "job-id": "a"}, {"caller": "claude-4", "job-id": "b"}]
        self.assertEqual(fv.current_job(jobs)["job-id"], "b")

    def test_only_bellbacks(self):
        self.assertIsNone(fv.current_job([{"caller": "auto-bellback"}]))


if __name__ == "__main__":
    unittest.main()
