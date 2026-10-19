"""Tests for ma_observe.py's card logic (no network, no model)."""
import datetime
import os
import sys
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ma_observe as ma  # noqa: E402


class CardTest(unittest.TestCase):
    def test_parse_card_reads_tagged_json_and_fenced_json(self):
        raw = '<observations>x</observations>\n<agent-card>```json\n{"purpose": "p", "repair": ["r"], "extra": 1}\n```</agent-card>'
        card = ma.parse_card(raw)
        self.assertEqual(card["purpose"], "p")
        self.assertEqual(card["repair"], ["r"])
        self.assertNotIn("extra", card)
        self.assertIsNone(ma.parse_card("<agent-card>not json</agent-card>"))
        self.assertIsNone(ma.parse_card("no card"))

    def test_done_lines_take_the_marker_not_a_mention(self):
        obs = "* ✅ (1) a\n  * ✅ b\n* 🟡 (2) the ✅ marker means done\n> tiers: ✅ work"
        self.assertEqual(ma.done_lines(obs), ["✅ (1) a", "✅ b"])

    def test_held_counts_verbatim_carries(self):
        first = ma.held(["a", "b"], None)
        second = ma.held(["a", "c"], first)
        self.assertEqual(second, [{"text": "a", "held": 2}, {"text": "c", "held": 1}])

    def test_build_card_tiers(self):
        rows = [{"evidence/body": {"event": "observation", "observations": "* ✅ (1) old done\n* 🟡 x"}},
                {"evidence/body": {"event": "turn-commits", "commits": [{"sha": "abcdef1234", "subject": "s"}]}}]
        with mock.patch.object(ma, "xiang_patterns", return_value={"p/a": 3, "p/b": 1}), \
             mock.patch.object(ma, "clock_of", return_value={"excursion-id": "E-x"}), \
             mock.patch.object(ma, "mission_patterns", return_value=["m/p"]):
            card = ma.build_card("claude-3", "s", rows, "* ✅ (2) new done\n* 🔴 (2) broke",
                                 {"purpose": "p", "open_threads": ["t"], "repair": [], "ramifications": []},
                                 {"standing": {"purpose": {"text": "p", "held": 2}, "open_threads": [{"text": "t", "held": 4}]}})
        self.assertEqual(card["settled"]["engaged"], {"excursion-id": "E-x"})
        self.assertEqual(card["settled"]["done"], ["✅ (1) old done"])
        self.assertEqual(card["settled"]["commits"]["last"], ["abcdef123 s"])
        self.assertEqual(card["standing"]["purpose"], {"text": "p", "held": 3})
        self.assertEqual(card["standing"]["open_threads"], [{"text": "t", "held": 5}])
        self.assertEqual(card["standing"]["xiang_patterns"], [["p/a", 3]])
        self.assertEqual(card["fresh"]["done"], ["✅ (2) new done"])
        self.assertEqual(card["fresh"]["problems"], ["🔴 (2) broke"])

    def test_system_prompt_appends_card_instruction_in_mastras_slot(self):
        sp = ma.system_prompt()
        self.assertTrue(sp.startswith("You are the memory consciousness"))
        self.assertIn("=== CUSTOM INSTRUCTIONS ===", sp)
        self.assertIn("<agent-card>", sp)


# The exact bad cases from zai-5's t95J03 run, 2026-10-17.
S2_PACKET = ("--- CURRENT TURN ---\nSurface: bell\nFrom: zai-5\n---\n\nFrom zai-5: prove ONE lemma.\n\n"
             "THIS JOB: P = t95J03, NAME = t95J03_torus_bridge (leaf S2, LIBRARY-sized).")
S2_TRUNCATED_REPLY = ("For injectivity the essential facts: phi(z) determines z. ... "
                      "z = (rho^2/l^2)(x',y') where ... wait |(x',y')| = rho^2/l. Hmm no:")



class WorkRecordTest(unittest.TestCase):
    KNOWN = ["E-prove-memory-useful", "close the stepper gap", "fix the prompt race"]
    TEXT = ("Please keep going with the work.\nThe stepper window\nis stuck at frame 34.")

    def test_known_work_dedupes_and_includes_ids_purpose_threads_repair(self):
        prev = {"standing": {"purpose": {"text": "close the stepper gap"},
                             "open_threads": [{"text": "awaiting Joe on quota"}, {"text": ""}],
                             "repair": [{"text": "awaiting Joe on quota"}]}}
        self.assertEqual(ma.known_work({"excursion-id": "E-prove-memory-useful"}, prev),
                         ["close the stepper gap", "awaiting Joe on quota", "E-prove-memory-useful"])

    def test_check_work_accepts_exact_item_and_verbatim_quote(self):
        r = ma.check_work({"item": "close the stepper gap",
                           "quote": "The stepper window"}, self.KNOWN, self.TEXT)
        self.assertEqual(r, {"item": "close the stepper gap", "quote": "The stepper window"})

    def test_check_work_rejects_paraphrased_item(self):
        # one word changed: not exactly on the card
        r = ma.check_work({"item": "close the stepper gaps", "quote": "stuck at frame 34"},
                          self.KNOWN, self.TEXT)
        self.assertEqual(r["rejected"], "item not on card")

    def test_check_work_rejects_quote_with_one_word_changed(self):
        r = ma.check_work({"item": "close the stepper gap", "quote": "stuck at frame 35"},
                          self.KNOWN, self.TEXT)
        self.assertEqual(r["rejected"], "quote not in turn")

    def test_check_work_accepts_line_break_for_space(self):
        r = ma.check_work({"item": "close the stepper gap",
                           "quote": "The stepper window is stuck"}, self.KNOWN, self.TEXT)
        self.assertIn("item", r)

    def test_check_work_accepts_none_with_reason_and_rejects_bad_records(self):
        self.assertEqual(ma.check_work({"none": "general chat"}, self.KNOWN, self.TEXT),
                         {"none": "general chat"})
        self.assertEqual(ma.check_work({"none": ""}, self.KNOWN, self.TEXT)["rejected"], "no work record")
        self.assertEqual(ma.check_work({}, self.KNOWN, self.TEXT)["rejected"], "no work record")
        self.assertEqual(ma.check_work("work", self.KNOWN, self.TEXT)["rejected"], "no work record")
        self.assertEqual(ma.check_work({"item": "close the stepper gap"}, self.KNOWN, self.TEXT)
                         ["rejected"], "quote not in turn")

    def test_parse_card_keeps_work(self):
        raw = '<agent-card>{"purpose": "p", "work": {"item": "x", "quote": "y"}}</agent-card>'
        self.assertEqual(ma.parse_card(raw)["work"], {"item": "x", "quote": "y"})

    def test_build_card_puts_checked_work_in_fresh(self):
        rows = [{"evidence/body": {"event": "observation", "observations": ""}}]
        with mock.patch.object(ma, "xiang_patterns", return_value={}), \
             mock.patch.object(ma, "clock_of", return_value={}), \
             mock.patch.object(ma, "mission_patterns", return_value=[]):
            card = ma.build_card("a", "s", rows, "", None, None,
                                 work={"item": "close the stepper gap", "quote": "The stepper window"})
        self.assertEqual(card["fresh"]["work"],
                         {"item": "close the stepper gap", "quote": "The stepper window"})


class HarnessTest(unittest.TestCase):
    def test_truncated_s2_episode_has_no_verdict(self):
        self.assertIsNone(ma.verdict(S2_TRUNCATED_REPLY))
        self.assertIsNone(ma.verdict("once this is PROVED we can splice ... not yet PROVED"))
        self.assertEqual(ma.verdict("...\nPROVED. area 86 lines, 5 failed compiles"), "proved")
        self.assertEqual(ma.verdict("PARTIAL: remaining goal injectivity"), "partial")

    def test_leaf_and_progress_lines_carry_the_lemma_name_first(self):
        packet = ma.packet_body(S2_PACKET)
        self.assertEqual(ma.leaf_of(packet), ("t95J03", "t95J03_torus_bridge"))
        obs = ("Date: Oct 17, 2026\n"
               "* 🔴 (9:02 PM) Task: prove lemma t95J03_torus_bridge — final reply must state PROVED/PARTIAL\n"
               "* 🟡 (21:04) route: l1 chart c(z)=|z|n(arg z)\n* 🔴 remaining: injectivity")
        lines = ma.progress_lines("t95J03", "t95J03_torus_bridge", "zai-1", "invoke-1", "without a verdict", obs, "2026-10-17T21:30Z")
        self.assertTrue(lines[0].startswith("t95J03 leaf (t95J03_torus_bridge) progress"))
        self.assertEqual(lines[1:], ["🟡 (21:04) route: l1 chart c(z)=|z|n(arg z)", "🔴 remaining: injectivity"])
        self.assertFalse(any("Task" in l or "\n" in l for l in lines))
        self.assertEqual(ma.memory_name("t95J03_torus_bridge"), "t95j03-torus-bridge-progress")

    def test_name_slot_finds_the_promoted_line_for_the_next_packet(self):
        sys.path.insert(0, os.path.expanduser("~/code/minus40/scripts"))
        import inject
        head = ma.progress_lines("t95J03", "t95J03_torus_bridge", "zai-1", "invoke-1", "without a verdict", "* 🟡 route", "t")[0]
        facts = [{"memory": "cc:ma/t95j03-torus-bridge-progress", "text": head},
                 {"memory": "cc:zai-1/t95j03-c5", "text": "t95J03 leaf C5 (isKSheeted_comp_homeomorph)"}]
        self.assertEqual(inject.name_hits(ma.packet_body(S2_PACKET), facts),
                         {"cc:ma/t95j03-torus-bridge-progress": "t95J03_torus_bridge"})

    def test_continuation_keeps_lineage_and_stops_after_max(self):
        inner = ma.packet_body(S2_PACKET)
        cont = ma.continuation_packet(inner, "invoke-root", 2, "t95j03-torus-bridge-progress")
        m = ma.CONT_HEAD.match(cont)
        self.assertEqual((m.group(1), m.group(2)), ("2", "invoke-root"))  # not "invoke-root." (the full stop)
        self.assertEqual(ma.leaf_of(cont.split("\n\n", 1)[1]), ("t95J03", "t95J03_torus_bridge"))
        turn = {"evidence/at": "2026-10-17T21:00:00Z", "evidence/type": "coordination",
                "evidence/body": {"event": "turn-start", "dispatch-id": "invoke-4",
                                  "prompt": ma.continuation_packet(inner, "invoke-root", 1 + ma.MAX_CONTINUE, "m")}}
        with mock.patch.object(ma, "job_of", return_value={"state": "done", "result": S2_TRUNCATED_REPLY}), \
             mock.patch.object(ma, "continue_agents", return_value={"zai-1"}), \
             mock.patch.object(ma, "send_bell") as bell:
            out = ma.after_harness_turn("zai-1", "s", turn, "* 🟡 x", [turn], log=lambda *_: None, dry_run=True)
        self.assertEqual(out["promoted"], "cc:ma/t95j03-torus-bridge-progress")
        self.assertIsNone(out["continued"])
        bell.assert_not_called()

    def test_turn_from_before_enabling_is_not_continued(self):
        inner = ma.packet_body(S2_PACKET)
        turn = {"evidence/at": "2026-10-17T21:00:00Z", "evidence/type": "coordination",
                "evidence/body": {"event": "turn-start", "dispatch-id": "j", "prompt": S2_PACKET}}
        recent = (datetime.datetime.utcnow()).strftime("%Y-%m-%dT%H:%M:%SZ")
        turn["evidence/at"] = recent
        with mock.patch.object(ma, "job_of", return_value={"state": "done", "result": S2_TRUNCATED_REPLY, "finished-at": recent}), \
             mock.patch.object(ma, "continue_agents", return_value={"zai-1"}), \
             mock.patch.object(ma, "continue_enabled_at", return_value="2099-01-01T00:00:00"):
            out = ma.after_harness_turn("zai-1", "s", turn, "* 🟡 x", [turn], log=lambda *_: None, dry_run=True)
        self.assertIsNone(out["continued"])
        with mock.patch.object(ma, "job_of", return_value={"state": "done", "result": S2_TRUNCATED_REPLY, "finished-at": recent}), \
             mock.patch.object(ma, "continue_agents", return_value={"zai-1"}), \
             mock.patch.object(ma, "continue_enabled_at", return_value="2000-01-01T00:00:00"):
            out = ma.after_harness_turn("zai-1", "s", turn, "* 🟡 x", [turn], log=lambda *_: None, dry_run=True)
        self.assertEqual(out["continued"]["episode"], 2)
        self.assertEqual(out["continued"]["root"], "j")

    def test_verdict_comes_from_the_final_round_before_the_job_is_done(self):
        # :turn-final fires when the final round row is stored, before the
        # Agency marks the job done: the job record is still empty then.
        turn = {"evidence/at": "2026-10-17T22:00:00Z", "evidence/type": "coordination",
                "evidence/body": {"event": "turn-start", "dispatch-id": "j", "turn-id": "zt1", "prompt": S2_PACKET}}
        final = {"evidence/at": "2026-10-17T22:05:00Z", "evidence/type": "coordination",
                 "evidence/body": {"event": "turn-round", "turn-id": "zt1", "final": True, "text": S2_TRUNCATED_REPLY}}
        with mock.patch.object(ma, "job_of", return_value={}):
            out = ma.after_harness_turn("zai-1", "s", turn, "* 🟡 x", [turn, final], log=lambda *_: None, dry_run=True)
        self.assertIsNone(out["verdict"])
        self.assertEqual(out["promoted"], "cc:ma/t95j03-torus-bridge-progress")
        self.assertIs(ma.final_round([turn, final], turn, "2026-10-17T22:00:00Z", None), final)

    def test_proved_turn_is_neither_promoted_nor_continued(self):
        turn = {"evidence/at": "t", "evidence/body": {"event": "turn-start", "dispatch-id": "j", "prompt": S2_PACKET}}
        with mock.patch.object(ma, "job_of", return_value={"state": "done", "result": "PROVED, 40 lines"}):
            out = ma.after_harness_turn("zai-1", "s", turn, "* ✅", [turn], log=lambda *_: None, dry_run=True)
        self.assertEqual((out["promoted"], out["continued"]), (None, None))

    def test_new_packet_does_not_inherit_the_last_purpose(self):
        prev = {"standing": {"purpose": {"text": "Prove t95J03_torus_bridge", "held": 5},
                             "packet_vs_purpose": "", "open_threads": [{"text": "await review", "held": 1}]}}
        def start(prompt):
            return {"evidence/type": "coordination",
                    "evidence/body": {"event": "turn-start", "dispatch-id": "invoke-9", "prompt": prompt}}
        new = ma.carried(prev, start(S2_PACKET.replace("t95J03_torus_bridge", "t95J03_other")))
        self.assertNotIn("Prove t95J03_torus_bridge", ma.known_work({}, new))
        self.assertIn("await review", ma.known_work({}, new))
        with mock.patch.object(ma, "xiang_patterns", return_value={}), \
             mock.patch.object(ma, "clock_of", return_value={}):
            card = ma.build_card("zai-1", "s", [], "", None, new)   # model card failed to parse
        self.assertEqual(card["standing"]["purpose"], {"text": "", "held": 1})
        cont = start(ma.continuation_packet(ma.packet_body(S2_PACKET), "invoke-root", 2, "m"))
        self.assertIs(ma.carried(prev, cont), prev)
        operator = {"evidence/type": "chat-turn", "evidence/body": {"role": "user", "text": "go on"}}
        self.assertIs(ma.carried(prev, operator), prev)

    def test_harness_prompt_forbids_restating_the_packet(self):
        self.assertIn("Do NOT restate it", ma.system_prompt(harness=True))
        self.assertNotIn("Do NOT restate it", ma.system_prompt())


if __name__ == "__main__":
    unittest.main()
