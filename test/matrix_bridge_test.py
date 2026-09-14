"""Offline transport contract: real routing/queue, stubbed HTTP boundaries."""
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import time
import unittest
from unittest.mock import patch
from urllib.parse import unquote, urlsplit, parse_qs

SPEC = importlib.util.spec_from_file_location(
    'matrix_bridge_test_subject', Path(__file__).resolve().parents[1] / 'scripts/matrix_bridge.py')
m = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(m)
ROOM = '!work:matrix.paragogy.net'
OTHER = '!other:remote.net'
SENDER = '@rob:remote.net'


def event(eid='$one', body='@codex hello', sender=SENDER, kind='m.room.message'):
    return dict(event_id=eid, sender=sender, type=kind, content={'msgtype': 'm.text', 'body': body})


def batch(token, events=(), room=ROOM, invites=()):
    return {'next_batch': token, 'rooms': {
        'join': {room: {'timeline': {'events': list(events)}}},
        'invite': {r: {} for r in invites}}}


class FakeHTTP:
    def __init__(self):
        self.calls, self.posts, self.invokes, self.announces = [], {}, [], []
        self.syncs = []
        self.reply = 'answer'
        self.fail_send_once = False

    def open(self, req, timeout=None):
        self.calls.append(req)
        assert req.get_header('Authorization') == 'Bearer offline-token'
        path = unquote(urlsplit(req.full_url).path).removeprefix('/_matrix/client/v3')
        if path == '/account/whoami':
            result = {'user_id': '@codex:matrix.paragogy.net'}
        elif path == '/sync':
            result = self.syncs.pop(0)
        elif path.startswith('/join/'):
            result = {'room_id': path[6:]}
        elif '/send/m.room.message/' in path:
            self.posts.setdefault(path, json.loads(req.data))
            if self.fail_send_once:
                self.fail_send_once = False
                raise TimeoutError('ambiguous transport outcome')
            result = {'event_id': '$sent'}
        else:
            raise AssertionError(path)
        return io.BytesIO(json.dumps(result).encode())

    def invoke(self, req, timeout=None):
        self.invokes.append(json.loads(req.data))
        return io.BytesIO(json.dumps({'ok': True, 'result': self.reply}).encode())

    def announce(self, url, payload, **kwargs):
        self.announces.append(payload)
        return {'ok': True, 'job-id': payload['job-id']}


class MatrixTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        (self.root / 'codex.token').write_text('offline-token\n')
        self.http = FakeHTTP()
        evidence_patch = patch.object(m.irc, "post_transport_evidence")
        self.evidence = evidence_patch.start()
        self.addCleanup(evidence_patch.stop)
        self.bots = []
        m.irc.ungated_nicks.clear()
        for p in [patch.object(m.urllib.request, 'build_opener', return_value=self.http),
                  patch.object(m.irc.urllib.request, 'urlopen', side_effect=self.http.invoke),
                  patch.object(m.irc, 'api_post', side_effect=self.http.announce),
                  patch.object(m.IRCBot, '_prepare_agent_for_new_invoke', return_value={'ok': True})]:
            p.start()
            self.addCleanup(p.stop)
        self.addCleanup(self.tmp.cleanup)
        self.addCleanup(self.shutdown)

    def shutdown(self):
        for bot in self.bots:
            bot.stop()
            bot._invoke_queue.put(None)
            bot._worker.join(2)
            self.assertFalse(bot._worker.is_alive())

    def bot(self, baseline=True):
        bot = m.MatrixBot('codex', 'codex-1', [ROOM], 'https://offline.invalid',
                          self.root, self.root / 'state', handle_commands=True)
        self.bots.append(bot)
        bot.connect()
        if baseline and bot._state['next_batch'] is None:
            bot.process_sync(batch('baseline'))
        return bot

    def drain(self, bot):
        deadline = time.monotonic() + 2
        while bot._invoke_queue.unfinished_tasks and time.monotonic() < deadline:
            time.sleep(.001)
        self.assertEqual(0, bot._invoke_queue.unfinished_tasks)

    def test_mention_invokes_and_replies_with_full_sender_and_surface(self):
        bot = self.bot()
        bot.process_sync(batch('b1', [event()]))
        self.drain(bot)
        self.assertEqual(1, len(self.http.invokes))
        for payload in self.http.invokes + self.http.announces:
            self.assertEqual('matrix:' + SENDER, payload['caller'])
            self.assertEqual('matrix (' + ROOM + ')', payload['surface'])
            self.assertIn('Surface: Matrix', payload['prompt'])
        reply = next(p for p in self.http.posts.values() if p['body'] == 'answer')
        self.assertEqual('$one', reply['m.relates_to']['m.in_reply_to']['event_id'])

    def test_inherited_gating_and_commands(self):
        bot = self.bot()
        bot.process_sync(batch('b1', [event(body='hello')]))
        self.assertFalse(self.http.invokes)
        bot.process_sync(batch('b2', [event('$ungate', '!ungate codex'), event('$plain', 'hello')]))
        self.drain(bot)
        self.assertEqual(1, len(self.http.invokes))
        bot.process_sync(batch('b3', [event('$gate', '!gate codex'), event('$quiet', 'hello')]))
        self.drain(bot)
        self.assertEqual(1, len(self.http.invokes))

    def test_full_mxid_and_hyphen_boundary(self):
        bot = self.bot()
        self.assertTrue(bot._is_mention('@codex:matrix.paragogy.net hi'))
        self.assertFalse(bot._is_mention('@codex-2 hi'))
        bot.process_sync(batch('b1', [event('$full', '@codex:matrix.paragogy.net hi'),
                                      event('$other', '@codex-2 hi')]))
        self.drain(bot)
        self.assertEqual(1, len(self.http.invokes))

    def test_filters_and_encryption_warning_once(self):
        bot = self.bot()
        with patch.object(m.irc, 'log') as log:
            bot.process_sync(batch('b1', [event(sender=bot.mxid),
                event('$enc', kind='m.room.encrypted'), event('$enc2', kind='m.room.encrypted'),
                event('$state', kind='m.room.name')]))
            self.assertEqual(1, log.call_count)
        bot.process_sync(batch('b2', [event()], room=OTHER))
        self.assertFalse(self.http.invokes)

    def test_restart_same_batch_and_event_under_new_token(self):
        bot = self.bot()
        original = batch('b1', [event()])
        bot.process_sync(original)
        self.drain(bot)
        fresh = self.bot()
        fresh.process_sync(original)
        fresh.process_sync(batch('b2', [event()]))
        self.drain(fresh)
        self.assertEqual(1, len(self.http.invokes))
        self.assertEqual('b2', json.loads(fresh.state_path.read_text())['next_batch'])

    def test_first_sync_skips_history_even_if_replayed(self):
        bot = self.bot(baseline=False)
        self.http.syncs = [batch('initial', [event()]), batch('later', [event()])]
        bot.sync_once()
        bot.sync_once()
        self.drain(bot)
        self.assertFalse(self.http.invokes)
        query = parse_qs(urlsplit(self.http.calls[-1].full_url).query)
        self.assertEqual(['initial'], query['since'])
        self.assertEqual([ROOM], json.loads(query['filter'][0])['room']['rooms'])
        self.assertEqual(['30000'], query['timeout'])

    def test_invites_only_listed_rooms(self):
        self.bot(baseline=False).process_sync(batch('initial', invites=[OTHER, ROOM]))
        joined = [unquote(urlsplit(r.full_url).path) for r in self.http.calls if r.method == 'POST']
        self.assertEqual(['/_matrix/client/v3/join/' + ROOM], joined)

    def test_send_timeout_reuses_transaction_and_long_reply_not_split(self):
        bot = self.bot()
        self.http.fail_send_once = True
        bot._say('x' * 2000)
        sends = [r for r in self.http.calls if r.method == 'PUT']
        self.assertEqual(2, len(sends))
        self.assertEqual(sends[0].full_url, sends[1].full_url)
        self.assertEqual(1, len(self.http.posts))
        self.assertEqual('x' * 2000, next(iter(self.http.posts.values()))['body'])
        self.http.reply = 'y' * 2000
        bot.process_sync(batch('b1', [event()]))
        self.drain(bot)
        self.assertTrue(any(p['body'] == self.http.reply for p in self.http.posts.values()))
        bot._say('z' * 20000)
        last = list(self.http.posts.values())[-1]['body']
        self.assertLessEqual(len(last), bot.text_cap)
        self.assertTrue(last.endswith('[truncated]'))

    def test_queue_preserves_distinct_event_reply_contexts(self):
        bot = self.bot()
        bot.process_sync(batch('b1', [event('$a'), event('$b')]))
        self.drain(bot)
        replies = [p for p in self.http.posts.values() if p['body'] == 'answer']
        self.assertEqual(['$a', '$b'], [p['m.relates_to']['m.in_reply_to']['event_id'] for p in replies])

    def test_bounded_ids_and_state_write_failure_precede_dispatch(self):
        bot = self.bot()
        bot.dedup_limit = 2
        bot.process_sync(batch('b1', [event('$a', 'quiet'), event('$b', 'quiet'),
                                      event('$c', 'quiet')]))
        self.assertEqual(['$b', '$c'], json.loads(bot.state_path.read_text())['seen'])
        with patch.object(bot, '_save_state', side_effect=OSError('disk full')):
            with self.assertRaises(OSError):
                bot.process_sync(batch('b2', [event('$new')]))
        self.assertFalse(self.http.announces)
        self.assertFalse(self.http.invokes)

    def test_token_identity_mismatch_and_missing_cursor_refused(self):
        bot = self.bot()
        with patch.object(bot, '_request', return_value={'user_id': '@someone:server'}):
            with self.assertRaises(ValueError):
                bot.connect()
        with self.assertRaises(ValueError):
            bot.process_sync({'rooms': {}})

    def test_transcript_evidence_uses_matrix_identity(self):
        bot = self.bot()
        bot.process_sync(batch('b1', [event()]))
        self.drain(bot)
        self.evidence.assert_any_call('matrix', ROOM, SENDER, '@codex hello',
                                      'inbound', via_nick='codex')
        self.evidence.assert_any_call('matrix', ROOM, bot.mxid, 'answer',
                                      'outbound', via_nick='codex')

    def test_logic_is_inherited(self):
        for name in ['_is_mention', '_strip_mention', '_handle_mention', '_handle_ungated',
                     '_handle_command', '_enqueue_invoke', '_invoke_worker_loop', '_announce_invoke',
                     '_invoke_agent', '_cmd_gate', '_cmd_ungate', '_cmd_mc']:
            self.assertIs(getattr(m.MatrixBot, name), getattr(m.IRCBot, name))

    def test_invalid_config_and_state_refused(self):
        for nick, rooms in [('../codex', [ROOM]), ('codex', ['#alias:server'])]:
            with self.assertRaises(ValueError):
                m.MatrixBot(nick, 'codex-1', rooms, 'https://offline.invalid', self.root, self.root)
        (self.root / 'codex.json').write_text('{"seen": "bad", "next_batch": "x"}')
        with self.assertRaises(ValueError):
            m.MatrixBot('codex', 'codex-1', [ROOM], 'https://offline.invalid', self.root, self.root)
        bot = self.bot()
        with self.assertRaises(ValueError):
            bot._say('no', channel=OTHER)


if __name__ == '__main__':
    unittest.main()
