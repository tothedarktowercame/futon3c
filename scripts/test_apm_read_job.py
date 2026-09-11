import importlib.util
import io
import json
from pathlib import Path
import unittest
from unittest.mock import patch
import urllib.error

spec = importlib.util.spec_from_file_location('reader', Path(__file__).with_name('apm-read-job.py'))
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)


class ReadJobTest(unittest.TestCase):
    def test_success_and_route(self):
        body = {'ok': True, 'job': {'job-id': 'a/b', 'events': []}}
        with patch.object(reader.urllib.request, 'urlopen', return_value=io.BytesIO(json.dumps(body).encode())) as call:
            self.assertEqual(body, reader.read_job('http://agency/', 'a/b'))
            call.assert_called_once_with('http://agency/api/alpha/invoke/jobs/a%2Fb', timeout=30)

    def test_wrong_identity(self):
        with patch.object(reader.urllib.request, 'urlopen', return_value=io.BytesIO(b'{"ok":true,"job":{"job-id":"other"}}')):
            self.assertFalse(reader.read_job('http://agency', 'wanted')['ok'])

    def test_html_404_preserves_diagnostic(self):
        error = urllib.error.HTTPError('http://store', 404, 'Not found', {}, io.BytesIO(b'<h1>404</h1>'))
        with patch.object(reader.urllib.request, 'urlopen', side_effect=error):
            result = reader.read_job('http://store', 'job')
            self.assertEqual(404, result['status'])
            self.assertEqual('<h1>404</h1>', result['error'])
            self.assertIn('/invoke/jobs/job', result['url'])

    def test_timeout_no_retry(self):
        with patch.object(reader.urllib.request, 'urlopen', side_effect=TimeoutError('timed out')) as call:
            self.assertFalse(reader.read_job('http://agency', 'job')['ok'])
            self.assertEqual(1, call.call_count)


if __name__ == '__main__':
    unittest.main()
