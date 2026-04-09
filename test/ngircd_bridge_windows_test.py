import pathlib
import unittest


class NgircdBridgeWindowsScriptTest(unittest.TestCase):
    def test_wrapper_forces_utf8_stdio_defaults(self):
        path = pathlib.Path(
            "I:/darktower/futon3c-mfuton-overlay/scripts/windows/ngircd-bridge-windows.bat"
        )
        text = path.read_text(encoding="utf-8")

        self.assertIn('if not defined PYTHONUTF8 set "PYTHONUTF8=1"', text)
        self.assertIn(
            'if not defined PYTHONIOENCODING set "PYTHONIOENCODING=UTF-8"', text
        )
        self.assertIn("echo [ngircd-bridge-windows] PYTHONUTF8=%PYTHONUTF8%", text)
        self.assertIn(
            "echo [ngircd-bridge-windows] PYTHONIOENCODING=%PYTHONIOENCODING%", text
        )


if __name__ == "__main__":
    unittest.main()
