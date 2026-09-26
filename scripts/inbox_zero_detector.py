#!/usr/bin/env python3
"""Commit-scoped ext4 event detector. Read-only; never edits watcher state.

One inotify fd is armed recursively before READY. stdin 'drain' returns the
ordered kernel queue through EAGAIN. A new directory is itself a race, so it
need not be watched after arming. Git metadata is excluded, not working files.
This is filesystem-API event coverage, not mmap/remote-writer coverage.
"""
import ctypes
import json
import os
from pathlib import Path
import struct
import subprocess
import sys
import time

RESPONSE_BUDGET_MS = 5000
EVENT = struct.Struct('iIII')
MASK = 0x00000002 | 0x00000004 | 0x00000008 | 0x00000040 | 0x00000080 | 0x00000100 | 0x00000200 | 0x00000400 | 0x00000800 | 0x00002000
BAD = 0x00002000 | 0x00004000 | 0x00008000  # unmount, overflow, ignored watch


class Detector:
    def __init__(self, root):
        self.root = Path(root).resolve(strict=True)
        fs = subprocess.check_output(['findmnt', '-T', str(self.root), '-n', '-o', 'FSTYPE'], timeout=5, text=True).strip()
        if fs != 'ext4':
            raise RuntimeError('detector-filesystem-not-commissioned: ' + fs)
        libc = ctypes.CDLL(None, use_errno=True)
        libc.inotify_init1.argtypes = [ctypes.c_int]
        libc.inotify_add_watch.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_uint32]
        self.fd = libc.inotify_init1(os.O_NONBLOCK | os.O_CLOEXEC)
        if self.fd < 0:
            raise OSError(ctypes.get_errno(), 'inotify_init1')
        self.watches = {}
        try:
            for directory, dirs, _ in os.walk(self.root, followlinks=False, onerror=self.walk_error):
                dirs[:] = [d for d in dirs if d != '.git' and not Path(directory, d).is_symlink()]
                wd = libc.inotify_add_watch(self.fd, os.fsencode(directory), MASK)
                if wd < 0:
                    raise OSError(ctypes.get_errno(), 'inotify_add_watch: ' + directory)
                self.watches[wd] = Path(directory)
        except BaseException:
            self.close()
            raise

    @staticmethod
    def walk_error(error):
        raise error

    def close(self):
        os.close(self.fd)

    def drain(self):
        started = time.monotonic_ns()
        events = []
        count = 0
        complete = True
        invalid = False
        while True:
            if (time.monotonic_ns() - started) / 1e6 >= RESPONSE_BUDGET_MS:
                complete = False
                break
            try:
                data = os.read(self.fd, 65536)
            except BlockingIOError:
                break
            if not data:
                raise RuntimeError('detector EOF')
            offset = 0
            while offset < len(data):
                wd, mask, cookie, size = EVENT.unpack_from(data, offset)
                offset += EVENT.size
                name = os.fsdecode(data[offset:offset + size].split(b'\0', 1)[0])
                offset += size
                count += 1
                invalid |= bool(mask & BAD)
                parent = self.watches.get(wd)
                path = str((parent / name).relative_to(self.root)) if parent else None
                if len(events) < 256:
                    events.append({'path': path, 'mask': mask, 'cookie': cookie})
        return {'complete': complete, 'invalid': invalid, 'event_count': count,
                'events': events, 'truncated': count > len(events),
                'elapsed_ms': (time.monotonic_ns() - started) / 1e6,
                'response_budget_ms': RESPONSE_BUDGET_MS}


def emit(value):
    print(json.dumps(value), flush=True)


def main():
    detector = None
    try:
        detector = Detector(sys.argv[1])
        emit({'ready': True, 'directories': len(detector.watches),
              'filesystem': 'ext4', 'response_budget_ms': RESPONSE_BUDGET_MS})
        for line in sys.stdin:
            command = line.strip()
            if command == 'drain':
                emit(detector.drain())
            elif command == 'close':
                break
            else:
                raise ValueError('unknown detector command')
    except Exception as error:
        emit({'error': str(error), 'complete': False})
        return 1
    finally:
        if detector is not None:
            detector.close()
    return 0


if __name__ == '__main__':
    sys.exit(main())
