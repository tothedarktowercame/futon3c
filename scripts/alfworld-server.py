#!/usr/bin/env python3
"""ALFWorld HTTP bridge for Claude peripheral integration.

Wraps an ALFWorld TextWorld environment as a simple HTTP server.
Claude interacts via curl; Joe coaches via the CLI walkie-talkie.

Usage:
    .venv-alfworld/bin/python3 scripts/alfworld-server.py [--port 3456]

Environment:
    ALFWORLD_SPLIT: train | valid_seen | valid_unseen (default: train)
    ALFWORLD_INCLUDE_UNSOLVABLE=1 to allow selecting unsolvable games (default: 0)

Endpoints:
    POST /reset  - Start new game (random pick-and-place task)
                   Optional JSON body for reproducibility/debugging:
                     {"seed": 123}
                     {"problem_dir": "/path/to/problem_dir"}
                     {"gamefile": "/path/to/game.tw-pddl"}
    POST /step   - Take action. Body: {"action": "go to shelf 1"}
    GET  /state  - Current observation, admissible commands, step count
    POST /quit   - Shutdown server
"""

import glob
import json
import os
import random
import sys
import threading
from datetime import datetime
from functools import lru_cache
from http.server import HTTPServer, BaseHTTPRequestHandler
from os.path import join as pjoin

from alfworld.info import ALFWORLD_DATA
from alfworld.agents.utils.misc import add_task_to_grammar
from alfworld.agents.environment.alfred_tw_env import AlfredDemangler

import textworld
import textworld.gym


def _ts():
    # Keep consistent with futon6 cluster scripts: [HH:MM:SS] prefix.
    return datetime.now().strftime("%H:%M:%S")


def log(msg):
    sys.stderr.write(f"[{_ts()}] {msg}\n")
    sys.stderr.flush()


def _split_root():
    # Keep terminology consistent with ALFWorld/base_config.yaml.
    # Accepted values: train | valid_seen | valid_unseen
    split = os.environ.get("ALFWORLD_SPLIT", "train").strip()
    split_aliases = {
        "eval_out_of_distribution": "valid_unseen",
        "eval_ood": "valid_unseen",
        "eval_in_distribution": "valid_seen",
        "eval_id": "valid_seen",
    }
    split = split_aliases.get(split, split)
    return split


@lru_cache(maxsize=4)
def _gamefile_pool(split: str, include_unsolvable: bool):
    # NOTE: This mirrors the intent of AlfredTWEnv.collect_game_files:
    # only choose solvable games, and skip known unsupported categories.
    games = glob.glob(
        pjoin(ALFWORLD_DATA, "json_2.1.1", split, "**", "game.tw-pddl"),
        recursive=True,
    )
    if not games:
        # Fall back to any split
        games = glob.glob(pjoin(ALFWORLD_DATA, "**", "game.tw-pddl"), recursive=True)

    # Remove movable receptacle problems (unsupported) and other known excluded sets.
    games = [g for g in games if "movable_recep" not in g and "Sliced" not in g and "/movable" not in g]

    if include_unsolvable:
        return games

    solvable = []
    for g in games:
        try:
            with open(g, "r") as f:
                data = json.load(f)
        except Exception:
            continue
        if data.get("solvable") is True:
            solvable.append(g)
    return solvable


def pick_gamefile():
    """Pick a random game.tw-pddl file."""
    split = _split_root()
    include_unsolvable = os.environ.get("ALFWORLD_INCLUDE_UNSOLVABLE", "0").strip() == "1"
    games = _gamefile_pool(split, include_unsolvable)
    if not games:
        raise ValueError(
            f"No solvable game files found in {ALFWORLD_DATA} (split={split}). Run: alfworld-download"
        )
    return random.choice(games)


def make_env(problem_dir, gamefile=None):
    """Create a textworld gym environment for a single problem."""
    domain_path = pjoin(ALFWORLD_DATA, "logic", "alfred.pddl")
    grammar_path = pjoin(ALFWORLD_DATA, "logic", "alfred.twl2")

    game_logic = {
        "pddl_domain": open(domain_path).read(),
        "grammar": open(grammar_path).read(),
    }

    # Load trajectory data to get task description
    json_file = pjoin(problem_dir, "traj_data.json")
    with open(json_file) as f:
        traj_data = json.load(f)

    game_logic["grammar"] = add_task_to_grammar(game_logic["grammar"], traj_data)

    gamefile = gamefile or pjoin(problem_dir, "game.tw-pddl")

    request_infos = textworld.EnvInfos(
        won=True,
        admissible_commands=True,
        score=True,
        max_score=True,
        intermediate_reward=True,
    )

    env_id = textworld.gym.register_game(
        gamefile,
        request_infos,
        max_episode_steps=100,
        wrappers=[AlfredDemangler()],
    )

    return textworld.gym.make(env_id), traj_data


class GameState:
    """Shared mutable game state."""

    def __init__(self):
        self.env = None
        self.observation = None
        self.infos = None
        self.task = None
        self.task_type = None
        self.step_count = 0
        self.done = False
        self.won = False
        self.score = 0.0
        self.max_score = 1.0
        self.problem_dir = None
        self.gamefile = None
        self.lock = threading.Lock()

    def to_dict(self):
        admissible = []
        if self.infos and "admissible_commands" in self.infos:
            cmds = self.infos["admissible_commands"]
            admissible = list(cmds) if isinstance(cmds, (list, tuple)) else []
        return {
            "observation": self.observation or "",
            "task": self.task or "",
            "task_type": self.task_type or "",
            "step": self.step_count,
            "done": self.done,
            "won": self.won,
            "score": self.score,
            "max_score": self.max_score,
            "problem_dir": self.problem_dir,
            "gamefile": self.gamefile,
            "admissible_commands": admissible,
        }


game = GameState()


class ALFWorldHandler(BaseHTTPRequestHandler):
    """HTTP request handler for ALFWorld bridge."""

    def _send_json(self, data, status=200):
        body = json.dumps(data, indent=2).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length > 0:
            return json.loads(self.rfile.read(length))
        return {}

    def do_GET(self):
        if self.path == "/state":
            with game.lock:
                self._send_json(game.to_dict())
        else:
            self._send_json({"error": f"Unknown: {self.path}"}, 404)

    def do_POST(self):
        try:
            if self.path == "/reset":
                self._handle_reset()
            elif self.path == "/step":
                self._handle_step()
            elif self.path == "/quit":
                self._send_json({"message": "ALFWorld server shutting down"})
                threading.Thread(target=self.server.shutdown, daemon=True).start()
            else:
                self._send_json({"error": f"Unknown: {self.path}"}, 404)
        except Exception as e:
            import traceback
            # Timestamped traceback for cluster log consistency.
            for line in traceback.format_exc().rstrip().splitlines():
                log(f"[alfworld] {line}")
            with game.lock:
                ctx = {}
                if game.problem_dir:
                    ctx["problem_dir"] = game.problem_dir
                if game.gamefile:
                    ctx["gamefile"] = game.gamefile
            self._send_json({"error": str(e), **ctx}, 500)

    def _handle_reset(self):
        body = self._read_body()
        with game.lock:
            # Close previous env if any
            if game.env is not None:
                try:
                    game.env.close()
                except Exception:
                    pass

            if isinstance(body, dict) and "seed" in body:
                try:
                    random.seed(int(body["seed"]))
                except Exception:
                    self._send_json({"error": "seed must be an integer"}, 400)
                    return

            if isinstance(body, dict) and body.get("gamefile"):
                gamefile = os.path.expanduser(str(body["gamefile"]))
            elif isinstance(body, dict) and body.get("problem_dir"):
                problem_dir = os.path.expanduser(str(body["problem_dir"]))
                gamefile = pjoin(problem_dir, "game.tw-pddl")
            else:
                gamefile = pick_gamefile()

            if not os.path.isfile(gamefile):
                self._send_json({"error": f"gamefile not found: {gamefile}"}, 400)
                return

            problem_dir = os.path.dirname(gamefile)

            # Record early so exceptions include repro path.
            game.problem_dir = problem_dir
            game.gamefile = gamefile
            log(f"[alfworld-server] /reset problem_dir={problem_dir} gamefile={gamefile}")

            env, traj_data = make_env(problem_dir, gamefile=gamefile)

            obs, infos = env.reset()
            game.env = env
            game.observation = obs
            game.infos = infos
            game.task = traj_data.get("turk_annotations", {}).get(
                "anns", [{}]
            )[0].get("task_desc", "")
            game.task_type = traj_data.get("task_type", "unknown")
            game.step_count = 0
            game.done = False
            game.won = False
            game.score = 0.0
            game.max_score = float(infos.get("max_score") or 1.0)
            self._send_json(game.to_dict())

    def _handle_step(self):
        body = self._read_body()
        action = body.get("action", "look")

        with game.lock:
            if game.env is None:
                self._send_json(
                    {"error": "No game running. POST /reset first."}, 400
                )
                return

            if game.done:
                self._send_json(
                    {
                        "error": "Game is over. POST /reset for a new one.",
                        **game.to_dict(),
                    },
                    400,
                )
                return

            obs, score, done, infos = game.env.step(action)
            game.observation = obs
            game.infos = infos
            game.score = float(score)
            game.done = bool(done)
            game.won = bool(infos.get("won", False))
            game.step_count += 1

            self._send_json(game.to_dict())

    def log_message(self, format, *args):
        try:
            msg = format % args
        except Exception:
            msg = format
        ip = self.client_address[0] if self.client_address else "?"
        log(f"[alfworld] {ip} {msg}")


def main():
    port = 3456
    if "--port" in sys.argv:
        idx = sys.argv.index("--port")
        port = int(sys.argv[idx + 1])

    split = _split_root()
    include_unsolvable = os.environ.get("ALFWORLD_INCLUDE_UNSOLVABLE", "0").strip() == "1"
    pool_size = len(_gamefile_pool(split, include_unsolvable))

    log(f"[alfworld-server] Starting on http://localhost:{port}")
    log(f"[alfworld-server] ALFWORLD_DATA = {ALFWORLD_DATA}")
    log(f"[alfworld-server] Split = {split} | include_unsolvable = {int(include_unsolvable)} | pool_size = {pool_size}")
    log("[alfworld-server] POST /reset to start a game")
    log("[alfworld-server] POST /step  to take an action")
    log("[alfworld-server] GET  /state to see current state")
    log("[alfworld-server] POST /quit  to shutdown")

    server = HTTPServer(("localhost", port), ALFWorldHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log("[alfworld-server] Shutting down")
        server.shutdown()


if __name__ == "__main__":
    main()
