#!/usr/bin/env python3
"""ALFWorld HTTP bridge for Claude peripheral integration.

Wraps an ALFWorld TextWorld environment as a simple HTTP server.
Claude interacts via curl; Joe coaches via the CLI walkie-talkie.

Usage:
    .venv-alfworld/bin/python3 scripts/alfworld-server.py [--port 3456]

Endpoints:
    POST /reset  - Start new game (random pick-and-place task)
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
from http.server import HTTPServer, BaseHTTPRequestHandler
from os.path import join as pjoin

from alfworld.info import ALFWORLD_DATA
from alfworld.agents.utils.misc import add_task_to_grammar
from alfworld.agents.environment.alfred_tw_env import AlfredDemangler

import textworld
import textworld.gym


def pick_problem():
    """Pick a random problem that has a pre-built game file."""
    # Only pick problems with existing game.tw-pddl (not all have them)
    # Prefer valid_unseen for evaluation variety
    games = glob.glob(
        pjoin(ALFWORLD_DATA, "json_2.1.1", "valid_unseen", "**", "game.tw-pddl"),
        recursive=True,
    )
    if not games:
        # Fall back to any split
        games = glob.glob(
            pjoin(ALFWORLD_DATA, "**", "game.tw-pddl"), recursive=True
        )
    # Remove movable receptacle problems (unsupported)
    games = [g for g in games if "movable_recep" not in g]
    if not games:
        raise ValueError(
            f"No game files found in {ALFWORLD_DATA}. Run: alfworld-download"
        )
    return os.path.dirname(random.choice(games))


def make_env(problem_dir):
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

    gamefile = pjoin(problem_dir, "game.tw-pddl")

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
            traceback.print_exc()
            self._send_json({"error": str(e)}, 500)

    def _handle_reset(self):
        with game.lock:
            # Close previous env if any
            if game.env is not None:
                try:
                    game.env.close()
                except Exception:
                    pass

            problem_dir = pick_problem()
            env, traj_data = make_env(problem_dir)

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
            game.problem_dir = problem_dir

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
        sys.stderr.write(f"[alfworld] {args[0]}\n")


def main():
    port = 3456
    if "--port" in sys.argv:
        idx = sys.argv.index("--port")
        port = int(sys.argv[idx + 1])

    print(f"[alfworld-server] Starting on http://localhost:{port}")
    print(f"[alfworld-server] ALFWORLD_DATA = {ALFWORLD_DATA}")
    print(f"[alfworld-server] POST /reset to start a game")
    print(f"[alfworld-server] POST /step  to take an action")
    print(f"[alfworld-server] GET  /state to see current state")
    print(f"[alfworld-server] POST /quit  to shutdown")

    server = HTTPServer(("localhost", port), ALFWorldHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\n[alfworld-server] Shutting down")
        server.shutdown()


if __name__ == "__main__":
    main()
