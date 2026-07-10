import os

os.environ.setdefault("INVOKE_BASE", "http://127.0.0.1:7070")

import ngircd_bridge as bridge  # noqa: E402


def roster(*pairs):
    return {"ok": True, "agents": dict(pairs)}


def agent(agent_type="claude", metadata=None, **extra):
    payload = {"type": agent_type, "metadata": metadata or {}}
    payload.update(extra)
    return payload


def test_desired_bot_nicks_includes_new_local_agent():
    data = roster(("claude-4", agent("claude")))
    assert bridge.desired_bot_nicks(data, {"claude", "codex", "zai"}) == {"claude-4"}


def test_desired_bot_nicks_excludes_proxy_or_remote_agents():
    data = roster(
        ("lon-claude-1", agent("claude", {"proxy?": True, "remote?": True})),
        ("chi-claude-1", agent("claude", {"remote?": True})),
        ("claude-4", agent("claude")),
    )
    assert bridge.desired_bot_nicks(data, {"claude"}) == {"claude-4"}


def test_desired_bot_nicks_absent_agent_absent_from_desired_set():
    before = roster(("claude-4", agent("claude")), ("codex-2", agent("codex")))
    after = roster(("codex-2", agent("codex")))
    assert "claude-4" in bridge.desired_bot_nicks(before, {"claude", "codex"})
    assert "claude-4" not in bridge.desired_bot_nicks(after, {"claude", "codex"})


def test_desired_bot_nicks_filters_agent_type_allowlist():
    data = roster(
        ("claude-4", agent("claude")),
        ("tickle-1", agent("tickle")),
        ("zai-1", agent("zai")),
    )
    assert bridge.desired_bot_nicks(data, {"claude", "zai"}) == {"claude-4", "zai-1"}


def test_flag_off_preserves_static_bridge_bots_exactly():
    data = roster(("claude-4", agent("claude")))
    selected = bridge.selected_bot_nicks(
        data,
        bots_from_roster=False,
        static_bots=["claude", "codex"],
        type_allowlist={"claude"},
    )
    assert selected == {"claude", "codex"}


def test_flag_on_unions_static_bots_with_roster_agents():
    data = roster(("claude-4", agent("claude")))
    selected = bridge.selected_bot_nicks(
        data,
        bots_from_roster=True,
        static_bots=["codex"],
        type_allowlist={"claude"},
    )
    assert selected == {"codex", "claude-4"}
