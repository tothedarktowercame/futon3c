from pathlib import Path

import pytest

import gates
import split_migrate


RICH_SOURCE = """import Mathlib

/-! Sample statement bank input. -/

open Set
open scoped Topology

noncomputable section

def helper (K : Type*) := K

theorem apm_x00A01 (K : Type*) [RCLike K] {n : ℕ}
    (x : Fin n → K) : x = x := by
  intro
  rfl

end
"""


def test_round_trip_moves_helpers_and_preserves_rich_binders():
    result = split_migrate.split_source(RICH_SOURCE, "x00A01")

    statement = result["statement_module"]
    main = result["main_file"]
    assert result["theorem_name"] == "apm_x00A01"
    assert "def helper (K : Type*) := K" in statement
    assert "def helper" not in main
    assert "def apm_x00A01_stmt : Prop := ∀ " in statement
    assert statement.count("def apm_x00A01_stmt : Prop := ∀ ") == 1
    assert "(K : Type*) [RCLike K] {n : ℕ}" in statement
    assert "import ApmStatements.X00A01" in main
    assert main.count("theorem ") == 1
    assert "theorem apm_x00A01 : apm_x00A01_stmt := by" in main
    assert "  intro\n  rfl" in main


def test_no_binders_does_not_insert_forall():
    source = "import Mathlib\n\ntheorem apm_x00A02 : True := by\n  trivial\n"
    result = split_migrate.split_source(source, "x00A02")

    declaration = result["statement_module"].split("def apm_x00A02_stmt", 1)[1]
    assert declaration.startswith(" : Prop := True")
    assert "∀" not in declaration


def test_unsplittable_source_raises_gate_error():
    with pytest.raises(gates.GateError):
        split_migrate.split_source("import Mathlib\n\ndef only_helper := 1\n", "x00A03")


def test_already_split_source_raises_instead_of_double_wrapping():
    source = """import ApmStatements.X00A04

theorem apm_x00A04 : apm_x00A04_stmt := by
  trivial
"""
    with pytest.raises(gates.GateError, match="already split"):
        split_migrate.split_source(source, "x00A04")


def test_already_split_claim_raises_even_if_import_was_reformatted():
    source = "theorem apm_x00A04 : apm_x00A04_stmt := by\n  trivial\n"
    with pytest.raises(gates.GateError, match="already split"):
        split_migrate.split_source(source, "x00A04")


def test_migrate_dry_run_writes_nothing(tmp_path: Path):
    main = tmp_path / "problems" / "x00A01" / "lean" / "Main.lean"
    main.parent.mkdir(parents=True)
    main.write_text(RICH_SOURCE, encoding="utf-8")

    result = split_migrate.migrate("x00A01", tmp_path, dry_run=True)

    assert result["main_path"] == main
    assert not (tmp_path / "ApmStatements" / "X00A01.lean").exists()
    assert main.read_text(encoding="utf-8") == RICH_SOURCE


def test_section_opened_before_the_theorem_is_closed_in_the_statement_module():
    """a00J02/a03J05/a94A09 open `noncomputable section` before the theorem and
    close it after; a naive split orphans the `end` in Main.lean."""

    source = (
        "import Mathlib\n\n"
        "noncomputable section\n\n"
        "def helper : Nat := 1\n\n"
        "theorem apm_x00a01 (n : Nat) : n = n := by\n"
        "  rfl\n\n"
        "end\n"
    )
    out = split_migrate.split_source(source, "x00A01")
    # the original leaves the section open at EOF; we keep that shape
    assert "noncomputable section" in out["statement_module"]
    assert "\nend" not in out["main_file"], out["main_file"]
    assert "theorem apm_x00a01 : apm_x00a01_stmt :=" in out["main_file"]


def test_namespace_is_refused_because_it_would_rename_the_theorem():
    source = (
        "import Mathlib\n\n"
        "namespace Foo\n\n"
        "theorem apm_x00a01 (n : Nat) : n = n := by\n"
        "  rfl\n\n"
        "end Foo\n"
    )
    try:
        split_migrate.split_source(source, "x00A01")
    except gates.GateError as exc:
        assert "namespace" in str(exc)
    else:
        raise AssertionError("expected GateError for a namespaced theorem")
