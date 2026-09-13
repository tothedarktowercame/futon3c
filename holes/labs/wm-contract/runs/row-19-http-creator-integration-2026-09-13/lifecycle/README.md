# HTTP invoke lifecycle accounting

The common creator now retains post-rename uncertain jobs as accepted work.
Actual queued/activating-to-running transitions start execution accounting;
the first terminal transition moves executing or never-started work to final
delivery; and the first durable delivery receipt finishes it. Duplicate
running, terminal, and delivery transitions do not move state twice.

This remains unloaded source with inactive configuration. Parked/deadline
resume wiring, startup reconciliation, and the first-install external fence
remain open, so no restart or deployment readiness is claimed.
