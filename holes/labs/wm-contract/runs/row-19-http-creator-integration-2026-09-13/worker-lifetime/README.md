# Worker-lifetime drain repair

Terminal ledger state and delivery no longer release execution. Only actual
worker-wrapper unwind does. Stable-ID reuse preserves the existing lifecycle
set, and the lifecycle-order monitor serializes ledger commits with controller
notifications without reversing controller/writer lock order.

Source remains inactive and undeployed. Startup reconciliation, parked and
deadline resumes, and the external first-install fence remain open.
