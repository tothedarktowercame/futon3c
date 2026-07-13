;;; claude-repl-bootstrap.el --- Persistent bootstrap for parked-on + jobs -*- lexical-binding: t; -*-

;; E-park-delivery-losses finding 0: the poller (claude-repl-park.el) and jobs
;; visibility (claude-repl-jobs.el) were never installed at startup — they had
;; to be loaded manually each session, so an Emacs restart silently disabled the
;; poller and resumes rotted in the ready-inbox. This file loads BOTH after
;; claude-repl so a restart cannot silently disable them.
;;
;; WHERE TO REQUIRE THIS FROM:
;;   Joe's Emacs init does NOT currently load the futon3c emacs files at all
;;   (they are loaded manually via `load-file' in the live session). The right
;;   home for this bootstrap is wherever Joe's workflow loads claude-repl.el —
;;   most likely a manual `(load ".../claude-repl.el")' or a project-local init.
;;
;;   TO ACTIVATE (one line, wherever claude-repl.el is loaded):
;;     (load "/home/joe/code/futon3c/emacs/claude-repl-bootstrap.el")
;;
;;   Do NOT edit Joe's personal init.el/settings.el — flag this for Joe/claude-6
;;   to wire in the right place after review.

;;; Code:

(require 'claude-repl)
(require 'claude-repl-park)
(require 'claude-repl-jobs)

(provide 'claude-repl-bootstrap)
;;; claude-repl-bootstrap.el ends here
