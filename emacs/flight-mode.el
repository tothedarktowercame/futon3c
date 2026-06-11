;;; flight-mode.el --- Live WM-flight organ view for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; The flight analogue of mission-mode.el: a War-Machine flight given proper
;; scopes at all times.  Entry point: M-x flight-mode
;;
;; The scope vocabulary is the flight anatomy's ORGANS
;; (futon6/holes/anatomy-of-a-wm-flight.md §2): field-read, velocity,
;; attribution, prediction, counterfactual, begin-state, act+witness,
;; measurement, out-of-band, self-record.  An organ that hasn't happened yet
;; renders as a GHOST line (cf. mission-mode's ghost phases) — so an
;; in-progress flight visibly fills in organ by organ, and an abandoned one
;; shows exactly where it stopped.
;;
;; Pipeline (mirrors the mission pipeline, one stage shorter — flight
;; artifacts are already typed EDN, no substrate ingest needed for v1):
;;   futon3c/data/repl-traces/<run-id>{.begin,}.edn   the artifacts
;;   futon3c/data/discipline-events.edn               out-of-band organ
;;   futon3c/scripts/flight_scope_view.bb             artifacts -> JSON
;;   flight-mode.el                                   this panel
;;
;; Keys: g refresh · l choose run · n/p next/prev run · RET visit artifact
;;       a toggle auto-refresh (5s; for watching a live flight fill in)

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup flight-mode nil
  "Live WM-flight organ view."
  :group 'applications)

(defcustom flight-mode-project-dir "/home/joe/code/futon3c"
  "Directory from which to run the flight scope projector."
  :type 'directory :group 'flight-mode)

(defcustom flight-mode-command '("bb" "scripts/flight_scope_view.bb")
  "Command prefix used to fetch flight scope JSON."
  :type '(repeat string) :group 'flight-mode)

(defcustom flight-mode-buffer-name "*flight-mode*"
  "Buffer name for the flight organ view."
  :type 'string :group 'flight-mode)

(defface flight-mode-present-face
  '((t :inherit success)) "Organ present." :group 'flight-mode)
(defface flight-mode-ghost-face
  '((t :inherit shadow)) "Organ not yet present (ghost)." :group 'flight-mode)
(defface flight-mode-warn-face
  '((t :inherit warning)) "Organ in a flagged state (refused/fallback/transient)."
  :group 'flight-mode)

(defvar flight-mode--run-id nil)
(defvar flight-mode--timer nil)

(defun flight-mode--call (&rest args)
  "Run the projector with ARGS; return parsed JSON or nil."
  (let ((default-directory flight-mode-project-dir))
    (with-temp-buffer
      (when (zerop (apply #'call-process (car flight-mode-command) nil t nil
                          (append (cdr flight-mode-command) args)))
        (goto-char (point-min))
        (ignore-errors (json-parse-buffer :object-type 'alist
                                          :array-type 'list))))))

(defun flight-mode--run-ids ()
  (alist-get 'run-ids (flight-mode--call "--list")))

(defun flight-mode--state-face (state)
  (cond ((member state '("present" "measured")) 'flight-mode-present-face)
        ((equal state "ghost") 'flight-mode-ghost-face)
        (t 'flight-mode-warn-face)))

(defun flight-mode--render (data)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Flight %s — %s\n"
                                (alist-get 'run-id data)
                                (if (eq (alist-get 'complete? data) t)
                                    "complete" "IN PROGRESS / partial"))
                        'face 'bold))
    (insert (propertize " keys: g refresh · l choose run · n/p next/prev · RET visit artifact · a auto\n\n"
                        'face 'shadow))
    (dolist (o (alist-get 'organs data))
      (let* ((state (alist-get 'state o))
             (line (format " %-16s %-26s %s\n"
                           (alist-get 'organ o)
                           state
                           (or (alist-get 'value o) ""))))
        (insert (propertize line
                            'face (flight-mode--state-face state)
                            'flight-artifact (alist-get 'artifact o)
                            'flight-anchor (alist-get 'anchor o)))))
    (goto-char (point-min))))

(defun flight-mode-refresh ()
  "Refresh the panel for the current (or latest) run."
  (interactive)
  (let ((data (if flight-mode--run-id
                  (flight-mode--call "--run-id" flight-mode--run-id)
                (flight-mode--call))))
    (when data
      (setq flight-mode--run-id (alist-get 'run-id data))
      (with-current-buffer (get-buffer-create flight-mode-buffer-name)
        (flight-mode--render data)))))

(defun flight-mode-choose-run ()
  "Pick a run-id from the trace directory."
  (interactive)
  (when-let ((rid (completing-read "Flight run: " (flight-mode--run-ids) nil t)))
    (setq flight-mode--run-id rid)
    (flight-mode-refresh)))

(defun flight-mode--step-run (dir)
  (let* ((ids (flight-mode--run-ids))
         (i (or (cl-position flight-mode--run-id ids :test #'equal) 0))
         (j (mod (+ i dir) (length ids))))
    (setq flight-mode--run-id (nth j ids))
    (flight-mode-refresh)))

(defun flight-mode-next-run () (interactive) (flight-mode--step-run 1))
(defun flight-mode-prev-run () (interactive) (flight-mode--step-run -1))

(defun flight-mode-visit-artifact ()
  "Open the artifact behind the organ at point (at its anchor line if known)."
  (interactive)
  (when-let ((f (get-text-property (point) 'flight-artifact)))
    (let ((anchor (get-text-property (point) 'flight-anchor)))
      (find-file-other-window f)
      (when (numberp anchor)
        (goto-char (point-min))
        (forward-line (1- anchor))))))

(defun flight-mode-toggle-auto ()
  "Toggle 5s auto-refresh (for watching a live flight fill in)."
  (interactive)
  (if flight-mode--timer
      (progn (cancel-timer flight-mode--timer)
             (setq flight-mode--timer nil)
             (message "flight-mode auto-refresh off"))
    (setq flight-mode--timer
          (run-with-timer 5 5 (lambda ()
                                (when (get-buffer flight-mode-buffer-name)
                                  (flight-mode-refresh)))))
    (message "flight-mode auto-refresh on (5s)")))

(defvar flight-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'flight-mode-refresh)
    (define-key m (kbd "l") #'flight-mode-choose-run)
    (define-key m (kbd "n") #'flight-mode-next-run)
    (define-key m (kbd "p") #'flight-mode-prev-run)
    (define-key m (kbd "RET") #'flight-mode-visit-artifact)
    (define-key m (kbd "a") #'flight-mode-toggle-auto)
    (define-key m (kbd "q") #'quit-window)
    m))

(define-derived-mode flight-mode-view-mode special-mode "Flight"
  "Major mode for the WM flight organ panel."
  (use-local-map flight-mode-map))

;;;###autoload
(defun flight-mode ()
  "Open the live WM-flight organ view (latest flight by default)."
  (interactive)
  (flight-mode-refresh)
  (with-current-buffer (get-buffer-create flight-mode-buffer-name)
    (flight-mode-view-mode))
  (pop-to-buffer flight-mode-buffer-name))

(provide 'flight-mode)
;;; flight-mode.el ends here
