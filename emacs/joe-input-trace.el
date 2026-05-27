;;; joe-input-trace.el --- Command-loop / display latency logger -*- lexical-binding: t -*-

;; Instrument the command loop to capture per-command timestamps, the
;; key bound, the gap since the previous post-command, whether input
;; is still pending, and GC time consumed. Designed for diagnosing
;; perceived "1-2-3-crocodile" delays between key-press and visible
;; effect: the gap between sequential pre-command-hook firings reveals
;; command-loop stalls, and the idle-after marker reveals redisplay
;; latency relative to command completion.
;;
;; Records accumulate in an in-memory buffer (no per-event disk I/O,
;; so the logger itself adds minimal overhead). Dump to disk with
;; M-x joe-input-trace-dump.
;;
;; Usage:
;;   M-x joe-input-trace-enable    ; start logging
;;   ... work normally ...
;;   M-x joe-input-trace-dump      ; flush *joe-input-trace* buffer to /tmp file
;;   M-x joe-input-trace-disable   ; stop logging
;;   M-x joe-input-trace-clear     ; wipe the in-memory buffer

(defvar joe-input-trace--enabled nil)
(defvar joe-input-trace--buffer-name "*joe-input-trace*")
(defvar joe-input-trace--seq 0)
(defvar joe-input-trace--last-post-ms nil)

(defun joe-input-trace--now-ms ()
  (truncate (* 1000 (float-time))))

(defun joe-input-trace--ensure-buffer ()
  (or (get-buffer joe-input-trace--buffer-name)
      (let ((buf (get-buffer-create joe-input-trace--buffer-name)))
        (with-current-buffer buf
          (setq buffer-undo-list t)
          (setq-local inhibit-read-only t))
        buf)))

(defun joe-input-trace--append (record)
  (when joe-input-trace--enabled
    (condition-case err
        (let ((print-length nil) (print-level nil))
          (with-current-buffer (joe-input-trace--ensure-buffer)
            (goto-char (point-max))
            (insert (prin1-to-string record) "\n")))
      (error
       (message "[joe-input-trace] append error: %s"
                (error-message-string err))))))

(defun joe-input-trace--pre ()
  (condition-case err
      (let* ((now (joe-input-trace--now-ms))
             (gap (when joe-input-trace--last-post-ms
                    (- now joe-input-trace--last-post-ms))))
        (setq joe-input-trace--seq (1+ joe-input-trace--seq))
        (joe-input-trace--append
         `((seq . ,joe-input-trace--seq)
           (phase . pre)
           (at-ms . ,now)
           (gap-since-prev-post-ms . ,gap)
           (cmd . ,this-command)
           (keys . ,(condition-case _ (key-description (this-command-keys))
                      (error nil)))
           (input-pending . ,(input-pending-p))
           (buf . ,(buffer-name))
           (point . ,(point))
           (gc-elapsed . ,gc-elapsed))))
    (error
     (message "[joe-input-trace] pre-hook error: %s"
              (error-message-string err)))))

(defun joe-input-trace--post ()
  (condition-case err
      (let ((post-at (joe-input-trace--now-ms))
            (my-seq joe-input-trace--seq))
        (joe-input-trace--append
         `((seq . ,my-seq)
           (phase . post)
           (at-ms . ,post-at)
           (cmd . ,this-command)
           (buf . ,(buffer-name))
           (point . ,(point))
           (gc-elapsed . ,gc-elapsed)
           (input-pending . ,(input-pending-p))))
        (setq joe-input-trace--last-post-ms post-at)
        ;; The 0-delay run-at-time fires once Emacs is next idle, which is
        ;; immediately after this command-loop turn (including any inline
        ;; redisplay). Its at-ms minus post-at approximates the
        ;; post-command → next-idle gap, i.e. inline redisplay duration.
        (run-at-time
         0 nil
         (lambda ()
           (joe-input-trace--append
            `((seq . ,my-seq)
              (phase . idle-after)
              (at-ms . ,(joe-input-trace--now-ms))
              (since-post-ms . ,(- (joe-input-trace--now-ms) post-at)))))))
    (error
     (message "[joe-input-trace] post-hook error: %s"
              (error-message-string err)))))

(defvar joe-input-trace--filter-wraps
  '(eat--filter
    url-http-generic-filter
    jsonrpc--process-filter
    comint-output-filter
    internal-default-process-filter
    url-retrieve-synchronously)
  "Symbol-level functions to time via around-advice when tracing is enabled.
Process filters bypass `pre-command-hook' and `timer-event-handler', so a
multi-second filter call is invisible without this wrap. Synchronous URL
calls block in `accept-process-output' and look the same as a stalled
loop; wrapping `url-retrieve-synchronously' catches that case.")

(defun joe-input-trace--make-filter-advice (fn-symbol)
  "Return an around-advice that logs FN-SYMBOL's timing."
  (lambda (orig-fn &rest args)
    (if (not joe-input-trace--enabled)
        (apply orig-fn args)
      (let ((start-ms (joe-input-trace--now-ms)))
        (unwind-protect
            (apply orig-fn args)
          (condition-case err
              (let ((end-ms (joe-input-trace--now-ms)))
                (joe-input-trace--append
                 `((phase . filter)
                   (at-ms . ,start-ms)
                   (elapsed-ms . ,(- end-ms start-ms))
                   (fn . ,fn-symbol))))
            (error
             (message "[joe-input-trace] filter-trace error: %s"
                      (error-message-string err)))))))))

(defvar joe-input-trace--installed-filter-advice nil
  "Alist (FN-SYMBOL . ADVICE-FN) of installed wraps, so disable can clean up.")

(defun joe-input-trace--wrap-filters ()
  (setq joe-input-trace--installed-filter-advice nil)
  (dolist (sym joe-input-trace--filter-wraps)
    (when (fboundp sym)
      (let ((advice (joe-input-trace--make-filter-advice sym)))
        (advice-add sym :around advice)
        (push (cons sym advice) joe-input-trace--installed-filter-advice)))))

(defun joe-input-trace--unwrap-filters ()
  (dolist (pair joe-input-trace--installed-filter-advice)
    (advice-remove (car pair) (cdr pair)))
  (setq joe-input-trace--installed-filter-advice nil))

(defun joe-input-trace--around-timer (orig-fn timer &rest args)
  "Wrap `timer-event-handler' to log per-timer firing function + duration.
Logs even when tracing is disabled? No — gated by `joe-input-trace--enabled'.
On any internal error, the original timer still runs (unwind-protect)."
  (if (not joe-input-trace--enabled)
      (apply orig-fn timer args)
    (let* ((start-ms (joe-input-trace--now-ms))
           (fn (condition-case _ (timer--function timer)
                 (error 'unknown)))
           (idle-delay (condition-case _ (timer--idle-delay timer)
                         (error nil))))
      (unwind-protect
          (apply orig-fn timer args)
        (condition-case err
            (let ((end-ms (joe-input-trace--now-ms)))
              (joe-input-trace--append
               `((phase . timer)
                 (at-ms . ,start-ms)
                 (elapsed-ms . ,(- end-ms start-ms))
                 (fn . ,(if (byte-code-function-p fn) 'byte-compiled fn))
                 (idle-delay . ,idle-delay))))
          (error
           (message "[joe-input-trace] timer-trace error: %s"
                    (error-message-string err))))))))

(defun joe-input-trace-enable ()
  "Start command-loop tracing."
  (interactive)
  (setq joe-input-trace--enabled t)
  (setq joe-input-trace--seq 0)
  (setq joe-input-trace--last-post-ms nil)
  (add-hook 'pre-command-hook  #'joe-input-trace--pre)
  (add-hook 'post-command-hook #'joe-input-trace--post)
  (advice-add 'timer-event-handler :around #'joe-input-trace--around-timer)
  (joe-input-trace--wrap-filters)
  (joe-input-trace--append
   `((phase . enable)
     (at-ms . ,(joe-input-trace--now-ms))
     (filter-wraps . ,(mapcar #'car joe-input-trace--installed-filter-advice))))
  (message "[joe-input-trace] enabled (records → *joe-input-trace* buffer)"))

(defun joe-input-trace-disable ()
  "Stop command-loop tracing."
  (interactive)
  (joe-input-trace--append
   `((phase . disable) (at-ms . ,(joe-input-trace--now-ms))))
  (setq joe-input-trace--enabled nil)
  (remove-hook 'pre-command-hook  #'joe-input-trace--pre)
  (remove-hook 'post-command-hook #'joe-input-trace--post)
  (advice-remove 'timer-event-handler #'joe-input-trace--around-timer)
  (joe-input-trace--unwrap-filters)
  (message "[joe-input-trace] disabled"))

(defun joe-input-trace-dump (&optional file)
  "Write the trace buffer to FILE (default /tmp/joe_input_trace_<ms>.log)."
  (interactive)
  (let* ((path (or file (format "/tmp/joe_input_trace_%d.log"
                                (joe-input-trace--now-ms))))
         (size (with-current-buffer (joe-input-trace--ensure-buffer)
                 (buffer-size))))
    (with-current-buffer (joe-input-trace--ensure-buffer)
      (write-region (point-min) (point-max) path nil 'silent))
    (message "[joe-input-trace] dumped %d bytes → %s" size path)
    path))

(defun joe-input-trace-clear ()
  "Wipe the in-memory trace buffer."
  (interactive)
  (with-current-buffer (joe-input-trace--ensure-buffer)
    (let ((inhibit-read-only t)) (erase-buffer)))
  (message "[joe-input-trace] cleared"))

(defun joe-input-trace-bench (buffer-name &optional iters key)
  "Bench keystroke→redisplay-complete latency in BUFFER-NAME.
ITERS defaults to 10. KEY defaults to SPC.

Synthesizes ITERS keystrokes through `execute-kbd-macro' so the full
command loop runs (pre/post-command-hook, cursor-sensor--detect,
etc.). Forces synchronous redisplay after each. Measures wall-clock
elapsed per iter.

Restores window configuration and deletes any inserted text on exit.
Returns a list of plists with :iter :elapsed-ms :point."
  (interactive "bBench buffer: ")
  (let* ((iters (or iters 10))
         (key (or key (kbd "SPC")))
         (buf (get-buffer buffer-name))
         (results nil))
    (unless buf (user-error "joe-input-trace-bench: no buffer %s" buffer-name))
    (save-window-excursion
      (switch-to-buffer buf)
      (redisplay t)
      (with-current-buffer buf
        (let ((start-point (point))
              (inhibit-read-only t))
          (unwind-protect
              (dotimes (i iters)
                (let ((t0 (current-time)))
                  (execute-kbd-macro key)
                  (redisplay t)
                  (let ((elapsed-ms
                         (truncate (* 1000 (float-time (time-since t0))))))
                    (push (list :iter i
                                :elapsed-ms elapsed-ms
                                :point (point))
                          results))))
            (when (> (point) start-point)
              (delete-region start-point (point)))))))
    (nreverse results)))

(defun joe-input-trace-bench-summary (results)
  "Reduce RESULTS from `joe-input-trace-bench' to a one-line summary plist."
  (let* ((times (mapcar (lambda (r) (plist-get r :elapsed-ms)) results))
         (n (length times))
         (sorted (sort (copy-sequence times) #'<))
         (sum (apply #'+ times))
         (median (nth (/ n 2) sorted)))
    (list :n n
          :min (car sorted)
          :median median
          :mean (if (zerop n) 0 (/ sum n))
          :max (car (last sorted))
          :raw-ms times)))

(defun joe-input-trace--find-sensor-regions (&optional max)
  "Return up to MAX (start . end) pairs of regions in current buffer
where `cursor-sensor-functions' is non-nil. Pairs are returned in
buffer order; default MAX is 20."
  (let ((max (or max 20))
        (p (point-min))
        regions)
    (while (and (< p (point-max)) (< (length regions) max))
      (let ((next (next-single-property-change
                   p 'cursor-sensor-functions nil (point-max))))
        (when (get-text-property p 'cursor-sensor-functions)
          (push (cons p next) regions))
        (setq p next)))
    (nreverse regions)))

(defun joe-input-trace-bench-sensor-cycle (buffer-name &optional iters)
  "Bench cursor-sensor entry/exit latency in BUFFER-NAME.
For each of ITERS iters (default 5), pick a tool-overlay region,
measure: jump-to-before, jump-into-region (sensor `entered' fires),
jump-out-of-region (sensor `left' fires), each followed by
synchronous redisplay.

Returns a list of plists per iter with the four phase latencies
in ms: :baseline-ms :enter-ms :leave-ms :total-ms."
  (interactive "bBench sensor cycle in buffer: ")
  (let* ((iters (or iters 5))
         (buf (get-buffer buffer-name))
         results)
    (unless buf (user-error "no buffer: %s" buffer-name))
    (save-window-excursion
      (switch-to-buffer buf)
      (with-current-buffer buf
        (let ((regions (joe-input-trace--find-sensor-regions iters))
              (orig-point (point)))
          (unless regions
            (user-error "no cursor-sensor-functions regions in %s" buffer-name))
          (unwind-protect
              (dolist (region regions)
                (let* ((before (max (point-min) (- (car region) 1)))
                       (inside (max (car region) (min (1- (cdr region))
                                                       (+ (car region) 1))))
                       (after (min (point-max) (1+ (cdr region))))
                       (t-start (current-time)))
                  ;; baseline: jump to just-before
                  (goto-char before) (redisplay t)
                  (let ((t-baseline (current-time)))
                    ;; enter: jump into the region (sensor 'entered fires)
                    (goto-char inside) (redisplay t)
                    (let ((t-enter (current-time)))
                      ;; leave: jump past the region (sensor 'left fires)
                      (goto-char after) (redisplay t)
                      (let ((t-leave (current-time)))
                        (push (list :region region
                                    :baseline-ms (truncate
                                                  (* 1000 (float-time
                                                           (time-subtract t-baseline t-start))))
                                    :enter-ms (truncate
                                               (* 1000 (float-time
                                                        (time-subtract t-enter t-baseline))))
                                    :leave-ms (truncate
                                               (* 1000 (float-time
                                                        (time-subtract t-leave t-enter))))
                                    :total-ms (truncate
                                               (* 1000 (float-time
                                                        (time-since t-start)))))
                              results))))))
            (goto-char orig-point)))))
    (nreverse results)))

(defun joe-input-trace-bench-compare (buffer-names &optional iters)
  "Bench each buffer in BUFFER-NAMES and return per-buffer summaries.
Each entry is (buffer-name . summary-plist)."
  (let ((iters (or iters 10))
        results)
    (dolist (name buffer-names)
      (when (get-buffer name)
        (let ((bench (joe-input-trace-bench name iters)))
          (push (cons name (joe-input-trace-bench-summary bench)) results))))
    (nreverse results)))

(provide 'joe-input-trace)

;;; joe-input-trace.el ends here
