;;; shadow-control.el --- Embedded shadow-cljs control via Drawbridge -*- lexical-binding: t -*-

;; Drives the futon3c-embedded shadow-cljs server (see
;; dev/futon3c/dev/shadow.clj) so CLJS watches run inside the same JVM
;; that serves the rest of futon3c. Avoids the standalone `npx
;; shadow-cljs' JVMs that CLAUDE.md I-0 forbids at rest.
;;
;; Commands:
;;   M-x start-shadow    — boot embedded server + watch default builds
;;                         (prefix arg prompts for build ids)
;;   M-x stop-shadow     — stop all watches + shut server
;;                         (prefix arg prompts for specific build ids)
;;   M-x shadow-status   — show server/watch state in the echo area

(defcustom shadow-control-drawbridge-host "127.0.0.1"
  "Host where the futon3c Drawbridge eval endpoint is reachable."
  :type 'string :group 'shadow-control)

(defcustom shadow-control-drawbridge-port 6768
  "Port for the futon3c Drawbridge eval endpoint."
  :type 'integer :group 'shadow-control)

(defcustom shadow-control-admintoken-file "/home/joe/code/futon3c/.admintoken"
  "File holding the admin token used to authenticate Drawbridge eval calls."
  :type 'file :group 'shadow-control)

(defcustom shadow-control-build-ids '("war-machine" "webarxana")
  "Default build ids to start when M-x start-shadow is called without a prefix arg.
Each entry corresponds to a :builds key in /home/joe/code/futon3c/shadow-cljs.edn."
  :type '(repeat string) :group 'shadow-control)

(defun shadow-control--admin-token ()
  (when (file-readable-p shadow-control-admintoken-file)
    (with-temp-buffer
      (insert-file-contents shadow-control-admintoken-file)
      (string-trim (buffer-string)))))

(defun shadow-control--eval (form)
  "POST FORM (Clojure source) to Drawbridge; return the response body string."
  (let* ((token (or (shadow-control--admin-token) "change-me"))
         (url (format "http://%s:%d/eval"
                      shadow-control-drawbridge-host
                      shadow-control-drawbridge-port)))
    (with-temp-buffer
      (let ((exit (call-process "curl" nil t nil
                                "-s"
                                "-H" (format "x-admin-token: %s" token)
                                "-H" "Content-Type: text/plain"
                                "-d" form
                                url)))
        (if (zerop exit)
            (string-trim (buffer-string))
          (format "ERROR (curl exit %d): %s" exit (buffer-string)))))))

(defun shadow-control--prompt-build-ids ()
  (let ((default (mapconcat #'identity shadow-control-build-ids " ")))
    (split-string
     (read-string (format "Build IDs (space/comma separated, default %s): "
                          default)
                  nil nil default)
     "[, ]+" t)))

(defun shadow-control--build-ids (arg)
  (if arg (shadow-control--prompt-build-ids) shadow-control-build-ids))

(defun shadow-control--quote-keywords (ids)
  (mapconcat (lambda (id) (format ":%s" id)) ids " "))

;;;###autoload
(defun start-shadow (&optional arg)
  "Boot embedded shadow-cljs and watch the default CLJS builds.
With prefix ARG, prompt for which build ids to watch instead of using
`shadow-control-build-ids'."
  (interactive "P")
  (let* ((ids (shadow-control--build-ids arg))
         (form (format "(do (require '[futon3c.dev.shadow :as s]) (apply s/start! [%s]))"
                       (shadow-control--quote-keywords ids))))
    (message "[shadow-control] starting: %s" (mapconcat #'identity ids " "))
    (message "[shadow-control] %s" (shadow-control--eval form))))

;;;###autoload
(defun stop-shadow (&optional arg)
  "Stop embedded shadow-cljs.
With no ARG, stops every watch and shuts the embedded server down.
With prefix ARG, prompt for specific build ids to stop (server stays up)."
  (interactive "P")
  (let* ((ids (when arg (shadow-control--prompt-build-ids)))
         (form (if ids
                   (format "(do (require '[futon3c.dev.shadow :as s]) (apply s/stop! [%s]))"
                           (shadow-control--quote-keywords ids))
                 "(do (require '[futon3c.dev.shadow :as s]) (s/stop!))")))
    (message "[shadow-control] stopping: %s" (or ids "all + server"))
    (message "[shadow-control] %s" (shadow-control--eval form))))

;;;###autoload
(defun shadow-status ()
  "Show embedded shadow-cljs server + watch state."
  (interactive)
  (message "[shadow-control] %s"
           (shadow-control--eval
            "(do (require '[futon3c.dev.shadow :as s]) (s/status))")))

(provide 'shadow-control)

;;; shadow-control.el ends here
