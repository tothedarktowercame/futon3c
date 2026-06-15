;;; futon-branches.el --- At-a-glance git branch overview across tracked repos -*- lexical-binding: t; -*-

;; A *branches* buffer, in the spirit of *agents*: one row per tracked repo
;; showing the checked-out branch, working-tree state, ahead/behind vs upstream,
;; and the last commit. Unlike *agents* (a JVM/blackboard-driven HUD sink), this
;; is fully self-contained — it only shells out to git — so it works even when
;; the futon stack is down.
;;
;; M-x futon-branches  → open/refresh the overview.  `g` refreshes.  RET / `d`
;; on a row opens that repo (dired / magit-status if available).
;;
;; Tracked repos default to every git repo matching `futon-branches-root'/
;; `futon-branches-glob' (~/code/futon*) plus `futon-branches-extra-repos'
;; (~/vsat, ~/code/ukrn-services-simulation). Customize to taste.

;;; Code:

(require 'tabulated-list)
(require 'subr-x)

(defgroup futon-branches nil
  "At-a-glance git branch overview across tracked repositories."
  :group 'tools
  :prefix "futon-branches-")

(defcustom futon-branches-root "~/code"
  "Directory under which `futon-branches-glob' auto-discovers repos."
  :type 'directory)

(defcustom futon-branches-glob "futon*"
  "Glob, relative to `futon-branches-root', for auto-discovered repos."
  :type 'string)

(defcustom futon-branches-extra-repos '("~/vsat" "~/code/ukrn-services-simulation")
  "Repos to track in addition to the auto-discovered `futon-branches-glob' set."
  :type '(repeat directory))

(defun futon-branches--git (repo &rest args)
  "Run git ARGS in REPO; return trimmed non-empty stdout, or nil on failure.
Stderr is discarded so a missing upstream / non-repo is just nil."
  (let ((dir (expand-file-name repo)))
    (when (file-directory-p dir)
      (let ((default-directory (file-name-as-directory dir)))
        (with-temp-buffer
          (when (eq 0 (apply #'process-file "git" nil '(t nil) nil args))
            (let ((s (string-trim (buffer-string))))
              (unless (string-empty-p s) s))))))))

(defun futon-branches--git-repo-p (repo)
  "Non-nil when REPO is inside a git work tree."
  (equal "true" (futon-branches--git repo "rev-parse" "--is-inside-work-tree")))

(defun futon-branches--repos ()
  "Return the de-duplicated list of existing git repos to display."
  (let* ((discovered (file-expand-wildcards
                      (expand-file-name futon-branches-glob futon-branches-root)))
         (all (append discovered (mapcar #'expand-file-name futon-branches-extra-repos)))
         (repos (seq-uniq (mapcar (lambda (p) (directory-file-name (expand-file-name p))) all)
                          #'string=)))
    (sort (seq-filter #'futon-branches--git-repo-p repos) #'string<)))

(defun futon-branches--truncate (s n)
  (if (and s (> (length s) n)) (concat (substring s 0 (1- n)) "…") (or s "-")))

(defun futon-branches--ahead-behind (repo)
  "Return a propertized ahead/behind cell for REPO vs its upstream."
  (let ((out (futon-branches--git repo "rev-list" "--left-right" "--count"
                                  "HEAD...@{upstream}")))
    (if (not out)
        (propertize "no upstream" 'face 'shadow)
      (let* ((parts (split-string out))
             (a (string-to-number (or (nth 0 parts) "0")))
             (b (string-to-number (or (nth 1 parts) "0"))))
        (if (and (zerop a) (zerop b))
            (propertize "= in sync" 'face 'shadow)
          (propertize (format "↑%d ↓%d" a b) 'face 'warning))))))

(defun futon-branches--row (repo)
  "Build a `tabulated-list-entries' row for REPO."
  (let* ((name (file-name-nondirectory repo))
         (branch (futon-branches--git repo "symbolic-ref" "--short" "HEAD"))
         (sha (futon-branches--git repo "rev-parse" "--short" "HEAD"))
         (branch-str (cond (branch branch)
                           (sha (format "(detached %s)" sha))
                           (t "?")))
         (porcelain (futon-branches--git repo "status" "--porcelain"))
         (ndirty (if porcelain (length (split-string porcelain "\n" t)) 0))
         (state (if (> ndirty 0)
                    (propertize (format "✗ %d dirty" ndirty) 'face 'error)
                  (propertize "✓ clean" 'face 'success)))
         (last (futon-branches--git repo "log" "-1" "--format=%h  %s")))
    (list repo
          (vector (propertize name 'face 'bold)
                  (propertize branch-str 'face (if branch 'default 'error))
                  state
                  (futon-branches--ahead-behind repo)
                  (futon-branches--truncate last 60)))))

(defun futon-branches--refresh ()
  "Recompute `tabulated-list-entries' from the tracked repos."
  (setq tabulated-list-entries
        (mapcar #'futon-branches--row (futon-branches--repos))))

(defun futon-branches-visit ()
  "Open the repo on the current row (magit-status if available, else dired)."
  (interactive)
  (when-let ((repo (tabulated-list-get-id)))
    (if (fboundp 'magit-status)
        (magit-status repo)
      (dired repo))))

(defvar futon-branches-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'futon-branches-visit)
    (define-key map (kbd "d") #'futon-branches-visit)
    map)
  "Keymap for `futon-branches-mode'.")

(define-derived-mode futon-branches-mode tabulated-list-mode "Branches"
  "Major mode for the *branches* repo branch overview.
\\{futon-branches-mode-map}"
  (setq tabulated-list-format
        [("Repo" 24 t)
         ("Branch" 26 t)
         ("State" 12 t)
         ("Sync" 12 nil)
         ("Last commit" 60 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Repo" nil))
  (add-hook 'tabulated-list-revert-hook #'futon-branches--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun futon-branches ()
  "Show the checked-out branch + status of each tracked repo in *branches*.
Refresh with `g'."
  (interactive)
  (let ((buf (get-buffer-create "*branches*")))
    (with-current-buffer buf
      (futon-branches-mode)
      (futon-branches--refresh)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(provide 'futon-branches)
;;; futon-branches.el ends here
