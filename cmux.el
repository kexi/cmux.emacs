;;; cmux.el --- Send file references to AI CLI on cmux -*- lexical-binding: t -*-

;; Author: Kei Nakayama <kei.of.nakayama@gmail.com>
;; Maintainer: Kei Nakayama <kei.of.nakayama@gmail.com>
;; URL: https://github.com/kexi/cmux.emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; cmux.el sends file-path references from Emacs to an AI CLI (Claude
;; Code or Gemini CLI) that is running in another cmux surface.
;;
;; Originally derived from tanabee/cmux.vim.  Ported to Emacs Lisp.
;;
;; Usage: open two tabs in cmux, one with Emacs and one with the AI
;; CLI, then invoke `cmux-send-file', `cmux-send-pos' or
;; `cmux-send-range'.  The target surface is auto-detected on first
;; use via `cmux-detect', and may be set manually with
;; `cmux-set-surface'.
;;
;; All process invocations go through `call-process' with absolute
;; executable paths and list-form arguments; no shell interpolation
;; happens at any point.  TRAMP buffers are refused.

;;; Code:

(require 'subr-x)
(require 'vc-git)

(declare-function vc-git-root "vc-git" (file))

(defgroup cmux nil
  "Send file references from Emacs to AI CLI on cmux."
  :group 'tools
  :prefix "cmux-")

(defcustom cmux-executable "cmux"
  "Name or absolute path of the cmux CLI executable."
  :type 'string
  :group 'cmux)

(defcustom cmux-surface nil
  "Target cmux surface reference, e.g. \"surface:1\".
Set automatically by `cmux-detect' or manually via
`cmux-set-surface'.  nil means no surface has been selected."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'cmux)

(defcustom cmux-cli-name nil
  "Human-readable name of the detected AI CLI, or nil.
This value is filled in by `cmux-detect' and should be treated as
read-only by user configuration."
  :type '(choice (const :tag "Unknown" nil) string)
  :group 'cmux)

(defcustom cmux-auto-focus t
  "When non-nil, focus the target surface pane after sending."
  :type 'boolean
  :group 'cmux)

(defcustom cmux-auto-enter nil
  "When non-nil, send the Enter key after the reference is sent."
  :type 'boolean
  :group 'cmux)

(defcustom cmux-read-screen-lines 30
  "Number of screen lines to read when detecting AI CLIs."
  :type 'integer
  :group 'cmux)

(defcustom cmux-detect-patterns
  '(("Claude Code" . "claude\\|Claude Code\\|anthropic\\|/help\\|╭\\|╰")
    ("Gemini CLI"  . "gemini\\|Gemini\\|✦"))
  "Alist of (CLI-NAME . REGEXP) used by `cmux-detect'.
Each REGEXP is matched against the screen contents of a surface
with `string-match-p'.  Keep regexps simple to avoid ReDoS."
  :type '(alist :key-type string :value-type regexp)
  :group 'cmux)

(defcustom cmux-path-style 'git-root
  "Strategy used to build the path that is sent to the AI CLI.
`git-root'       -- path relative to the enclosing git worktree.
`absolute'       -- the buffer's absolute file name.
`relative-home'  -- `abbreviate-file-name' applied to the absolute path."
  :type '(choice (const git-root)
                 (const absolute)
                 (const relative-home))
  :group 'cmux)

(defcustom cmux-path-style-fallback 'absolute-with-warning
  "Fallback when `cmux-path-style' is `git-root' but no git root exists.
`absolute-with-warning' -- fall back to the absolute path and warn.
`error'                 -- signal a `user-error'.
`relative-home'         -- fall back to `abbreviate-file-name'."
  :type '(choice (const absolute-with-warning)
                 (const error)
                 (const relative-home))
  :group 'cmux)

(defcustom cmux-surface-env-trust t
  "When non-nil, honor the CMUX_SURFACE_ID environment variable.
The value is still validated by `cmux--valid-surface-p' before use."
  :type 'boolean
  :group 'cmux)

(defcustom cmux-debug nil
  "When non-nil, write diagnostic messages to the *cmux-log* buffer."
  :type 'boolean
  :group 'cmux)

(defcustom cmux-call-timeout 3
  "Timeout, in seconds, for each synchronous cmux CLI invocation."
  :type 'number
  :group 'cmux)

(defcustom cmux-log-max-lines 1000
  "Upper bound on the number of lines kept in the *cmux-log* buffer."
  :type 'integer
  :group 'cmux)

(defconst cmux--surface-regexp "\\`surface:[0-9]+\\'"
  "Regexp that a surface reference must match exactly.")

(defconst cmux--ansi-regexp
  "\x1b\\[[0-9;?]*[ -/]*[@-~]"
  "Regexp matching CSI ANSI escape sequences.")

(defconst cmux--control-chars-regexp "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]"
  "Regexp matching control characters (excluding TAB, LF, CR).")

;;;; Logging and messaging

(defun cmux--log (fmt &rest args)
  "Append a formatted line to *cmux-log* when `cmux-debug' is non-nil.
FMT and ARGS are passed to `format'."
  (when cmux-debug
    (let ((buf (get-buffer-create "*cmux-log*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert (apply #'format fmt args))
        (insert "\n")
        (when (and (integerp cmux-log-max-lines)
                   (> cmux-log-max-lines 0))
          (let ((excess (- (count-lines (point-min) (point-max))
                           cmux-log-max-lines)))
            (when (> excess 0)
              (goto-char (point-min))
              (forward-line excess)
              (delete-region (point-min) (point)))))))))

(defun cmux--message (fmt &rest args)
  "Show a cmux.el-prefixed message.
FMT and ARGS are passed to `format'."
  (message "cmux.el: %s" (apply #'format fmt args)))

;;;; String sanitization

(defun cmux--strip-ansi (str)
  "Return STR with ANSI CSI escape sequences removed."
  (if (stringp str)
      (replace-regexp-in-string cmux--ansi-regexp "" str)
    ""))

(defun cmux--strip-controls (str)
  "Return STR with control characters removed."
  (if (stringp str)
      (replace-regexp-in-string cmux--control-chars-regexp "" str)
    ""))

(defun cmux--truncate-for-error (str)
  "Sanitize STR for inclusion in a `user-error' message.
Strips ANSI escapes and control characters, then truncates to 500
characters."
  (let* ((cleaned (cmux--strip-controls (cmux--strip-ansi (or str "")))))
    (if (> (length cleaned) 500)
        (concat (substring cleaned 0 500) "...")
      cleaned)))

(defun cmux--sanitize-path (path)
  "Validate PATH; signal `user-error' when unsafe.
Reject PATH when it contains any byte in the range [\\x00-\\x1f\\x7f].
This range includes NUL, TAB (0x09), LF (0x0a), CR (0x0d), ESC (0x1b),
and DEL (0x7f); all of them are rejected because an @mention payload
sent to the AI CLI must be a single line of printable characters.
Note: `cmux--strip-controls' is deliberately more permissive and
*preserves* TAB/LF/CR for log / error-message sanitization purposes;
only this function (used for outbound paths) treats them as unsafe.
Return PATH unchanged when it is safe."
  (unless (stringp path)
    (user-error "Cmux.el: Buffer has no associated file path"))
  (when (string-match-p "[\x00-\x1f\x7f]" path)
    (user-error "Cmux.el: Refusing path containing control characters"))
  path)

;;;; Surface validation

(defun cmux--valid-surface-p (str)
  "Return non-nil when STR is a syntactically valid surface reference.
Performs control-character rejection (NUL / TAB / LF / CR / DEL etc.)
on the raw input *before* trimming, then applies length check and a
case-sensitive anchored regexp match.  This is the single source of
truth for surface validation.  Note: embedded control characters must
be rejected before trimming, because `string-trim' silently strips
trailing CR / LF / TAB and would otherwise let them slip through."
  (and (stringp str)
       (not (string-match-p "[\x00-\x1f\x7f]" str))
       (let ((s (string-trim str))
             (case-fold-search nil))
         (and (> (length s) 0)
              (<= (length s) 64)
              (string-match-p cmux--surface-regexp s)))))

(defun cmux--coerce-surface (str)
  "Return a trimmed surface string from STR when valid, else nil."
  (when (cmux--valid-surface-p str)
    (string-trim str)))

;;;; Executable resolution and locality checks

(defun cmux--ensure-executable ()
  "Return the absolute path of `cmux-executable', or signal `user-error'."
  (let* ((name cmux-executable)
         (resolved (and (stringp name) (executable-find name))))
    (unless resolved
      (user-error "Cmux.el: cmux executable not found (cmux-executable=%S)"
                  name))
    resolved))

(defun cmux--ensure-local ()
  "Signal `user-error' if the current buffer is remote (TRAMP)."
  (when (or (and buffer-file-name (file-remote-p buffer-file-name))
            (file-remote-p default-directory))
    (user-error "Cmux.el: Does not support remote buffers")))

;;;; Subprocess invocation

(defun cmux--call (exe &rest args)
  "Run EXE with ARGS synchronously, capturing stdout and stderr together.
Return a cons (EXIT-CODE . OUTPUT-STRING).  Signal `user-error' on
timeout.  EXE must be an absolute path; ARGS must be strings."
  (cmux--log "call: %s %S" exe args)
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (process-environment process-environment)
           exit)
      (setq exit
            (with-timeout (cmux-call-timeout
                           (user-error
                            "Cmux.el: %s timed out after %s seconds"
                            (file-name-nondirectory exe)
                            cmux-call-timeout))
              (apply #'call-process exe nil t nil args)))
      (cons exit (buffer-string)))))

(defun cmux--call-with-separate-stderr (exe &rest args)
  "Run EXE with ARGS synchronously; capture stdout and stderr separately.
Return (EXIT-CODE . (STDOUT . STDERR))."
  (cmux--log "call(split): %s %S" exe args)
  (let ((stderr-file (make-temp-file "cmux-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((coding-system-for-read 'utf-8)
                 (coding-system-for-write 'utf-8)
                 exit stderr)
            (setq exit
                  (with-timeout (cmux-call-timeout
                                 (user-error
                                  "Cmux.el: %s timed out after %s seconds"
                                  (file-name-nondirectory exe)
                                  cmux-call-timeout))
                    (apply #'call-process exe nil
                           (list t stderr-file) nil args)))
            (setq stderr
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8))
                      (insert-file-contents stderr-file))
                    (buffer-string)))
            (cons exit (cons (buffer-string) stderr))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun cmux--call-checked (exe &rest args)
  "Run EXE with ARGS; return stdout as a string.
Signal `user-error' when the exit code is non-zero."
  (let* ((res (apply #'cmux--call exe args))
         (exit (car res))
         (out (cdr res)))
    (unless (eq exit 0)
      (user-error "Cmux.el: %s failed (exit %s): %s"
                  (file-name-nondirectory exe)
                  exit
                  (cmux--truncate-for-error out)))
    out))

;;;; Surface discovery

(defun cmux--parse-tree (output)
  "Parse OUTPUT from `cmux tree' into a list of (PANE . SURFACE) pairs."
  (let ((result nil)
        (current-pane nil))
    (dolist (line (split-string (or output "") "\n"))
      (when (string-match "pane:[0-9]+" line)
        (setq current-pane (match-string 0 line)))
      (when (and current-pane (string-match "surface:[0-9]+" line))
        (push (cons current-pane (match-string 0 line)) result)))
    (nreverse result)))

(defun cmux--my-surface-ref (exe)
  "Return this Emacs process's own surface ref using EXE, or nil.
First tries `cmux identify'; falls back to CMUX_SURFACE_ID when
`cmux-surface-env-trust' is non-nil."
  (let* ((res (cmux--call exe "identify"))
         (exit (car res))
         (out (cdr res)))
    (or (and (eq exit 0)
             (string-match "surface:[0-9]+" out)
             (let ((ref (match-string 0 out)))
               (cmux--coerce-surface ref)))
        (and cmux-surface-env-trust
             (cmux--coerce-surface (getenv "CMUX_SURFACE_ID"))))))

(defun cmux--read-screen (exe surface lines)
  "Return the last LINES rows of SURFACE's screen using EXE.
Return nil on failure."
  (let* ((res (cmux--call-with-separate-stderr
               exe "read-screen"
               "--surface" surface
               "--lines" (number-to-string lines)))
         (exit (car res))
         (stdout (cadr res)))
    (when (eq exit 0) stdout)))

(defun cmux--match-cli (screen)
  "Return the CLI name whose regexp matches SCREEN, or nil.
Regexps come from `cmux-detect-patterns'."
  (catch 'hit
    (dolist (entry cmux-detect-patterns)
      (let ((name (car entry))
            (re (cdr entry)))
        (when (and (stringp re)
                   (string-match-p re (or screen "")))
          (throw 'hit name))))
    nil))

;;;; Path building

(defun cmux--git-root (file)
  "Return the absolute git root of FILE, or nil."
  (when (and file (not (file-remote-p file)))
    (let ((root (ignore-errors (vc-git-root file))))
      (and root (expand-file-name root)))))

(defun cmux--path-from-style (abs-path style)
  "Return ABS-PATH formatted per STYLE.
STYLE is one of the values accepted by `cmux-path-style'."
  (pcase style
    ('absolute abs-path)
    ('relative-home (abbreviate-file-name abs-path))
    ('git-root
     (let ((root (cmux--git-root abs-path)))
       (cond
        ((and root (string-prefix-p root abs-path))
         (substring abs-path (length root)))
        (t
         (pcase cmux-path-style-fallback
           ('error
            (user-error "Cmux.el: Not inside a git repository"))
           ('relative-home (abbreviate-file-name abs-path))
           (_
            (display-warning
             'cmux
             (format "not in a git repository; using absolute path for %s"
                     (abbreviate-file-name abs-path))
             :warning)
            abs-path))))))
    (_ abs-path)))

(defun cmux--relative-path ()
  "Return the path to send for the current buffer, per `cmux-path-style'."
  (unless buffer-file-name
    (user-error "Cmux.el: No file associated with this buffer"))
  (cmux--ensure-local)
  (let ((abs (cmux--sanitize-path (expand-file-name buffer-file-name))))
    (cmux--path-from-style abs cmux-path-style)))

(defun cmux--format-line-info (beg end)
  "Return the \":L<n>\" or \":L<n>-<m>\" suffix for region BEG..END."
  (let ((l1 (line-number-at-pos beg))
        (l2 (line-number-at-pos end)))
    (if (= l1 l2)
        (format ":L%d" l1)
      (format ":L%d-%d" l1 l2))))

;;;; Orchestration

(defun cmux--ensure-surface (exe)
  "Return a valid surface ref using EXE, running detection if needed.
Signal `user-error' when no surface can be determined."
  (let ((candidate
         (or (cmux--coerce-surface cmux-surface)
             (and cmux-surface-env-trust
                  (cmux--coerce-surface (getenv "CMUX_SURFACE_ID"))))))
    (unless candidate
      (cmux--detect-internal exe)
      (setq candidate (cmux--coerce-surface cmux-surface)))
    (unless candidate
      (user-error
       "Cmux.el: No target surface; run M-x cmux-detect or cmux-set-surface"))
    candidate))

(defun cmux--focus-surface (exe surface)
  "Focus the pane containing SURFACE using EXE."
  (let* ((res (cmux--call exe "tree"))
         (exit (car res))
         (out (cdr res)))
    (when (eq exit 0)
      (let ((pane (car (rassoc surface (cmux--parse-tree out)))))
        (when pane
          (cmux--call exe "focus-pane" "--pane" pane))))))

(defun cmux--send-ref (path line-info)
  "Send @PATH with LINE-INFO suffix to the configured surface.
This is the internal orchestration routine.  PATH must already
have been validated by `cmux--sanitize-path'."
  (let* ((exe (cmux--ensure-executable))
         (surface (cmux--ensure-surface exe))
         (text (concat "@" path line-info " ")))
    (cmux--log "send-ref: surface=%s text=%S" surface text)
    ;; Clear current input (Ctrl-U) before sending the new reference.
    (cmux--call exe "send-key" "--surface" surface "ctrl+u")
    (let* ((res (cmux--call exe "send" "--surface" surface text))
           (exit (car res))
           (out (cdr res)))
      (unless (eq exit 0)
        (user-error "Cmux.el: Send failed (exit %s): %s"
                    exit (cmux--truncate-for-error out))))
    (when cmux-auto-enter
      (cmux--call exe "send-key" "--surface" surface "enter"))
    (when cmux-auto-focus
      (cmux--focus-surface exe surface))
    (cmux--message "Sent -> @%s%s" path line-info)))

(defun cmux--detect-internal (exe)
  "Scan surfaces with EXE and set `cmux-surface'/`cmux-cli-name' on match."
  (let* ((my-surface (cmux--my-surface-ref exe))
         (tree-out (cmux--call-checked exe "tree"))
         (surfaces (mapcar #'cdr (cmux--parse-tree tree-out)))
         (found nil))
    (catch 'done
      (dolist (sid surfaces)
        (unless (equal sid my-surface)
          (let* ((screen (cmux--read-screen exe sid cmux-read-screen-lines))
                 (name (and screen (cmux--match-cli screen))))
            (when name
              (setq cmux-surface sid
                    cmux-cli-name name
                    found (cons name sid))
              (throw 'done nil))))))
    (if found
        (cmux--message "Detected %s -> %s" (car found) (cdr found))
      (cmux--message "No AI CLI found; set manually with M-x cmux-set-surface"))
    found))

;;;; Public commands

;;;###autoload
(defun cmux-send-file ()
  "Send the current buffer's path as an @mention reference."
  (interactive)
  (cmux--ensure-local)
  (let* ((path (cmux--relative-path)))
    (cmux--send-ref path "")))

;;;###autoload
(defun cmux-send-pos ()
  "Send the current buffer's path with the cursor's line number."
  (interactive)
  (cmux--ensure-local)
  (let* ((path (cmux--relative-path))
         (line (line-number-at-pos (point))))
    (cmux--send-ref path (format ":L%d" line))))

;;;###autoload
(defun cmux-send-range (beg end)
  "Send the current buffer's path with the selected line range BEG..END.
Must be called with an active region."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Cmux.el: `cmux-send-range' requires an active region"))
  (cmux--ensure-local)
  (let* ((path (cmux--relative-path))
         (info (cmux--format-line-info beg end)))
    (cmux--send-ref path info)))

;;;###autoload
(defun cmux-detect ()
  "Scan cmux surfaces and auto-select the one running an AI CLI."
  (interactive)
  (let ((exe (cmux--ensure-executable)))
    (cmux--detect-internal exe)))

;;;###autoload
(defun cmux-set-surface (surface)
  "Manually set the target cmux SURFACE (e.g. \"surface:1\")."
  (interactive "sSurface (e.g. surface:1): ")
  (let ((coerced (cmux--coerce-surface surface)))
    (unless coerced
      (user-error "Cmux.el: Invalid surface reference: %S" surface))
    (setq cmux-surface coerced)
    (cmux--message "Surface set to %s" coerced)))

;;;###autoload
(defun cmux-dwim ()
  "Send region when active, otherwise send the current file path."
  (interactive)
  (if (use-region-p)
      (cmux-send-range (region-beginning) (region-end))
    (cmux-send-file)))

;;;; Minor mode

(defvar cmux-mode-map (make-sparse-keymap)
  "Keymap for `cmux-mode'.  Left intentionally empty; bind keys yourself.")

;;;###autoload
(define-minor-mode cmux-mode
  "Buffer-local minor mode that enables cmux.el key bindings.
The mode itself provides no bindings by default; customize
`cmux-mode-map' to add your own."
  :lighter " cmux"
  :keymap cmux-mode-map
  :group 'cmux)

(defun cmux--maybe-turn-on ()
  "Enable `cmux-mode' in buffers where it is applicable."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (not (file-remote-p default-directory))
             (not (minibufferp))
             (let ((name (buffer-name)))
               (not (and name
                         (> (length name) 0)
                         (eq (aref name 0) ?\s)))))
    (cmux-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-cmux-mode
  cmux-mode cmux--maybe-turn-on
  :group 'cmux)

(provide 'cmux)

;;; cmux.el ends here
