;;; cmux-tests.el --- ERT tests for cmux.el -*- lexical-binding: t -*-

;; Author: Kei Nakayama <kei.of.nakayama@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l ert -l cmux-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmux)

;;;; Helpers -----------------------------------------------------------

(defmacro cmux-tests--with-calls (recorder-sym &rest body)
  "Bind RECORDER-SYM to a list and stub cmux process calls to push into it.

Each stubbed cmux--call/cmux--call-checked/cmux--call-with-separate-stderr
invocation pushes (TYPE EXE . ARGS) onto RECORDER-SYM (newest first).
The stubs return success values by default; individual tests that need
a specific response can rebind the functions themselves."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,recorder-sym nil))
     (cl-letf* (((symbol-function 'executable-find)
                 (lambda (name)
                   (and (stringp name)
                        (> (length name) 0)
                        (concat "/usr/local/bin/" name))))
                ((symbol-function 'cmux--call)
                 (lambda (exe &rest args)
                   (push (cons 'call (cons exe args)) ,recorder-sym)
                   (cons 0 "")))
                ((symbol-function 'cmux--call-checked)
                 (lambda (exe &rest args)
                   (push (cons 'call-checked (cons exe args)) ,recorder-sym)
                   ""))
                ((symbol-function 'cmux--call-with-separate-stderr)
                 (lambda (exe &rest args)
                   (push (cons 'call-split (cons exe args)) ,recorder-sym)
                   (cons 0 (cons "" "")))))
       ,@body)))

(defun cmux-tests--send-args (calls)
  "Extract the positional argument of the `cmux send` invocation.
CALLS is the recorder list populated by `cmux-tests--with-calls'.
Return the last string argument of the first `send' subcommand."
  (let ((hit (cl-find-if
              (lambda (entry)
                ;; entry = (TYPE EXE SUBCOMMAND ...)
                (and (consp (cdr entry))
                     (stringp (nth 2 entry))
                     (equal (nth 2 entry) "send")))
              (reverse calls))))
    (and hit (car (last hit)))))

;;;; Surface validation: cmux--valid-surface-p / cmux--coerce-surface --

(ert-deftest cmux-test-V-01-valid-surface-digits ()
  (should (cmux--valid-surface-p "surface:0"))
  (should (cmux--valid-surface-p "surface:1"))
  (should (cmux--valid-surface-p "surface:12345"))
  (should (equal "surface:1" (cmux--coerce-surface "surface:1")))
  (should (equal "surface:12345" (cmux--coerce-surface "surface:12345"))))

(ert-deftest cmux-test-V-03-non-string-rejected ()
  (should-not (cmux--valid-surface-p nil))
  (should-not (cmux--valid-surface-p 1))
  (should-not (cmux--valid-surface-p 'surface))
  (should-not (cmux--coerce-surface nil))
  (should-not (cmux--coerce-surface 42))
  (should-not (cmux--coerce-surface 'surface:1)))

(ert-deftest cmux-test-V-04-empty-or-whitespace ()
  (should-not (cmux--valid-surface-p ""))
  (should-not (cmux--valid-surface-p "   "))
  (should-not (cmux--valid-surface-p "\t\t"))
  (should-not (cmux--coerce-surface ""))
  (should-not (cmux--coerce-surface "   ")))

(ert-deftest cmux-test-V-05-malformed-rejected ()
  (dolist (s '("surface:" "surface:-1" "surface:1a" "pane:1"
               "Surface:1" "surface:1 extra"))
    (should-not (cmux--valid-surface-p s))
    (should-not (cmux--coerce-surface s))))

(ert-deftest cmux-test-V-06-nul-rejected ()
  (should-not (cmux--valid-surface-p "surface:1\x00"))
  (should-not (cmux--valid-surface-p "\x00surface:1"))
  (should-not (cmux--coerce-surface "surface:1\x00"))
  (should-not (cmux--coerce-surface "\x00surface:1")))

(ert-deftest cmux-test-V-07-newline-rejected ()
  (should-not (cmux--valid-surface-p "surface:1\nsurface:2"))
  (should-not (cmux--valid-surface-p "surface:1\r"))
  (should-not (cmux--coerce-surface "surface:1\nsurface:2"))
  (should-not (cmux--coerce-surface "surface:1\r")))

(ert-deftest cmux-test-V-08-trim-retained-on-valid ()
  ;; Leading/trailing whitespace should be trimmed for otherwise-valid inputs.
  (should (cmux--valid-surface-p "  surface:3  "))
  (should (equal "surface:3" (cmux--coerce-surface "  surface:3  "))))

;;;; cmux-set-surface --------------------------------------------------

(ert-deftest cmux-test-SS-01-set-surface-happy ()
  (let ((cmux-surface nil))
    (cmux-set-surface "surface:1")
    (should (equal cmux-surface "surface:1"))))

(ert-deftest cmux-test-SS-02-set-surface-trims ()
  (let ((cmux-surface nil))
    (cmux-set-surface "  surface:7  ")
    (should (equal cmux-surface "surface:7"))))

(ert-deftest cmux-test-SS-03-set-surface-invalid-strings ()
  (let ((cmux-surface nil))
    (should-error (cmux-set-surface "") :type 'user-error)
    (should-error (cmux-set-surface "garbage") :type 'user-error)
    (should (null cmux-surface))))

(ert-deftest cmux-test-SS-04-set-surface-rejects-nul ()
  (let ((cmux-surface nil))
    (should-error (cmux-set-surface "surface:1\x00") :type 'user-error)
    (should (null cmux-surface))))

(ert-deftest cmux-test-SS-05-set-surface-rejects-newline ()
  (let ((cmux-surface nil))
    (should-error (cmux-set-surface "surface:1\nsurface:2") :type 'user-error)
    (should (null cmux-surface))))

;;;; cmux--sanitize-path -----------------------------------------------

(ert-deftest cmux-test-S-01-sanitize-accepts-plain-path ()
  (should (equal "/tmp/a.txt" (cmux--sanitize-path "/tmp/a.txt"))))

(ert-deftest cmux-test-S-02-sanitize-nil-errors ()
  (should-error (cmux--sanitize-path nil) :type 'user-error))

(ert-deftest cmux-test-S-03-sanitize-nul-errors ()
  (should-error (cmux--sanitize-path "/tmp/a\x00b") :type 'user-error))

(ert-deftest cmux-test-S-04-sanitize-lf-errors ()
  (should-error (cmux--sanitize-path "/tmp/a\nb") :type 'user-error))

(ert-deftest cmux-test-S-05-sanitize-esc-errors ()
  (should-error (cmux--sanitize-path "/tmp/a\x1b[31mred") :type 'user-error))

(ert-deftest cmux-test-S-06-sanitize-cr-errors ()
  (should-error (cmux--sanitize-path "/tmp/a\rb") :type 'user-error))

;;;; cmux--ensure-executable -------------------------------------------

(ert-deftest cmux-test-C-16-empty-or-nil-executable ()
  (let ((cmux-executable ""))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil)))
      (should-error (cmux--ensure-executable) :type 'user-error)))
  (let ((cmux-executable nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil)))
      (should-error (cmux--ensure-executable) :type 'user-error))))

(ert-deftest cmux-test-C-17-missing-executable ()
  (let ((cmux-executable "cmux-does-not-exist-xyz"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil)))
      (should-error (cmux--ensure-executable) :type 'user-error))))

(ert-deftest cmux-test-C-18-found-executable-returns-absolute ()
  (let ((cmux-executable "cmux"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (n) (concat "/opt/bin/" n))))
      (should (equal "/opt/bin/cmux" (cmux--ensure-executable))))))

;;;; cmux--ensure-local (TRAMP refusal) --------------------------------

(ert-deftest cmux-test-C-05-refuses-remote-buffer-file-name ()
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/tmp/a.txt")
    (setq default-directory "/tmp/")
    (should-error (cmux--ensure-local) :type 'user-error)))

(ert-deftest cmux-test-C-06-refuses-remote-default-directory ()
  (with-temp-buffer
    (setq buffer-file-name nil)
    (setq default-directory "/ssh:host:/tmp/")
    (should-error (cmux--ensure-local) :type 'user-error)))

(ert-deftest cmux-test-C-07-allows-local-buffer ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/a.txt")
    (setq default-directory "/tmp/")
    (should (null (cmux--ensure-local)))))

;;;; cmux--parse-tree --------------------------------------------------

(ert-deftest cmux-test-T-01-empty-input ()
  (should (null (cmux--parse-tree "")))
  (should (null (cmux--parse-tree nil))))

(ert-deftest cmux-test-T-02-single-pane-single-surface ()
  (let ((out "pane:0\n  surface:1\n"))
    (should (equal '(("pane:0" . "surface:1"))
                   (cmux--parse-tree out)))))

(ert-deftest cmux-test-T-03-multiple-panes-and-surfaces ()
  ;; Each pane attaches any following surface lines until the next pane.
  (let* ((out (concat "pane:0\n"
                      "  surface:1\n"
                      "  surface:2\n"
                      "pane:1\n"
                      "  surface:3\n"))
         (parsed (cmux--parse-tree out)))
    (should (member '("pane:0" . "surface:1") parsed))
    (should (member '("pane:0" . "surface:2") parsed))
    (should (member '("pane:1" . "surface:3") parsed))
    (should (= 3 (length parsed)))))

(ert-deftest cmux-test-T-04-surface-without-pane ()
  ;; Surfaces that precede any pane line must be dropped.
  (let ((out "surface:9\nsurface:10\n"))
    (should (null (cmux--parse-tree out)))))

;;;; cmux--path-from-style / git fallback ------------------------------

(ert-deftest cmux-test-P-03-git-root-relativizes ()
  (cl-letf (((symbol-function 'vc-git-root)
             (lambda (_f) "/home/u/repo/")))
    (should (equal "src/main.el"
                   (cmux--path-from-style
                    "/home/u/repo/src/main.el" 'git-root)))))

(ert-deftest cmux-test-P-04-no-git-root-absolute-with-warning ()
  (let ((cmux-path-style-fallback 'absolute-with-warning)
        (warned nil))
    (cl-letf (((symbol-function 'vc-git-root) (lambda (_f) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should (equal "/tmp/a.txt"
                     (cmux--path-from-style "/tmp/a.txt" 'git-root)))
      (should warned))))

(ert-deftest cmux-test-P-05-no-git-root-error ()
  (let ((cmux-path-style-fallback 'error))
    (cl-letf (((symbol-function 'vc-git-root) (lambda (_f) nil)))
      (should-error (cmux--path-from-style "/tmp/a.txt" 'git-root)
                    :type 'user-error))))

(ert-deftest cmux-test-P-06-no-git-root-relative-home ()
  (let ((cmux-path-style-fallback 'relative-home))
    (cl-letf (((symbol-function 'vc-git-root) (lambda (_f) nil))
              ((symbol-function 'abbreviate-file-name)
               (lambda (p) (replace-regexp-in-string "\\`/home/u" "~" p))))
      (should (equal "~/work/a.txt"
                     (cmux--path-from-style "/home/u/work/a.txt" 'git-root))))))

(ert-deftest cmux-test-P-07-path-outside-root-uses-fallback ()
  ;; git-root returns a path, but the file lives outside that root.
  (let ((cmux-path-style-fallback 'absolute-with-warning)
        (warned nil))
    (cl-letf (((symbol-function 'vc-git-root)
               (lambda (_f) "/home/u/repo/"))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should (equal "/etc/hosts"
                     (cmux--path-from-style "/etc/hosts" 'git-root)))
      (should warned))))

(ert-deftest cmux-test-P-08-absolute-style-passthrough ()
  (should (equal "/tmp/a.txt"
                 (cmux--path-from-style "/tmp/a.txt" 'absolute))))

(ert-deftest cmux-test-P-09-relative-home-style ()
  (cl-letf (((symbol-function 'abbreviate-file-name)
             (lambda (p) (replace-regexp-in-string "\\`/home/u" "~" p))))
    (should (equal "~/a.txt"
                   (cmux--path-from-style "/home/u/a.txt" 'relative-home)))))

;;;; CMUX_SURFACE_ID environment variable ------------------------------

(ert-deftest cmux-test-D-04a-env-nul-rejected ()
  (let ((process-environment
         (cons "CMUX_SURFACE_ID=surface:1\x00hack" process-environment)))
    (should-not (cmux--coerce-surface (getenv "CMUX_SURFACE_ID")))))

(ert-deftest cmux-test-D-04b-env-garbage-rejected ()
  (let ((process-environment
         (cons "CMUX_SURFACE_ID=garbage" process-environment)))
    (should-not (cmux--coerce-surface (getenv "CMUX_SURFACE_ID")))))

(ert-deftest cmux-test-D-04c-env-valid-accepted ()
  (let ((process-environment
         (cons "CMUX_SURFACE_ID=surface:2" process-environment)))
    (should (equal "surface:2"
                   (cmux--coerce-surface (getenv "CMUX_SURFACE_ID"))))))

;;;; cmux--format-line-info --------------------------------------------

(ert-deftest cmux-test-F-01-same-line ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1) ;; on line 2
    (let ((p (point)))
      (should (equal ":L2" (cmux--format-line-info p p))))))

(ert-deftest cmux-test-F-02-multi-line ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (let ((beg (progn (goto-char (point-min)) (point)))
          (end (progn (goto-char (point-min))
                      (forward-line 2) ;; start of line 3
                      (point))))
      (should (equal ":L1-3" (cmux--format-line-info beg end))))))

;;;; Public commands: cmux-send-file / cmux-send-pos / cmux-send-range -

(ert-deftest cmux-test-C-01-send-file-payload ()
  (cmux-tests--with-calls calls
    (let ((cmux-surface "surface:1")
          (cmux-auto-focus nil)
          (cmux-auto-enter nil)
          (cmux-path-style 'absolute))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.txt")
        (setq default-directory "/tmp/")
        (cmux-send-file))
      (should (equal "@/tmp/a.txt " (cmux-tests--send-args calls))))))

(ert-deftest cmux-test-C-02-send-pos-payload ()
  (cmux-tests--with-calls calls
    (let ((cmux-surface "surface:1")
          (cmux-auto-focus nil)
          (cmux-auto-enter nil)
          (cmux-path-style 'absolute))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.txt")
        (setq default-directory "/tmp/")
        (insert (make-string 50 ?\n))
        (goto-char (point-min))
        (forward-line 41) ;; now on line 42
        (cmux-send-pos))
      (should (equal "@/tmp/a.txt:L42 " (cmux-tests--send-args calls))))))

(ert-deftest cmux-test-C-03-send-range-payload ()
  (cmux-tests--with-calls calls
    (let ((cmux-surface "surface:1")
          (cmux-auto-focus nil)
          (cmux-auto-enter nil)
          (cmux-path-style 'absolute))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.txt")
        (setq default-directory "/tmp/")
        (insert "a\nb\nc\nd\n")
        (let ((beg (point-min))
              (end (progn (goto-char (point-min))
                          (forward-line 2)
                          (point))))
          ;; Simulate an active region by binding `use-region-p'.
          (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
            (cmux-send-range beg end))))
      (should (equal "@/tmp/a.txt:L1-3 " (cmux-tests--send-args calls))))))

(ert-deftest cmux-test-C-25-dwim-dispatches-range-or-file ()
  ;; region active -> cmux-send-range is called.
  (let (called-range called-file)
    (cl-letf (((symbol-function 'cmux-send-range)
               (lambda (&rest _) (setq called-range t)))
              ((symbol-function 'cmux-send-file)
               (lambda (&rest _) (setq called-file t)))
              ((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'region-beginning) (lambda () 1))
              ((symbol-function 'region-end) (lambda () 2)))
      (cmux-dwim)
      (should called-range)
      (should-not called-file)))
  ;; no region -> cmux-send-file is called.
  (let (called-range called-file)
    (cl-letf (((symbol-function 'cmux-send-range)
               (lambda (&rest _) (setq called-range t)))
              ((symbol-function 'cmux-send-file)
               (lambda (&rest _) (setq called-file t)))
              ((symbol-function 'use-region-p) (lambda () nil)))
      (cmux-dwim)
      (should called-file)
      (should-not called-range))))

(ert-deftest cmux-test-C-26-send-range-requires-region ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/a.txt")
    (setq default-directory "/tmp/")
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
      (should-error (cmux-send-range (point-min) (point-max))
                    :type 'user-error))))

(ert-deftest cmux-test-C-27-send-file-rejects-buffer-without-file ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (n) (concat "/usr/local/bin/" n)))
            ((symbol-function 'cmux--call)
             (lambda (&rest _) (cons 0 "")))
            ((symbol-function 'cmux--call-checked)
             (lambda (&rest _) "")))
    (let ((cmux-surface "surface:1"))
      (with-temp-buffer
        (setq buffer-file-name nil)
        (setq default-directory "/tmp/")
        (should-error (cmux-send-file) :type 'user-error)))))

;;;; cmux--ensure-executable: control-character rejection ---------------

(ert-deftest cmux-test-C-16b-executable-with-nul-rejected ()
  (let ((cmux-executable "bad\x00exe"))
    (should-error (cmux--ensure-executable) :type 'user-error)))

(ert-deftest cmux-test-C-16c-executable-with-escape-rejected ()
  (let ((cmux-executable "bad\x1bexe"))
    (should-error (cmux--ensure-executable) :type 'user-error)))

;;;; cmux-surface-env-trust --------------------------------------------

(ert-deftest cmux-test-D-04d-env-trust-nil-ignores-env ()
  ;; When env trust is disabled, a valid CMUX_SURFACE_ID must not be
  ;; promoted into `cmux-surface' and `cmux--detect-internal' is
  ;; triggered instead.
  (let* ((detect-called nil)
         (cmux-surface nil)
         (cmux-surface-env-trust nil)
         (process-environment
          (cons "CMUX_SURFACE_ID=surface:9" process-environment)))
    (cl-letf (((symbol-function 'cmux--detect-internal)
               (lambda (_exe) (setq detect-called t) nil)))
      (should-error (cmux--ensure-surface "/opt/bin/cmux") :type 'user-error)
      (should detect-called))))

(ert-deftest cmux-test-D-04e-env-trust-t-uses-env ()
  (let ((cmux-surface nil)
        (cmux-surface-env-trust t)
        (process-environment
         (cons "CMUX_SURFACE_ID=surface:7" process-environment)))
    (should (equal "surface:7"
                   (cmux--ensure-surface "/opt/bin/cmux")))))

;;;; cmux--truncate-for-error ------------------------------------------

(ert-deftest cmux-test-Tr-01-strips-ansi-and-controls ()
  (let ((result
         (cmux--truncate-for-error
          "\x1b[31mred\x1b[0m message with \x00 NUL")))
    (should-not (string-match-p "\x1b" result))
    (should-not (string-match-p "\x00" result))
    (should (string-match-p "red" result))
    (should (string-match-p "message" result))))

(ert-deftest cmux-test-Tr-02-caps-at-500-chars ()
  (let* ((long (make-string 1000 ?a))
         (result (cmux--truncate-for-error long)))
    (should (<= (length result) (+ 500 3)))
    (should (string-suffix-p "..." result))))

;;;; cmux--format-line-info: reversed order normalization --------------

(ert-deftest cmux-test-F-06-reversed-region-is-normalized ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (let ((line1 (point-min))
          (line3 (progn (goto-char (point-min))
                        (forward-line 2)
                        (point))))
      (should (equal ":L1-3" (cmux--format-line-info line3 line1)))
      (should (equal ":L1-3" (cmux--format-line-info line1 line3))))))

;;;; cmux--parse-tree: case-fold independence --------------------------

(ert-deftest cmux-test-T-05-case-fold-does-not-affect-parsing ()
  ;; Even with `case-fold-search' globally set to t (a common user
  ;; preference), `cmux--parse-tree' must only accept lower-case
  ;; "pane:" / "surface:" tokens produced by the cmux CLI.
  (let ((case-fold-search t))
    (should (null (cmux--parse-tree "PANE:0\n  SURFACE:1\n")))
    (should (equal '(("pane:0" . "surface:1"))
                   (cmux--parse-tree "pane:0\n  surface:1\n")))))

;;;; cmux--read-screen: ANSI stripping ---------------------------------

(ert-deftest cmux-test-RS-01-read-screen-strips-ansi ()
  (cl-letf (((symbol-function 'cmux--call-with-separate-stderr)
             (lambda (&rest _)
               (cons 0 (cons "\x1b[31mclaude\x1b[0m banner" "")))))
    (let ((out (cmux--read-screen "/opt/bin/cmux" "surface:1" 30)))
      (should (stringp out))
      (should-not (string-match-p "\x1b" out))
      (should (string-match-p "claude" out)))))

(ert-deftest cmux-test-RS-02-read-screen-returns-nil-on-failure ()
  (cl-letf (((symbol-function 'cmux--call-with-separate-stderr)
             (lambda (&rest _) (cons 1 (cons "" "boom")))))
    (should (null (cmux--read-screen "/opt/bin/cmux" "surface:1" 30)))))

;;;; cmux--send-ref: success message must not expose absolute path -----

(ert-deftest cmux-test-MSG-01-success-message-uses-abbreviated-path ()
  (let ((cmux-surface "surface:1")
        (cmux-auto-focus nil)
        (cmux-auto-enter nil)
        (cmux-path-style 'absolute)
        (captured nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (n) (concat "/usr/local/bin/" n)))
              ((symbol-function 'cmux--call)
               (lambda (&rest _) (cons 0 "")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args))))
              ((symbol-function 'abbreviate-file-name)
               (lambda (p)
                 (replace-regexp-in-string "\\`/home/kei" "~" p))))
      (with-temp-buffer
        (setq buffer-file-name "/home/kei/project/a.txt")
        (setq default-directory "/home/kei/project/")
        (cmux-send-file))
      (should (string-match-p "~/project/a.txt" (or captured "")))
      (should-not (string-match-p "/home/kei/" (or captured ""))))))

(provide 'cmux-tests)

;;; cmux-tests.el ends here
