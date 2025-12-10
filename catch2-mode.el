;;; catch2-mode.el --- Display Catch2 tests in a tabulated list -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: c, tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for displaying Catch2 test results in a tabulated list.

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'subr-x)
(require 'project)
(require 'transient)

(defgroup catch2-mode nil
  "Display Catch2 tests in a tabulated list."
  :group 'tools
  :prefix "catch2-")

;;
;; Variables
;;
(defcustom catch2-xml-file-pattern "test-results-*.tests.xml"
  "Pattern to match Catch2 XML test result files."
  :type 'string
  :group 'catch2-mode)

(defcustom catch2-project-name "Catch2 Test Suites"
  "Default project name to show in the buffer."
  :type 'string
  :group 'catch2-mode)

(defcustom catch2-test-arguments '("--reporter" "xml" "--durations" "yes" "--filenames-as-tags")
  "Arguments to pass to Catch2 test executable.
The output file argument (-o) is added automatically."
  :type '(repeat string)
  :group 'catch2-mode)

(defvar-local catch2--search-directory nil
  "The directory used to search for Catch2 XML files in this buffer.")

(defvar-local catch2--suite-files nil
  "Hash table mapping suite names to their XML file paths.")

;;
;; Debugging
;;
(defvar catch2-debug-enabled t
  "When non-nil, enable debug logging for Catch2 functions.")

(defvar catch2-debug-buffer "*catch2-debug*"
  "When non-nil, enable debug logging for Catch2 functions.")

(defun catch2--debug (format-string &rest args)
  "Log debug message to *cmake-debug* buffer if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when catch2-debug-enabled
    (with-current-buffer (get-buffer-create catch2-debug-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%Y-%m-%dT%H:%M:%S.%3N")
                      (apply #'format format-string args))))))

;;
;; Files
;;
(defun catch2-locate-xml-files (&optional directory)
  "Locate all Catch2 XML test files in DIRECTORY recursively.
Returns a list of absolute file paths."
  (let ((search-dir (or directory
                        (when-let ((proj (project-current)))
                          (project-root proj))
                        default-directory)))
    (unless search-dir
      (error "Not in a project and no directory specified"))

    (catch2--debug "Searching for Catch2 XML files in: %s" search-dir)

    (let ((files (directory-files-recursively
                  search-dir
                  (replace-regexp-in-string
                   "\\*" ".*"
                   (file-name-nondirectory catch2-xml-file-pattern)))))
      (catch2--debug "Found %d Catch2 XML files" (length files))

      (if files
          files
        (error "No Catch2 XML files found matching pattern: %s in %s"
               catch2-xml-file-pattern search-dir)))))

;;
;; Parsing
;;
(defun catch2-parse-tags (tags-string)
  "Parse TAGS-STRING from Catch2 XML into a list of tag strings.
Input: \"[handler][messaging][#messaging_risk_message_handler_tests]\"
Output: (\"handler\" \"messaging\" \"messaging_risk_message_handler_tests\")"
  (when tags-string
    (let ((parsed-tags
           (mapcar (lambda (tag)
                     (replace-regexp-in-string "^#+" "" tag))
                   (split-string
                    (replace-regexp-in-string "^\\[\\|\\]$" "" tags-string)
                    "\\]\\[" t))))
      (catch2--debug "Parsed tags: '%s' -> %S" tags-string parsed-tags)
      parsed-tags)))

(defun catch2-parse-testcase (case-node xml-file)
  "Parse a <TestCase> element CASE-NODE from XML-FILE.
Return plist with all test case attributes and overall result data."
  (let* ((attrs (xml-node-attributes case-node))
         (name (cdr (assq 'name attrs)))
         (tags (catch2-parse-tags (cdr (assq 'tags attrs))))
         (filename (cdr (assq 'filename attrs)))
         (line (string-to-number (or (cdr (assq 'line attrs)) "0")))
         (children (xml-node-children case-node))
         (success nil)  ; Default to nil (fail) until we find OverallResult
         (durationInSeconds 0.0)
         (skips 0)
         (found-overall-result nil))

    ;; Find <OverallResult> element
    (dolist (child children)
      (when (and (consp child)
                 (eq (car child) 'OverallResult))
        (setq found-overall-result t)
        (let* ((result-attrs (xml-node-attributes child))
               (success-attr (cdr (assq 'success result-attrs))))
          (catch2--debug "OverallResult for '%s': success-attr='%s'" name success-attr)
          (setq success (string= success-attr "true"))
          (setq durationInSeconds (string-to-number
                                   (or (cdr (assq 'durationInSeconds result-attrs)) "0.0")))
          (setq skips (string-to-number
                       (or (cdr (assq 'skips result-attrs)) "0"))))))

    (unless found-overall-result
      (catch2--debug "WARNING: No OverallResult found for test case '%s'" name))

    (catch2--debug "Parsed test case: %s (success: %s, durationInSeconds: %.3fs, skips: %d, tags: %S)"
                   name success durationInSeconds skips tags)

    `(:name ,name
      :success ,success
      :file ,xml-file
      :tags ,tags
      :filename ,filename
      :line ,line
      :durationInSeconds ,durationInSeconds
      :skips ,skips
      :raw-attrs ,attrs)))

(defun catch2--sanitize-xml-buffer ()
  "Remove invalid XML characters from current buffer.
XML 1.0 only allows: #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]"
  (goto-char (point-min))
  ;; Remove control characters except tab (9), newline (10), carriage return (13)
  ;; This regex matches control chars 0-8, 11-12, 14-31
  (let ((count 0))
    (while (re-search-forward "[\x00-\x08\x0B\x0C\x0E-\x1F]" nil t)
      (replace-match "" t t)
      (cl-incf count))
    (when (> count 0)
      (catch2--debug "Sanitized %d invalid XML characters" count))))

(defun catch2--validate-xml-file (xml-file)
  "Check if XML-FILE appears to be a complete Catch2 XML file.
Returns nil if file is truncated or incomplete, t otherwise."
  (let ((size (file-attribute-size (file-attributes xml-file))))
    (when (< size 100)
      (catch2--debug "File %s appears truncated (only %d bytes)" xml-file size)
      nil)))

(defun catch2--extract-build-config (xml-file)
  "Extract build configuration from XML-FILE path.
Looks for patterns like 'linux-clang-debug' or 'linux-gcc-release' in the path."
  (let ((path (file-name-directory xml-file)))
    (when (string-match "/\\([^/]*-\\(?:debug\\|release\\)\\)/" path)
      (match-string 1 path))))

(defun catch2-parse-suite (xml-file)
  "Parse a Catch2 v3 XML suite from XML-FILE.
Return plist with all suite attributes and test cases."
  (catch2--debug "Parsing suite file: %s" xml-file)
  ;; Check for truncated files first
  (let ((size (file-attribute-size (file-attributes xml-file))))
    (when (< size 100)
      (catch2--debug "Skipping truncated file %s (only %d bytes)" xml-file size)
      (error "Truncated XML file (only %d bytes): %s" size xml-file)))
  (condition-case err
      (let* ((file-attributes (file-attributes xml-file))
             (modification-time (file-attribute-modification-time file-attributes))
             (xml (with-temp-buffer
                    (insert-file-contents xml-file)
                    (catch2--sanitize-xml-buffer)
                    (xml-parse-region (point-min) (point-max))))
             (root (car xml)) ;; <Catch2TestRun ...>
             (attrs (xml-node-attributes root))
             (name (cdr (assq 'name attrs)))
             (preset (catch2--extract-build-config xml-file))
             (rng-seed (cdr (assq 'rng-seed attrs)))
             (xml-format-version (cdr (assq 'xml-format-version attrs)))
             (catch2-version (cdr (assq 'catch2-version attrs)))
             (children (xml-node-children root))
             cases
             overall-results
             overall-results-cases)

        ;; Validate we have a proper XML structure
        (unless (and (consp root) (eq (car root) 'Catch2TestRun))
          (error "Not a valid Catch2 XML file: %s" xml-file))

        ;; Collect <TestCase> nodes
        (dolist (node children)
          (cond
           ((and (consp node) (eq (car node) 'TestCase))
            (push (catch2-parse-testcase node xml-file) cases))
           ((and (consp node) (eq (car node) 'OverallResults))
            (setq overall-results (xml-node-attributes node)))
           ((and (consp node) (eq (car node) 'OverallResultsCases))
            (setq overall-results-cases (xml-node-attributes node)))))

        (let ((suite-plist `(:name ,name
                            :preset ,preset
                            :cases ,(nreverse cases)
                            :file ,xml-file
                            :modification-time ,modification-time
                            :rng-seed ,rng-seed
                            :xml-format-version ,xml-format-version
                            :catch2-version ,catch2-version
                            :overall-results ,overall-results
                            :overall-results-cases ,overall-results-cases
                            :raw-attrs ,attrs)))
          (catch2--debug "Parsed suite: %s with %d test cases" name (length cases))
          suite-plist))
    (error
     (catch2--debug "Error parsing XML file %s: %s" xml-file (error-message-string err))
     (error "Failed to parse Catch2 XML file %s: %s" xml-file (error-message-string err)))))

;;
;; Post-processing
;;
(defun catch2-generate-suite-summaries (suites)
  "Generate summaries for each test suite in SUITES.
Return list of plists with suite summary information."
  (catch2--debug "Starting generate-suite-summaries with %d suites" (length suites))

  (mapcar (lambda (suite)
            (catch2--debug "Processing suite: %S" suite)
            (let* ((suite-name (plist-get suite :name))
                   (preset (plist-get suite :preset))
                   (cases (plist-get suite :cases))
                   (xml-file (plist-get suite :file))
                   (modification-time (plist-get suite :modification-time))
                   (rng-seed (plist-get suite :rng-seed))
                   (catch2-version (plist-get suite :catch2-version))
                   (xml-format-version (plist-get suite :xml-format-version))
                   (overall-results (plist-get suite :overall-results))
                   (overall-results-cases (plist-get suite :overall-results-cases)))

              (catch2--debug "Suite name: %s, cases count: %d, file: %s, modification-time: %S"
                             suite-name (length cases) xml-file modification-time)
              (catch2--debug "RNG seed: %s, Catch2 version: %s, XML format: %s"
                             rng-seed catch2-version xml-format-version)

              (let ((total-durationInSeconds 0.0)
                    (all-tags (make-hash-table :test 'equal))
                    (has-failures nil)
                    (fail-count 0))

                ;; Process each test case using the parsed data
                (dolist (case cases)
                  (catch2--debug "Processing case: %S" case)
                  (let ((success (plist-get case :success))
                        (tags (plist-get case :tags))
                        (durationInSeconds (plist-get case :durationInSeconds)))
                    (catch2--debug "Case success: %s, durationInSeconds: %s, tags: %S"
                                   success durationInSeconds tags)
                    (setq total-durationInSeconds (+ total-durationInSeconds (or durationInSeconds 0.0)))
                    (unless success
                      (setq has-failures t)
                      (cl-incf fail-count))
                    ;; Collect unique tags
                    (dolist (tag tags)
                      (puthash tag t all-tags))))

                (let* ((unique-tags (hash-table-keys all-tags))
                      (summary `(:suite-name ,suite-name
                                 :preset ,preset
                                 :status ,(if has-failures 'fail 'pass)
                                 :durationInSeconds ,total-durationInSeconds
                                 :tags ,(mapconcat 'identity unique-tags ", ")
                                 :modification-time ,modification-time
                                 :xml-file ,xml-file
                                 :rng-seed ,rng-seed
                                 :catch2-version ,catch2-version
                                 :xml-format-version ,xml-format-version
                                 :test-count ,(length cases)
                                 :fail-count ,fail-count
                                 :overall-results ,overall-results
                                 :overall-results-cases ,overall-results-cases)))

                  (catch2--debug "Suite summary: %s - %s (%d tests, %d failed, %.3fs, tags: %s, mod-time: %S)"
                                 suite-name
                                 (if has-failures "FAIL" "PASS")
                                 (length cases)
                                 fail-count
                                 total-durationInSeconds
                                 (mapconcat 'identity unique-tags ", ")
                                 modification-time)
                  summary))))
          suites))

(defun catch2-generate-totals-summary (summaries)
  "Generate an overall totals summary from SUMMARIES.
Return a plist with combined totals across all test suites."
  (catch2--debug "Generating totals summary from %d suite summaries" (length summaries))

  (let ((total-durationInSeconds 0.0)
        (total-test-count 0)
        (total-fail-count 0)
        (has-any-failures nil)
        (newest-mod-time nil))

    (dolist (summary summaries)
      (let ((duration (plist-get summary :durationInSeconds))
            (test-count (plist-get summary :test-count))
            (fail-count (plist-get summary :fail-count))
            (status (plist-get summary :status))
            (mod-time (plist-get summary :modification-time)))

        (setq total-durationInSeconds (+ total-durationInSeconds (or duration 0.0)))
        (setq total-test-count (+ total-test-count (or test-count 0)))
        (setq total-fail-count (+ total-fail-count (or fail-count 0)))
        (when (eq status 'fail)
          (setq has-any-failures t))

        ;; Find the newest modification time
        (when mod-time
          (if (or (null newest-mod-time)
                  (time-less-p newest-mod-time mod-time))
              (setq newest-mod-time mod-time)))))

    (let ((totals-summary `(:suite-name "TOTALS"
                            :status ,(if has-any-failures 'fail 'pass)
                            :durationInSeconds ,total-durationInSeconds
                            :test-count ,total-test-count
                            :fail-count ,total-fail-count
                            :modification-time ,newest-mod-time
                            :is-totals t)))

      (catch2--debug "Generated totals: %d tests, %d failed, %.3fs, status: %s, newest mod-time: %S"
                     total-test-count total-fail-count total-durationInSeconds
                     (if has-any-failures "FAIL" "PASS") newest-mod-time)
      totals-summary)))

(defvar catch2-tabulated-mode-hook nil
  "Hook run when entering `catch2-tabulated-mode'.")

(defun catch2-tabulated-reload ()
  "Reload the Catch2 suites table."
  (interactive)
  (catch2-tabulated-display))

(defun catch2-tabulated-open-xml ()
  "Open the XML file for the test suite at point."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (xml-file (when key (gethash key catch2--suite-files))))
    (if (and xml-file (not (string= key "TOTALS")))
        (find-file xml-file)
      (message "No XML file for this entry"))))

(defun catch2-tabulated-delete-xml ()
  "Delete the XML file for the test suite at point and reload."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (xml-file (when key (gethash key catch2--suite-files))))
    (if (and xml-file (not (string= key "TOTALS")))
        (when (yes-or-no-p (format "Delete %s? " xml-file))
          (delete-file xml-file)
          (message "Deleted %s" xml-file)
          (catch2-tabulated-reload))
      (message "No XML file for this entry"))))

(defun catch2--xml-to-executable (xml-file)
  "Convert XML-FILE path to the test executable path.
Removes 'test-results-' prefix and '.xml' extension."
  (let* ((dir (file-name-directory xml-file))
         (name (file-name-nondirectory xml-file))
         (exe-name (string-remove-suffix ".xml"
                     (string-remove-prefix "test-results-" name))))
    (expand-file-name exe-name dir)))

(defun catch2-tabulated-run-tests ()
  "Run the tests for the test suite at point."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (xml-file (when key (expand-file-name (gethash key catch2--suite-files)))))
    (if (and xml-file (not (string= key "TOTALS")))
        (let* ((executable (catch2--xml-to-executable xml-file))
               (args (append catch2-test-arguments (list "-o" xml-file)))
               (default-directory (file-name-directory executable))
               (buffer-name (format "*catch2-run: %s*" (file-name-nondirectory executable)))
               (cmd-line (format "%s %s" executable (string-join args " "))))
          (catch2--debug "Run tests: executable=%s" executable)
          (catch2--debug "Run tests: args=%S" args)
          (catch2--debug "Run tests: default-directory=%s" default-directory)
          (catch2--debug "Run tests: full command: %s" cmd-line)
          (if (file-executable-p executable)
              (progn
                (message "Running %s..." (file-name-nondirectory executable))
                (with-current-buffer (get-buffer-create buffer-name)
                  (erase-buffer)
                  (insert (format "Command: %s\n" cmd-line))
                  (insert (format "Directory: %s\n" default-directory))
                  (insert (format "Started: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
                  (insert "--- Output ---\n\n"))
                (let ((proc (apply #'start-process
                                   (file-name-nondirectory executable)
                                   buffer-name
                                   executable
                                   args)))
                  (set-process-sentinel
                   proc
                   (lambda (proc event)
                     (catch2--debug "Run tests: process event: %s" (string-trim event))
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-max))
                       (insert (format "\n--- Process %s ---\n" (string-trim event)))
                       (insert (format "Exit code: %s\n" (process-exit-status proc))))
                     (catch2--debug "Run tests: exit code=%s" (process-exit-status proc))
                     (when (string-match-p "finished\\|exited" event)
                       (message "Test run completed: %s (exit code %d)"
                                (string-trim event)
                                (process-exit-status proc))
                       (catch2-tabulated-reload))))
                  (set-process-filter
                   proc
                   (lambda (proc output)
                     (catch2--debug "Run tests: output chunk: %s" (substring output 0 (min 200 (length output))))
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-max))
                         (insert output)))))))
            (progn
              (catch2--debug "Run tests: executable not found: %s" executable)
              (message "Executable not found: %s" executable))))
      (message "No test suite at point"))))

(defun catch2-testcases-reload ()
  "Reload the current test cases view."
  (interactive)
  (catch2-tabulated-view-suite))

(defun catch2-testcases-open-log ()
  "Open the log file for the test case at point."
  (interactive)
  (let* ((test-name (tabulated-list-get-id))
         (suites (catch2-parse-all-suites))
         (found-case nil)
         (found-suite nil))

    ;; Find the test case and its suite
    (cl-loop for suite in suites
             for cases = (plist-get suite :cases)
             do (cl-loop for case in cases
                         when (string= (plist-get case :name) test-name)
                         do (setq found-case case)
                         and do (setq found-suite suite)
                         and return t)
             when found-case
             return t)

    (if (and found-case found-suite)
        (let* ((xml-file (plist-get found-suite :file))
               (suite-name (plist-get found-suite :name))
               (log-file (catch2-find-log-file xml-file suite-name test-name)))
          (if log-file
              (progn
                (find-file log-file)
                (goto-char (point-max))
                (message "Opened log file: %s" log-file))
            (message "No log file found for test case: %s" test-name)))
      (message "Test case not found: %s" test-name))))

(defun catch2--show-current-file ()
  "Show the XML file path for the suite at point in the message area."
  (when (and (eq major-mode 'catch2-tabulated-mode)
             catch2--suite-files)
    (let* ((suite-name (tabulated-list-get-id))
           (xml-file (when suite-name
                       (gethash suite-name catch2--suite-files))))
      (when xml-file
        (message "%s" xml-file)))))

;;
;; Transient menus
;;
(transient-define-prefix catch2-suites-menu ()
  "Menu for Catch2 test suites."
  ["Actions"
   ("RET" "View suite tests" catch2-tabulated-view-suite)
   ("r" "Run tests" catch2-tabulated-run-tests)
   ("x" "Open XML file" catch2-tabulated-open-xml)
   ("k" "Delete XML file" catch2-tabulated-delete-xml)
   ("g" "Reload" catch2-tabulated-reload)
   ("q" "Quit" quit-window)])

(transient-define-prefix catch2-testcases-menu ()
  "Menu for Catch2 test cases."
  ["Actions"
   ("RET" "Open test file" catch2-testcases-open-file)
   ("t" "Open test file" catch2-testcases-open-file)
   ("l" "Open log file" catch2-testcases-open-log)
   ("g" "Reload" catch2-testcases-reload)
   ("q" "Quit" quit-window)])

;;
;; Keymaps
;;
(defvar catch2-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'catch2-tabulated-view-suite)
    (define-key map (kbd "r") #'catch2-tabulated-run-tests)
    (define-key map (kbd "x") #'catch2-tabulated-open-xml)
    (define-key map (kbd "k") #'catch2-tabulated-delete-xml)
    (define-key map (kbd "g") #'catch2-tabulated-reload)
    (define-key map (kbd "m") #'catch2-suites-menu)
    (define-key map (kbd "?") #'catch2-suites-menu)
    map)
  "Keymap for `catch2-tabulated-mode'.")

(defvar catch2-testcases-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'catch2-testcases-reload)
    (define-key map (kbd "t") #'catch2-testcases-open-file)
    (define-key map (kbd "l") #'catch2-testcases-open-log)
    (define-key map (kbd "RET") #'catch2-testcases-open-file)
    (define-key map (kbd "m") #'catch2-testcases-menu)
    (define-key map (kbd "?") #'catch2-testcases-menu)
    map)
  "Keymap for `catch2-testcases-mode'.")

;;
;; Modes
;;
(define-derived-mode catch2-tabulated-mode tabulated-list-mode "Catch2 Suites"
  "Major mode for viewing Catch2 test suites in a tabulated list."
  (setq tabulated-list-format
        [("Status" 8 t)
         ("Preset" 20 t)
         ("Suite Name" 30 t)
         ("Tests" 6 t :right-align t)
         ("Duration" 10 t :right-align t)
         ("Fails" 6 t :right-align t)
         ("Modified" 16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Preset" . nil))
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'catch2--show-current-file nil t)
  (run-hooks 'catch2-tabulated-mode-hook))

;; Add hl-line-mode to the hook
(add-hook 'catch2-tabulated-mode-hook #'hl-line-mode)

(defun catch2-tabulated-display (&optional directory)
  "Display Catch2 test suites in a tabulated list buffer.
DIRECTORY is the directory to search for XML files."
  (interactive)
  (let* ((search-dir (or directory
                         catch2--search-directory
                         (when-let ((proj (project-current)))
                           (project-root proj))
                         default-directory))
         (suites (catch2-parse-all-suites search-dir))
         (summaries (catch2-generate-suite-summaries suites))
         (totals-summary (catch2-generate-totals-summary summaries))
         (short-dir (abbreviate-file-name search-dir))
         (buffer-name (format "*%s*" catch2-project-name)))

    (catch2--debug "Displaying %d suites + totals in buffer: %s (dir: %s)"
                   (length summaries) buffer-name short-dir)

    (if (null summaries)
        (message "No Catch2 test suites found in %s" short-dir)
      (with-current-buffer (get-buffer-create buffer-name)
        (catch2-tabulated-mode)
        ;; Store the search directory for reload
        (setq catch2--search-directory search-dir)

        ;; Build unique key to XML file mapping (suite-name + preset)
        (setq catch2--suite-files (make-hash-table :test 'equal))
        (dolist (summary summaries)
          (let* ((suite-name (plist-get summary :suite-name))
                 (preset (plist-get summary :preset))
                 (key (if preset (format "%s|%s" suite-name preset) suite-name)))
            (puthash key (plist-get summary :xml-file) catch2--suite-files)))
        ;; Add entry for TOTALS showing the search directory
        (puthash "TOTALS" short-dir catch2--suite-files)

        ;; Find the most recent modification time
        (let ((newest-mod-time nil))
          (dolist (summary summaries)
            (let ((mod-time (plist-get summary :modification-time)))
              (when (and mod-time
                         (or (null newest-mod-time)
                             (time-less-p newest-mod-time mod-time)))
                (setq newest-mod-time mod-time))))

          ;; Combine regular summaries with totals
          (setq tabulated-list-entries
                (append
                 (mapcar (lambda (summary)
                           (let* ((fail-count (plist-get summary :fail-count))
                                  (mod-time (plist-get summary :modification-time))
                                  (status (plist-get summary :status))
                                  (suite-name (plist-get summary :suite-name))
                                  (preset (or (plist-get summary :preset) ""))
                                  (key (if (string-empty-p preset)
                                           suite-name
                                         (format "%s|%s" suite-name preset)))
                                  ;; Check if stale (> 5 mins older than newest)
                                  (stale (and mod-time newest-mod-time
                                              (> (float-time (time-subtract newest-mod-time mod-time))
                                                 300)))) ; 300 seconds = 5 minutes
                             (list key ; unique key
                                   (vector
                                    (propertize
                                     (if (eq status 'pass) "✓ PASS" "✗ FAIL")
                                     'face (cond
                                            (stale '(:foreground "yellow" :weight bold))
                                            ((eq status 'pass) '(:foreground "green" :weight bold))
                                            (t '(:foreground "red" :weight bold))))
                                    preset
                                    (if stale
                                        (propertize suite-name 'face '(:foreground "yellow"))
                                      suite-name)
                                    (number-to-string (plist-get summary :test-count))
                                    (format "%.3fs" (plist-get summary :durationInSeconds))
                                    (propertize (number-to-string fail-count)
                                                'face (if (> fail-count 0)
                                                        '(:foreground "red" :weight bold)
                                                      'default))
                                    (if mod-time
                                        (propertize (format-time-string "%Y-%m-%d %H:%M" mod-time)
                                                    'face (if stale '(:foreground "yellow") 'default))
                                      "N/A")))))
                          summaries)
                 (list (list "TOTALS" ; key for totals row
                             (let ((total-fail-count (plist-get totals-summary :fail-count))
                                   (total-status (plist-get totals-summary :status))
                                   (oldest-mod-time (plist-get totals-summary :modification-time)))
                               (vector
                                (propertize
                                 (if (eq total-status 'pass) "✓ PASS" "✗ FAIL")
                                 'face (if (eq total-status 'pass)
                                         '(:foreground "green" :weight bold :height 1.1)
                                       '(:foreground "red" :weight bold :height 1.1)))
                                ""  ; empty preset for totals
                                (propertize "TOTALS"
                                            'face '(:weight bold :height 1.1))
                                (propertize (number-to-string (plist-get totals-summary :test-count))
                                           'face '(:weight bold :height 1.1))
                                (propertize (format "%.3fs" (plist-get totals-summary :durationInSeconds))
                                           'face '(:weight bold :height 1.1))
                                (propertize (number-to-string total-fail-count)
                                           'face (if (> total-fail-count 0)
                                                   '(:foreground "red" :weight bold :height 1.1)
                                                 '(:weight bold :height 1.1)))
                                (if oldest-mod-time
                                    (format-time-string "%Y-%m-%d %H:%M" oldest-mod-time)
                                  "N/A"))))))))

        (tabulated-list-print)
        (switch-to-buffer (current-buffer))))))

(defun catch2--status-lessp (entry1 entry2)
  "Compare ENTRY1 and ENTRY2 by status, with failures first."
  (let ((status1 (aref (cadr entry1) 0))
        (status2 (aref (cadr entry2) 0)))
    (let ((fail1 (string-match-p "FAIL" status1))
          (fail2 (string-match-p "FAIL" status2)))
      (cond
       ((and fail1 (not fail2)) t)   ; entry1 is fail, entry2 is pass -> entry1 first
       ((and fail2 (not fail1)) nil) ; entry1 is pass, entry2 is fail -> entry2 first
       (t nil)))))                    ; same status, keep order

(define-derived-mode catch2-testcases-mode tabulated-list-mode "Catch2 Test Cases"
  "Major mode for viewing Catch2 test cases in a tabulated list."
  (setq tabulated-list-format
        [("Status" 8 catch2--status-lessp)
         ("Test Name" 50 t)
         ("Duration" 12 t :right-align t)
         ("File" 50 t)
         ("Line" 6 t :right-align t)
         ("Tags" 60 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Status" . nil))
  (tabulated-list-init-header)
  (run-hooks 'catch2-tabulated-mode-hook))

(defun catch2-tabulated-view-suite ()
  "View details of the test suite at point."
  (interactive)
  (let ((key (tabulated-list-get-id)))
    (if key
        (let ((suites (catch2-parse-all-suites catch2--search-directory))
              (found nil))
          (if (string= key "TOTALS")
              ;; Show all test cases from all suites
              (let ((all-cases (cl-loop for suite in suites
                                        append (plist-get suite :cases))))
                (catch2-display-testcases all-cases "ALL SUITES"))
            ;; Parse the key to get suite-name and preset
            (let* ((parts (split-string key "|"))
                   (suite-name (car parts))
                   (preset (cadr parts)))
              ;; Show test cases for specific suite matching both name and preset
              (dolist (suite suites)
                (when (and (string= (plist-get suite :name) suite-name)
                           (equal (plist-get suite :preset) preset))
                  (setq found suite)
                  (catch2-display-testcases (plist-get suite :cases)
                                            (if preset
                                                (format "%s [%s]" suite-name preset)
                                              suite-name))))
              (unless found
                (message "Suite '%s' not found" key)))))
      (message "No suite at point"))))

(defun catch2-display-testcases (cases suite-name)
  "Display test cases in a tabulated list buffer.
CASES is a list of test case plists, SUITE-NAME is the name of the suite."
  (let ((buffer-name (format "*Catch2 Tests: %s*" suite-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (catch2-testcases-mode)

      (setq tabulated-list-entries
            (mapcar (lambda (case)
                      (let* ((name (plist-get case :name))
                            (success (plist-get case :success))
                            (duration (plist-get case :durationInSeconds))
                            (filename (plist-get case :filename))
                            (line (plist-get case :line))
                            (tags (plist-get case :tags))
                            (basename (when filename
                                        (file-name-nondirectory filename))))
                        (list name ; key
                              (vector
                               (propertize
                                (if success "✓ PASS" "✗ FAIL")
                                'face (if success
                                        '(:foreground "green" :weight bold)
                                      '(:foreground "red" :weight bold)))
                               name
                               (format "%.3fs" (or duration 0.0))
                               (propertize (or basename "N/A") 'full-filename filename)
                               (if (and line (> line 0))
                                   (number-to-string line)
                                 "N/A")
                               (mapconcat 'identity tags ", ")))))
                    cases))

      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun catch2-testcases-open-file ()
  "Open the test case file at the line number."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (filename-cell (aref entry 3))  ; File column cell
         (full-filename (get-text-property 0 'full-filename filename-cell))
         (line-str (aref entry 4))  ; Line column
         (line (when (stringp line-str)
                 (string-to-number line-str))))
    (if (and full-filename line (> line 0))
        (progn
          (find-file full-filename)
          (goto-char (point-min))
          (forward-line (1- line)))
      (message "No file location available for this test case"))))

(defun catch2-find-log-file (xml-file test-suite-name test-case-name)
  "Find the log file for a test case.
XML-FILE is the path to the XML results file.
TEST-SUITE-NAME is the name of the test suite (may include [preset] suffix in display).
TEST-CASE-NAME is the name of the test case.
Returns the path to the log file or nil if not found."
  (let* ((xml-dir (file-name-directory xml-file))
         (log-dir (expand-file-name "../log" xml-dir))
         ;; Strip any [preset] suffix from suite name for log directory lookup
         (base-suite-name (if (string-match "^\\(.+\\) \\[.+\\]$" test-suite-name)
                              (match-string 1 test-suite-name)
                            test-suite-name))
         (suite-log-dir (expand-file-name base-suite-name log-dir))
         (log-pattern (format "%s*.log" test-case-name))
         (log-files (when (file-exists-p suite-log-dir)
                      (directory-files-recursively suite-log-dir log-pattern))))
    (catch2--debug "Looking for log file: XML=%s, suite=%s, test=%s" xml-file test-suite-name test-case-name)
    (catch2--debug "Base suite name: %s" base-suite-name)
    (catch2--debug "Log directory: %s" suite-log-dir)
    (catch2--debug "Log pattern: %s" log-pattern)
    (catch2--debug "Found log files: %S" log-files)

    (car log-files))) ; Return the first matching log file

(defun catch2-test-summaries ()
  "Test function to generate suite summaries."
  (interactive)
  (let* ((suites (catch2-parse-all-suites))
        (summaries (catch2-generate-suite-summaries suites)))
    (message "Generated %d suite summaries:" (length summaries))
    (dolist (summary summaries)
      (message "  %s: %s (%d tests, %.3fs)"
               (plist-get summary :suite-name)
               (plist-get summary :status)
               (plist-get summary :test-count)
               (plist-get summary :total-duration)))
    summaries))

(defun catch2-parse-all-suites (&optional directory)
  "Parse all Catch2 XML suites in DIRECTORY or current project.
Return list of suite plists."
  (let ((files (catch2-locate-xml-files directory))
        suites)
    (dolist (file files)
      (condition-case err
          (push (catch2-parse-suite file) suites)
        (error
         (catch2--debug "Error parsing %s: %s" file (error-message-string err))
         (message "Error parsing %s: %s" file (error-message-string err)))))
    (nreverse suites)))

(defun catch2 (directory)
  "Open the Catch2 test suites overview.
Prompts for DIRECTORY to search for Catch2 XML test result files."
  (interactive
   (list (read-directory-name "Search for Catch2 tests in: "
                              (or (when-let ((proj (project-current)))
                                    (project-root proj))
                                  default-directory))))
  (catch2-tabulated-display directory))

(provide 'catch2-mode)
;;; catch2-mode.el ends here
