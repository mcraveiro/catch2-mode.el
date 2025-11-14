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

(defcustom catch2-project-name "Catch2 Tests"
  "Default project name to show in the buffer."
  :type 'string
  :group 'catch2-mode)

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
         success
         durationInSeconds
         skips)

    ;; Find <OverallResult> element
    (dolist (child children)
      (when (and (consp child)
                 (eq (car child) 'OverallResult))
        (let ((result-attrs (xml-node-attributes child)))
          (setq success (string= (cdr (assq 'success result-attrs)) "true"))
          (setq durationInSeconds (string-to-number
                                   (or (cdr (assq 'durationInSeconds result-attrs)) "0.0")))
          (setq skips (string-to-number
                       (or (cdr (assq 'skips result-attrs)) "0"))))))

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

(defun catch2-parse-suite (xml-file)
  "Parse a Catch2 v3 XML suite from XML-FILE.
Return plist with all suite attributes and test cases."
  (catch2--debug "Parsing suite file: %s" xml-file)
  (let* ((file-attributes (file-attributes xml-file))
         (modification-time (file-attribute-modification-time file-attributes))
         (xml (with-temp-buffer
                (insert-file-contents xml-file)
                (xml-parse-region (point-min) (point-max))))
         (root (car xml)) ;; <Catch2TestRun ...>
         (attrs (xml-node-attributes root))
         (name (cdr (assq 'name attrs)))
         (rng-seed (cdr (assq 'rng-seed attrs)))
         (xml-format-version (cdr (assq 'xml-format-version attrs)))
         (catch2-version (cdr (assq 'catch2-version attrs)))
         (children (xml-node-children root))
         cases
         overall-results
         overall-results-cases)

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
      (catch2--debug "Suite attributes: rng-seed=%s, xml-format-version=%s, catch2-version=%s, modification-time=%S"
                     rng-seed xml-format-version catch2-version modification-time)
      suite-plist)))

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
                                 :status ,(if has-failures 'fail 'pass)
                                 :durationInSeconds ,total-durationInSeconds
                                 :tags ,(mapconcat 'identity unique-tags ", ")
                                 :modification-time ,modification-time
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
        (oldest-mod-time nil))

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

        ;; Find the oldest modification time
        (when mod-time
          (if (or (null oldest-mod-time)
                  (time-less-p mod-time oldest-mod-time))
              (setq oldest-mod-time mod-time)))))

    (let ((totals-summary `(:suite-name "TOTALS"
                            :status ,(if has-any-failures 'fail 'pass)
                            :durationInSeconds ,total-durationInSeconds
                            :test-count ,total-test-count
                            :fail-count ,total-fail-count
                            :modification-time ,oldest-mod-time
                            :is-totals t)))

      (catch2--debug "Generated totals: %d tests, %d failed, %.3fs, status: %s, oldest mod-time: %S"
                     total-test-count total-fail-count total-durationInSeconds
                     (if has-any-failures "FAIL" "PASS") oldest-mod-time)
      totals-summary)))

(defvar catch2-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'catch2-tabulated-view-suite)
    map))

(defvar catch2-tabulated-mode-hook nil
  "Hook run when entering `catch2-tabulated-mode'.")

(define-derived-mode catch2-tabulated-mode tabulated-list-mode "Catch2 Suites"
  "Major mode for viewing Catch2 test suites in a tabulated list."
  (setq tabulated-list-format
        [("Status" 8 t)
         ("Suite Name" 40 t)
         ("Tests" 8 t :right-align t)
         ("Duration" 12 t :right-align t)
         ("Fails" 8 t :right-align t)
         ("Modified" 16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Suite Name" nil))
  (tabulated-list-init-header)
  (run-hooks 'catch2-tabulated-mode-hook))

;; Add hl-line-mode to the hook
(add-hook 'catch2-tabulated-mode-hook #'hl-line-mode)

(defun catch2-tabulated-display ()
  "Display Catch2 test suites in a tabulated list buffer."
  (interactive)
  (let* ((project (project-current))
         (project-name (if project
                           (project-name project)
                         "No Project"))
         (suites (catch2-parse-all-suites))
         (summaries (catch2-generate-suite-summaries suites))
         (totals-summary (catch2-generate-totals-summary summaries))
         (buffer-name (format "*%s: %s*" catch2-project-name project-name)))

    (catch2--debug "Displaying %d suites + totals in buffer: %s" (length summaries) buffer-name)

    (if (null summaries)
        (message "No Catch2 test suites found")
      (with-current-buffer (get-buffer-create buffer-name)
        (catch2-tabulated-mode)

        ;; Combine regular summaries with totals
        (setq tabulated-list-entries
              (append
               (mapcar (lambda (summary)
                         (let ((fail-count (plist-get summary :fail-count))
                               (mod-time (plist-get summary :modification-time))
                               (status (plist-get summary :status))
                               (suite-name (plist-get summary :suite-name)))
                           (list suite-name ; key
                                 (vector
                                  (propertize
                                   (if (eq status 'pass) "✓ PASS" "✗ FAIL")
                                   'face (if (eq status 'pass)
                                           '(:weight bold)
                                         '(:weight bold)))
                                  (propertize suite-name 'face (if (eq status 'pass)
                                                                 '(:weight bold)
                                                               '(:weight bold)))
                                  (propertize (number-to-string (plist-get summary :test-count))
                                             'face (if (eq status 'pass)
                                                     '(:weight bold)
                                                   '(:weight bold)))
                                  (propertize (format "%.3fs" (plist-get summary :durationInSeconds))
                                             'face (if (eq status 'pass)
                                                     '(:weight bold)
                                                   '(:weight bold)))
                                  (propertize (number-to-string fail-count)
                                             'face (if (eq status 'pass)
                                                     '(:weight bold)
                                                   '(:weight bold)))
                                  (propertize (if mod-time
                                                  (format-time-string "%Y-%m-%d %H:%M" mod-time)
                                                "N/A")
                                             'face (if (eq status 'pass)
                                                     '(:weight bold)
                                                   '(:weight bold))))
                                 :face (if (eq status 'pass)
                                         '(:background "dark green" :foreground "white")
                                       '(:background "dark red" :foreground "white")))))
                       summaries)
               (list (list "TOTALS" ; key for totals row
                           (let ((total-fail-count (plist-get totals-summary :fail-count))
                                 (total-status (plist-get totals-summary :status)))
                             (vector
                              (propertize
                               (if (eq total-status 'pass) "✓ PASS" "✗ FAIL")
                               'face '(:weight bold :height 1.1))
                              (propertize "TOTALS" 'face '(:weight bold :height 1.1))
                              (propertize (number-to-string (plist-get totals-summary :test-count))
                                         'face '(:weight bold :height 1.1))
                              (propertize (format "%.3fs" (plist-get totals-summary :durationInSeconds))
                                         'face '(:weight bold :height 1.1))
                              (propertize (number-to-string total-fail-count)
                                         'face '(:weight bold :height 1.1))
                              "")
                             :face (if (eq total-status 'pass)
                                     '(:background "dark green" :foreground "white" :height 1.1)
                                   '(:background "dark red" :foreground "white" :height 1.1)))))))

        (tabulated-list-print)
        (switch-to-buffer (current-buffer))))))

(defun catch2-tabulated-view-suite ()
  "View details of the test suite at point."
  (interactive)
  (let ((suite-name (tabulated-list-get-id)))
    (message "Viewing suite: %s" suite-name)))

;;
;; Testing
;;

;; Test function
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

;; Test function to verify it works
(defun catch2-test-parse-tags ()
  "Test function for tag parsing."
  (interactive)
  (let ((test-cases '("[handler][messaging][#messaging_risk_message_handler_tests]"
                      "[parsing][#parser_tests]"
                      "[#single_tag]"
                      "[]"
                      nil)))
    (dolist (tags test-cases)
      (message "Input: %-60s Output: %S" tags (catch2-parse-tags tags)))))

(provide 'catch2-mode)
;;; catch2-mode.el ends here
