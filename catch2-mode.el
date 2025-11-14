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

(defcustom catch2-xml-file-pattern "test-results-*.tests.xml"
  "Pattern to match Catch2 XML test result files."
  :type 'string
  :group 'catch2-mode)

(defcustom catch2-project-name "Catch2 Tests: "
  "Default project name to show in the buffer."
  :type 'string
  :group 'catch2-mode)

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
Return plist (:name STRING :result BOOLEAN :file XML-FILE :tags
LIST :filename STRING :line NUMBER)."
  (let* ((attrs (xml-node-attributes case-node))
         (case-name (cdr (assq 'name attrs)))
         (tags (catch2-parse-tags (cdr (assq 'tags attrs))))
         (filename (cdr (assq 'filename attrs)))
         (line (string-to-number (or (cdr (assq 'line attrs)) "0")))
         (children (xml-node-children case-node))
         result)

    ;; Find <OverallResult success="...">
    (dolist (child children)
      (when (and (consp child)
                 (eq (car child) 'OverallResult))
        (setq result (string= (cdr (assq 'success (xml-node-attributes child)))
                              "true"))))

    (catch2--debug "Parsed test case: %s (result: %s, tags: %S)" case-name result tags)

    `(:name ,case-name
      :result ,result
      :file ,xml-file
      :tags ,tags
      :filename ,filename
      :line ,line)))

(defun catch2-parse-testcase (case-node xml-file)
  "Parse a <TestCase> element CASE-NODE from XML-FILE.
Return plist with all test case attributes and overall result data."
  (let* ((attrs (xml-node-attributes case-node))
         (case-name (cdr (assq 'name attrs)))
         (tags (catch2-parse-tags (cdr (assq 'tags attrs))))
         (filename (cdr (assq 'filename attrs)))
         (line (string-to-number (or (cdr (assq 'line attrs)) "0")))
         (children (xml-node-children case-node))
         result
         duration
         skips)

    ;; Find <OverallResult> element
    (dolist (child children)
      (when (and (consp child)
                 (eq (car child) 'OverallResult))
        (let ((result-attrs (xml-node-attributes child)))
          (setq result (string= (cdr (assq 'success result-attrs)) "true"))
          (setq duration (string-to-number
                          (or (cdr (assq 'durationInSeconds result-attrs)) "0.0")))
          (setq skips (string-to-number
                       (or (cdr (assq 'skips result-attrs)) "0"))))))

    (catch2--debug "Parsed test case: %s (result: %s, duration: %.3fs, skips: %d, tags: %S)"
                   case-name result duration skips tags)

    `(:name ,case-name
      :result ,result
      :file ,xml-file
      :tags ,tags
      :filename ,filename
      :line ,line
      :duration ,duration
      :skips ,skips
      :raw-attrs ,attrs)))

(defun catch2-parse-suite (xml-file)
  "Parse a Catch2 v3 XML suite from XML-FILE.
Return plist with all suite attributes and test cases."
  (catch2--debug "Parsing suite file: %s" xml-file)
  (let* ((xml (with-temp-buffer
                (insert-file-contents xml-file)
                (xml-parse-region (point-min) (point-max))))
         (root (car xml)) ;; <Catch2TestRun ...>
         (attrs (xml-node-attributes root))
         (suite-name (cdr (assq 'name attrs)))
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

    (let ((suite-plist `(:suite ,suite-name
                        :cases ,(nreverse cases)
                        :file ,xml-file
                        :rng-seed ,rng-seed
                        :xml-format-version ,xml-format-version
                        :catch2-version ,catch2-version
                        :overall-results ,overall-results
                        :overall-results-cases ,overall-results-cases
                        :raw-attrs ,attrs)))
      (catch2--debug "Parsed suite: %s with %d test cases" suite-name (length cases))
      (catch2--debug "Suite attributes: rng-seed=%s, xml-format=%s, catch2-version=%s"
                     rng-seed xml-format-version catch2-version)
      suite-plist)))

;; Test function
(defun catch2-test-summaries ()
  "Test function to generate suite summaries."
  (interactive)
  (let ((suites (catch2-parse-all-suites))
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
