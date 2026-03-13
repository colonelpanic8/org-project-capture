;;; org-projectile-test.el --- org-project-capture test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Ivan Malison

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The unit test suite of org-project-capture

;;; Code:

(require 'ert)
(require 'noflet)

(require 'org-project-capture)
(require 'org-projectile)
(setq org-adapt-indentation 1)

(defun equal-as-sets (seq1 seq2)
  "Return t if SEQ1 and SEQ2 contain the same elements."
  (and
   (-all? (lambda (element) (member element seq2)) seq1)
   (-all? (lambda (element) (member element seq1)) seq2)))

(defun org-project-capture-test-join-paths (root &rest dirs)
  "Join ROOT with DIRS to form a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defvar org-project-capture-test-data-directory
  (org-project-capture-test-join-paths
   (file-name-directory (or load-file-name buffer-file-name)) "data")
  "Directory containing test data files.")

(defun org-project-capture-test-file (&rest paths)
  "Return path to test file by joining PATHS with test data directory."
  (apply 'org-project-capture-test-join-paths org-project-capture-test-data-directory
         paths))

(defvar org-project-capture-test-all-projects
  (org-project-capture-test-file "all_projects.org")
  "Path to the test all_projects.org file.")

;; Tests for single file strategy

(ert-deftest test-org-project-capture-supports-various-heading-types ()
  "Test that various heading formats are recognized as categories."
  (let ((org-project-capture-strategy (make-instance 'org-project-capture-single-file-strategy))
        (org-project-capture-projects-file org-project-capture-test-all-projects)
        (org-project-capture-default-backend
         (make-instance 'org-project-capture-projectile-backend))
        (projectile-known-projects nil))
    (should (equal-as-sets
             (occ-get-categories org-project-capture-strategy)
             '("proj1" "ideas2" "test" "proj4" "proj3" "github-search" "c")))))

;; Tests for per-project strategy

(ert-deftest test-org-project-capture-per-project-filepath-with-function ()
  "Test per-project-filepath works with both function and string values."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-per-project-strategy))
         (a-project (org-project-capture-test-file "a"))
         (b-project (org-project-capture-test-file "b"))
         (projectile-known-projects
          (list a-project b-project))
         (org-project-capture-per-project-filepath
          (lambda (path)
            (if (string-equal path b-project)
                "OTHER.org"
              "TODO.org"))))
    (should (equal-as-sets
             (org-project-capture-todo-files)
             (list (org-project-capture-test-join-paths a-project "TODO.org")
                   (org-project-capture-test-join-paths b-project "OTHER.org"))))
    (let ((org-project-capture-per-project-filepath "COOL.org"))
      (should (equal-as-sets
               (org-project-capture-todo-files)
               ;; The "b" project does not have a COOL.org
               (list (org-project-capture-test-join-paths a-project "COOL.org")))))))

(ert-deftest test-org-project-capture-per-project-empty-projects ()
  "Test per-project strategy with no known projects."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-per-project-strategy))
         (projectile-known-projects nil))
    (should (null (occ-get-categories org-project-capture-strategy)))
    (should (null (org-project-capture-todo-files)))))

(ert-deftest test-org-project-capture-per-project-nonexistent-todo ()
  "Test per-project strategy filters out projects without TODO files."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-per-project-strategy))
         (c-project (org-project-capture-test-file "c"))
         (projectile-known-projects (list c-project))
         (org-project-capture-per-project-filepath "TODO.org"))
    ;; c project has no TODO.org file
    (should (null (occ-get-existing-categories org-project-capture-strategy)))))

;; Tests for combine strategies

(defun org-project-capture-test-get-project-capture-filepath (project-name)
  "Get the filepath where captures for PROJECT-NAME would go."
  (with-current-buffer
      (marker-buffer (occ-get-capture-marker
                      (make-instance 'occ-context
                                     :category project-name
                                     :template org-project-capture-capture-template
                                     :strategy org-project-capture-strategy)))
    (buffer-file-name)))

(ert-deftest test-org-project-capture-combine-strategies ()
  "Test that combine strategies merges categories from all sub-strategies."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-combine-strategies))
         (a-project (org-project-capture-test-file "a"))
         (b-project (org-project-capture-test-file "b"))
         (c-project (org-project-capture-test-file "c"))
         (org-project-capture-projects-file org-project-capture-test-all-projects)
         (projectile-known-projects
          (list a-project b-project c-project)))
    (should (equal-as-sets
             (occ-get-categories org-project-capture-strategy)
             '("proj1" "ideas2" "test" "proj4" "proj3" "github-search" "a" "b" "c")))
    (should (string-equal (org-project-capture-test-get-project-capture-filepath "c")
                          org-project-capture-test-all-projects))
    (should (string-equal (org-project-capture-test-get-project-capture-filepath "a")
                          (org-project-capture-test-join-paths a-project "TODO.org")))))

(ert-deftest test-org-project-capture-combine-selects-correct-strategy ()
  "Test that combine strategy routes to correct sub-strategy."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-combine-strategies))
         (a-project (org-project-capture-test-file "a"))
         (org-project-capture-projects-file org-project-capture-test-all-projects)
         (projectile-known-projects (list a-project)))
    ;; proj1 exists in the single file, should use single-file-strategy
    (let ((selected (org-project-capture-select-strategy
                     org-project-capture-strategy "proj1")))
      (should (eq (eieio-object-class selected)
                  'org-project-capture-single-file-strategy)))
    ;; a exists as per-project, should use per-project-strategy
    (let ((selected (org-project-capture-select-strategy
                     org-project-capture-strategy "a")))
      (should (eq (eieio-object-class selected)
                  'org-project-capture-per-project-strategy)))))

(ert-deftest test-org-project-capture-projectile-backend-uses-custom-project-names ()
  "Test that projectile category maps follow `projectile-project-name-function'."
  (let* ((backend (make-instance 'org-project-capture-projectile-backend))
         (a-project (org-project-capture-test-file "a"))
         (b-project (org-project-capture-test-file "b"))
         (projectile-known-projects (list a-project b-project))
         (projectile-project-name-function
          (lambda (project-root)
            (format "named-%s"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (should (equal
             (org-project-capture-build-category-to-project-path backend)
             `(("named-a" . ,a-project)
               ("named-b" . ,b-project))))
    (should (equal-as-sets
             (org-project-capture-get-all-categories backend)
             '("named-a" "named-b")))))

(ert-deftest test-org-project-capture-current-project-agenda-settings-single-file ()
  "Test agenda settings for single-file strategy."
  (let ((org-project-capture-strategy
         (make-instance 'org-project-capture-single-file-strategy)))
    (cl-letf (((symbol-function 'org-project-capture-strategy-get-backend)
               (lambda (_strategy) 'fake-backend))
              ((symbol-function 'org-project-capture-current-project)
               (lambda (_backend) "proj1"))
              ((symbol-function 'occ-get-capture-file)
               (lambda (_strategy category)
                 (should (string-equal category "proj1"))
                 "/tmp/projects.org")))
      (should (equal
               (org-project-capture-current-project-agenda-settings)
               '("proj1" "/tmp/projects.org" "CATEGORY=\"proj1\""))))))

(ert-deftest test-org-project-capture-current-project-agenda-settings-per-project ()
  "Test agenda settings for per-project strategy."
  (let ((org-project-capture-strategy
         (make-instance 'org-project-capture-per-project-strategy)))
    (cl-letf (((symbol-function 'org-project-capture-strategy-get-backend)
               (lambda (_strategy) 'fake-backend))
              ((symbol-function 'org-project-capture-current-project)
               (lambda (_backend) "proj1"))
              ((symbol-function 'occ-get-capture-file)
               (lambda (_strategy category)
                 (should (string-equal category "proj1"))
                 "/tmp/proj1/TODO.org")))
      (should (equal
               (org-project-capture-current-project-agenda-settings)
               '("proj1" "/tmp/proj1/TODO.org" nil))))))

;; Tests for heading text processing

(ert-deftest test-org-project-capture-get-category-from-heading-strips-links ()
  "Test that linked headings are properly stripped to get category."
  (with-temp-buffer
    (org-mode)
    (insert "* [[elisp:(org-project-capture-open-project \"test\")][test]]")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (should (string-equal (org-project-capture-get-category-from-heading) "test"))))

(ert-deftest test-org-project-capture-get-category-from-heading-strips-counts ()
  "Test that heading counts like [0/5] are stripped from category."
  (with-temp-buffer
    (org-mode)
    (insert "* myproject [2/5]")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (should (string-equal (org-project-capture-get-category-from-heading) "myproject"))))

(ert-deftest test-org-project-capture-get-category-from-heading-both ()
  "Test heading with both link and count."
  (with-temp-buffer
    (org-mode)
    (insert "* [[elisp:(something)][linked-proj]] [0/10]")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (should (string-equal (org-project-capture-get-category-from-heading) "linked-proj"))))

(ert-deftest test-org-project-capture-get-category-plain-heading ()
  "Test plain heading without links or counts."
  (with-temp-buffer
    (org-mode)
    (insert "* simple-project")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (should (string-equal (org-project-capture-get-category-from-heading) "simple-project"))))

;; Tests for linked heading generation

(ert-deftest test-org-project-capture-linked-heading ()
  "Test that linked heading creates proper elisp link."
  (let ((result (org-project-capture-linked-heading "myproj")))
    (should (string-match-p "elisp:" result))
    (should (string-match-p "org-project-capture-open-project" result))
    (should (string-match-p "myproj" result))))

(ert-deftest test-org-project-capture-build-heading-with-link ()
  "Test build-heading adds link when force-linked is t."
  (let ((org-project-capture-force-linked t)
        (org-project-capture-counts-in-heading nil))
    (let ((result (org-project-capture-build-heading "test")))
      (should (string-match-p "\\[\\[" result)))))

(ert-deftest test-org-project-capture-build-heading-with-count ()
  "Test build-heading adds count when counts-in-heading is t."
  (let ((org-project-capture-force-linked nil)
        (org-project-capture-counts-in-heading t))
    (let ((result (org-project-capture-build-heading "test")))
      (should (string-match-p "\\[/\\]" result)))))

(ert-deftest test-org-project-capture-build-heading-plain ()
  "Test build-heading without link or count."
  (let ((org-project-capture-force-linked nil)
        (org-project-capture-counts-in-heading nil))
    (let ((result (org-project-capture-build-heading "test")))
      (should (string-equal result "test")))))

;; Tests for IO permission checking

(ert-deftest test-org-project-capture-io-action-permitted-local ()
  "Test that local paths are always permitted."
  (should (org-project-capture-io-action-permitted "/home/user/project")))

(ert-deftest test-org-project-capture-io-action-permitted-tramp-disabled ()
  "Test that tramp paths are blocked when allow-tramp is nil."
  (let ((org-project-capture-allow-tramp-projects nil))
    (should-not (org-project-capture-io-action-permitted "/ssh:host:/path"))))

(ert-deftest test-org-project-capture-io-action-permitted-tramp-enabled ()
  "Test that tramp paths are allowed when allow-tramp is t."
  (let ((org-project-capture-allow-tramp-projects t))
    (should (org-project-capture-io-action-permitted "/ssh:host:/path"))))

;; Tests for utility functions

(ert-deftest test-org-project-capture-invert-alist ()
  "Test alist inversion swaps keys and values."
  (let ((alist '(("a" . 1) ("b" . 2) ("c" . 3))))
    (should (equal (org-project-capture-invert-alist alist)
                   '((1 . "a") (2 . "b") (3 . "c"))))))

(ert-deftest test-org-project-capture-invert-alist-empty ()
  "Test alist inversion with empty list."
  (should (null (org-project-capture-invert-alist nil))))

;; Tests for todo files

(ert-deftest test-org-project-capture-todo-files-filters-unreadable ()
  "Test that todo-files filters out unreadable files."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (org-project-capture-strategy (make-instance 'org-project-capture-per-project-strategy))
         (a-project (org-project-capture-test-file "a"))
         (c-project (org-project-capture-test-file "c"))
         (projectile-known-projects (list a-project c-project))
         (org-project-capture-per-project-filepath "TODO.org"))
    ;; a has TODO.org, c does not
    (let ((files (org-project-capture-todo-files)))
      (should (= (length files) 1))
      (should (string-match-p "/a/TODO.org" (car files))))))

;; Tests for project directory handling

(ert-deftest test-org-project-capture-projects-directory ()
  "Test that projects-directory overrides per-project location."
  (let* ((org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
         (a-project (org-project-capture-test-file "a"))
         (projectile-known-projects (list a-project))
         (org-project-capture-projects-directory "/tmp/todos/")
         (org-project-capture-per-project-filepath "TODO.org"))
    (should (string-equal
             (org-project-capture-get-project-todo-file a-project)
             "/tmp/todos/TODO.org"))))

(ert-deftest test-org-project-capture-get-project-todo-file-default ()
  "Test default project todo file path."
  (let* ((org-project-capture-projects-directory nil)
         (org-project-capture-per-project-filepath "TODO.org"))
    (should (string-equal
             (org-project-capture-get-project-todo-file "/home/user/myproj/")
             "/home/user/myproj/TODO.org"))))

;; Tests for single file strategy specifics

(ert-deftest test-single-file-strategy-todo-files ()
  "Test single-file strategy returns only the projects file."
  (let ((org-project-capture-projects-file "/path/to/projects.org")
        (strategy (make-instance 'org-project-capture-single-file-strategy)))
    (should (equal (occ-get-todo-files strategy)
                   '("/path/to/projects.org")))))

(ert-deftest test-single-file-strategy-capture-file ()
  "Test single-file strategy always returns projects file."
  (let ((org-project-capture-projects-file "/path/to/projects.org")
        (strategy (make-instance 'org-project-capture-single-file-strategy)))
    (should (string-equal (occ-get-capture-file strategy "any-category")
                          "/path/to/projects.org"))))

(ert-deftest test-single-file-strategy-target-entry-p ()
  "Test that single-file strategy targets entries."
  (let ((strategy (make-instance 'org-project-capture-single-file-strategy)))
    (should (occ-target-entry-p strategy nil))))

;; Tests for per-project strategy specifics

(ert-deftest test-per-project-strategy-target-entry-p ()
  "Test that per-project strategy does not target entries."
  (let ((strategy (make-instance 'org-project-capture-per-project-strategy)))
    (should-not (occ-target-entry-p strategy nil))))

;; Tests for linked heading regexp

(ert-deftest test-org-project-capture-linked-heading-regexp ()
  "Test linked heading regexp matches various link formats."
  (let ((regexp (org-project-capture-linked-heading-regexp "myproj")))
    (should (string-match-p regexp "[[elisp:(something)][myproj]]"))
    (should (string-match-p regexp "[[https://example.com][myproj]]"))
    (should-not (string-match-p regexp "[[elisp:(something)][other]]"))))

(provide 'org-projectile-test)
;;; org-projectile-test.el ends here
