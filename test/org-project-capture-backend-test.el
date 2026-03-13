;;; org-project-capture-backend-test.el --- Backend test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ivan Malison

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

;; Tests for org-project-capture-backend.el

;;; Code:

(require 'ert)
(require 'org-project-capture-backend)

;; Tests for org-project-capture-category-from-project-root

(ert-deftest test-category-from-project-root-simple ()
  "Test extracting category from simple project path."
  (should (string-equal
           (org-project-capture-category-from-project-root "/home/user/myproject")
           "myproject")))

(ert-deftest test-category-from-project-root-trailing-slash ()
  "Test extracting category from path with trailing slash."
  (should (string-equal
           (org-project-capture-category-from-project-root "/home/user/myproject/")
           "myproject")))

(ert-deftest test-category-from-project-root-nested ()
  "Test extracting category from deeply nested path."
  (should (string-equal
           (org-project-capture-category-from-project-root "/a/b/c/d/deep-project")
           "deep-project")))

(ert-deftest test-category-from-project-root-nil ()
  "Test that nil input returns nil."
  (should (null (org-project-capture-category-from-project-root nil))))

(ert-deftest test-category-from-project-root-special-chars ()
  "Test extracting category with special characters."
  (should (string-equal
           (org-project-capture-category-from-project-root "/home/user/my-project_v2.0")
           "my-project_v2.0")))

;; Tests for backend class structure

(ert-deftest test-project-backend-is-backend ()
  "Test that project-backend inherits from backend."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (should (object-of-class-p backend 'org-project-capture-backend))))

;; Tests for build-category-to-project-path

(ert-deftest test-build-category-to-project-path-empty ()
  "Test building category map with no projects."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil)))
      (should (null (org-project-capture-build-category-to-project-path backend))))))

(ert-deftest test-build-category-to-project-path-single ()
  "Test building category map with single project."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/home/user/myproject"))))
      (let ((result (org-project-capture-build-category-to-project-path backend)))
        (should (= (length result) 1))
        (should (equal (car result) '("myproject" . "/home/user/myproject")))))))

(ert-deftest test-build-category-to-project-path-multiple ()
  "Test building category map with multiple projects."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/path/to/alpha" "/path/to/beta" "/path/to/gamma"))))
      (let ((result (org-project-capture-build-category-to-project-path backend)))
        (should (= (length result) 3))
        (should (assoc "alpha" result))
        (should (assoc "beta" result))
        (should (assoc "gamma" result))))))

(ert-deftest test-build-category-to-project-path-prefers-project-name ()
  "Test building category map uses `project-name' when available."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/path/to/alpha")))
              ((symbol-function 'project-current)
               (lambda (_maybe-prompt dir)
                 (when (string-prefix-p "/path/to/alpha" dir)
                   'fake-project)))
              ((symbol-function 'project-name)
               (lambda (project)
                 (should (eq project 'fake-project))
                 "renamed-alpha")))
      (let ((result (org-project-capture-build-category-to-project-path backend)))
        (should (equal result '(("renamed-alpha" . "/path/to/alpha"))))))))

;; Tests for category-from-file

(ert-deftest test-category-from-file ()
  "Test getting category from file path."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (_maybe-prompt dir)
                 (when (string-prefix-p "/home/user/myproj" dir)
                   'fake-project)))
              ((symbol-function 'project-root)
               (lambda (project)
                 (should (eq project 'fake-project))
                 "/home/user/myproj")))
      (should (string-equal
               (org-project-capture-category-from-file
                backend "/home/user/myproj/src/file.el")
               "myproj")))))

(ert-deftest test-category-from-file-not-in-project ()
  "Test category-from-file when file is not in a project."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (_maybe-prompt _dir) nil)))
      (should (null (org-project-capture-category-from-file
                     backend "/tmp/random-file.txt"))))))

(ert-deftest test-current-project ()
  "Test getting the current project name."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) 'fake-project))
              ((symbol-function 'project-name)
               (lambda (project)
                 (should (eq project 'fake-project))
                 "myproj")))
      (should (string-equal
               (org-project-capture-current-project backend)
               "myproj")))))

(ert-deftest test-current-project-not-in-project ()
  "Test current-project when not in a project."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) nil)))
      (should (null (org-project-capture-current-project backend))))))

;; Tests for get-all-categories

(ert-deftest test-get-all-categories ()
  "Test getting all categories from backend."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/p/one" "/p/two" "/p/three"))))
      (let ((categories (org-project-capture-get-all-categories backend)))
        (should (= (length categories) 3))
        (should (member "one" categories))
        (should (member "two" categories))
        (should (member "three" categories))))))

(ert-deftest test-get-all-categories-empty ()
  "Test getting categories when no projects exist."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () nil)))
      (should (null (org-project-capture-get-all-categories backend))))))

;; Tests for duplicate project names

(ert-deftest test-duplicate-project-names ()
  "Test handling of projects with same directory name in different paths."
  (let ((backend (make-instance 'org-project-capture-project-backend)))
    (cl-letf (((symbol-function 'project-known-project-roots)
               (lambda () '("/work/myproj" "/personal/myproj"))))
      (let ((result (org-project-capture-build-category-to-project-path backend)))
        ;; Both have same category name - last one wins in alist lookup
        (should (= (length result) 2))
        (should (assoc "myproj" result))))))

(provide 'org-project-capture-backend-test)
;;; org-project-capture-backend-test.el ends here
