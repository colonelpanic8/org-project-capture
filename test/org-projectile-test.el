;;; org-project-capture-test.el --- org-project-capture test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023 Ivan Malison

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
  (and
   (-all? (lambda (element) (member element seq2)) seq1)
   (-all? (lambda (element) (member element seq1)) seq2)))

(defun org-project-capture-test-join-paths (root &rest dirs)
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defvar org-project-capture-test-data-directory
  (org-project-capture-test-join-paths
   (file-name-directory (or load-file-name buffer-file-name)) "data"))

(defun org-project-capture-test-file (&rest paths)
  (apply 'org-project-capture-test-join-paths org-project-capture-test-data-directory
         paths))

(defvar org-project-capture-test-all-projects (org-project-capture-test-file "all_projects.org"))

(ert-deftest test-org-project-capture-supports-various-heading-types ()
  (let ((org-project-capture-strategy (make-instance 'org-project-capture-single-file-strategy))
        (org-project-capture-projects-file org-project-capture-test-all-projects)
        (org-project-capture-default-backend
         (make-instance 'org-project-capture-projectile-backend))
        (projectile-known-projects nil))
    (should (equal-as-sets
             (occ-get-categories org-project-capture-strategy)
             '("proj1" "ideas2" "test" "proj4" "proj3" "github-search" "c")))))

(ert-deftest test-org-project-capture-per-project-filepath-with-function ()
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
               (list (org-project-capture-test-join-paths a-project "COOL.org")
                     (org-project-capture-test-join-paths b-project "COOL.org")))))))

(defun org-project-capture-test-get-project-capture-filepath (project-name)
  (with-current-buffer
      (marker-buffer (occ-get-capture-marker
                      (make-instance 'occ-context
                                     :category project-name
                                     :template org-project-capture-capture-template
                                     :strategy org-project-capture-strategy)))
    (buffer-file-name)))

(ert-deftest test-org-project-capture-combine-strategies ()
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

(provide 'org-projectile-test)
;;; org-projectile-test.el ends here
