;;; org-projectile-test.el --- org-projectile test suite -*- lexical-binding: t; -*-

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

;; The unit test suite of org-projectile

;;; Code:

(require 'ert)
(require 'noflet)

(require 'org-projectile)
(setq org-adapt-indentation 1)

(defun equal-as-sets (seq1 seq2)
  (and
   (-all? (lambda (element) (member element seq2)) seq1)
   (-all? (lambda (element) (member element seq1)) seq2)))

(defun org-projectile-test-join-paths (root &rest dirs)
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defvar org-projectile-test-data-directory
  (org-projectile-test-join-paths
   (file-name-directory (or load-file-name buffer-file-name)) "data"))

(defun org-projectile-test-file (&rest paths)
  (apply 'org-projectile-test-join-paths org-projectile-test-data-directory
         paths))

(defvar org-projectile-test-all-projects (org-projectile-test-file "all_projects.org"))

(ert-deftest test-org-projectile-supports-various-heading-types ()
  (setq org-projectile-strategy
        (make-instance 'org-projectile-single-file-strategy)
        org-projectile-projects-file org-projectile-test-all-projects)
  (let ((projectile-known-projects nil))
    (should (equal-as-sets
             (occ-get-categories org-projectile-strategy)
             '("proj1" "ideas2" "test" "proj4" "proj3" "github-search" "c")))))

(ert-deftest test-org-projectile-per-project-filepath-with-function ()
  (let* ((org-projectile-strategy (make-instance 'org-projectile-per-project-strategy))
         (a-project (org-projectile-test-file "a"))
         (b-project (org-projectile-test-file "b"))
         (projectile-known-projects
          (list a-project b-project))
         (org-projectile-per-project-filepath
          (lambda (path)
            (if (string-equal path b-project)
                "OTHER.org"
              "TODO.org"))))
    (should (equal-as-sets
             (org-projectile-todo-files)
             (list (org-projectile-test-join-paths a-project "TODO.org")
                   (org-projectile-test-join-paths b-project "OTHER.org"))))
    (let ((org-projectile-per-project-filepath "COOL.org"))
      (should (equal-as-sets
               (org-projectile-todo-files)
               ;; The "b" project does not have a COOL.org
               (list (org-projectile-test-join-paths a-project "COOL.org")))))))

(defun org-projectile-test-get-project-capture-filepath (project-name)
  (with-current-buffer
      (marker-buffer (occ-get-capture-marker
                      (make-instance 'occ-context
                                     :category project-name
                                     :template org-projectile-capture-template
                                     :strategy org-projectile-strategy)))
    (buffer-file-name)))

(ert-deftest test-org-projectile-combine-strategies ()
  (let* ((org-projectile-strategy (make-instance 'org-projectile-combine-strategies))
         (a-project (org-projectile-test-file "a"))
         (b-project (org-projectile-test-file "b"))
         (c-project (org-projectile-test-file "c"))
         (org-projectile-projects-file org-projectile-test-all-projects)
         (projectile-known-projects
          (list a-project b-project c-project)))
    (should (equal-as-sets
             (occ-get-categories org-projectile-strategy)
             '("proj1" "ideas2" "test" "proj4" "proj3" "github-search" "a" "b" "c")))
    (should (string-equal (org-projectile-test-get-project-capture-filepath "c")
                          org-projectile-test-all-projects))
    (should (string-equal (org-projectile-test-get-project-capture-filepath "a")
                          (org-projectile-test-join-paths a-project "TODO.org")))))

(provide 'org-projectile-test)
;;; org-projectile-test.el ends here
