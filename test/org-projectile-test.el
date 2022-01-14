;;; org-projectile-test.el --- org-projectile test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; (ert-deftest test-org-projectile-supports-various-heading-types ()
;;   (setq org-projectile-strategy
;;         (make-instance 'org-projectile-single-file-strategy)
;;         org-projectile-projects-file
;;         (concat (file-name-as-directory (expand-file-name "test/")) "test_projects.org"))
;;   (let ((projectile-known-projects nil))
;;     (should (equal-as-sets (occ-get-categories org-projectile-strategy)
;;                            '("proj1" "ideas2" "test" "proj4" "proj3"
;;                              "github-search")))))

(ert-deftest org-projectile-per-project-filepath-with-function ()
  (let ((org-projectile-strategy (make-instance 'org-projectile-per-project-strategy))
        (projectile-known-projects '("/a" "/b"))
        (org-projectile-per-project-filepath
         (lambda (path)
           (if (string-equal path "/a")
               "OTHER.org"
             "TODO.org"))))
    (should (equal-as-sets (org-projectile-todo-files)
                           '("/a/OTHER.org" "/b/TODO.org")))
    (let ((org-projectile-per-project-filepath "COOL.org"))
      (should (equal-as-sets (org-projectile-todo-files)
                           '("/a/COOL.org" "/b/COOL.org"))))))

(provide 'org-projectile-test)
;;; org-projectile-test.el ends here
