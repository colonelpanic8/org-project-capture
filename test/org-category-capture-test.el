;;; org-category-capture-test.el --- org-category-capture test suite -*- lexical-binding: t; -*-

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
(require 'dash)

(require 'org-category-capture)
(setq org-adapt-indentation 1)

(defun equal-as-sets (seq1 seq2)
  (and
   (-all? (lambda (element) (member element seq2)) seq1)
   (-all? (lambda (element) (member element seq1)) seq2)))

(ert-deftest test-occ-get-value-by-category-handles-missing-categories ()
  (with-temp-buffer
    (org-mode)
    (insert "
* proj2
** TODO do my thing
** TODO cool
* proj1
* proj3
* emacs
  :PROPERTIES:
  :CATEGORY: proj4
  :END:
") (should (equal-as-sets (mapcar 'car (occ-get-value-by-category))
                          '("proj1" "proj2" "proj3" "proj4")))))

(defclass occ-test-strategy (occ-strategy) nil)

(cl-defmethod occ-get-capture-marker ((_ occ-test-strategy) context)
  (with-slots (category) context
    (occ-goto-or-insert-category-heading category)
    (point-marker)))

(cl-defmethod occ-target-entry-p ((_ occ-test-strategy) context) t)

(defvar occ-test-text-to-insert "dummy-text")

(defun occ-do-test-capture (category heading-text)
  (let ((occ-test-text-to-insert heading-text))
    (occ-capture
     (make-instance 'occ-context :category category :options nil :strategy
                    (make-instance 'occ-test-strategy) :template "* TODO %?\n"))))

(defun occ-mock-place-template (&rest args)
  (goto-char (org-capture-get :pos))
  (setq-local outline-level 'org-outline-level)
  (pcase (org-capture-get :type)
    ((or `nil `entry) (org-capture-place-entry))
    (`table-line (org-capture-place-table-line))
    (`plain (org-capture-place-plain-text))
    (`item (org-capture-place-item))
    (`checkitem (org-capture-place-item)))
  (insert occ-test-text-to-insert))

(ert-deftest test-get-heading-location-on-empty-file ()
  (with-temp-buffer
    (org-mode)
    (occ-get-category-heading-location "test-category")))

(ert-deftest test-insert-todo ()
  (with-temp-buffer
    (org-mode)
    (insert "
* cool
* someproj
* anotherproj
  :PROPERTIES:
  :CATEGORY: actualcategory
  :END:
* something
") (noflet ((org-capture-place-template (&rest args)
                                        (occ-mock-place-template))
            (org-capture-narrow (&rest args)
                                nil))
     (occ-do-test-capture "someproj" "dummy text")
     (occ-do-test-capture "anotherproj" "dummy text")
     (occ-do-test-capture "actualcategory" "some text")
     (occ-do-test-capture "someproj" "dummy text")
     (should
      (equal (buffer-string) "
* cool
* someproj
** TODO dummy text
** TODO dummy text
* anotherproj
  :PROPERTIES:
  :CATEGORY: actualcategory
  :END:
** TODO some text
* something
* anotherproj
  :PROPERTIES:
  :CATEGORY: anotherproj
  :END:
** TODO dummy text
")))))

(provide 'org-category-capture-test)
;;; org-capture-test.el ends here
