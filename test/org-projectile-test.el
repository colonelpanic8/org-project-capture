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

(ert-deftest test-build-capture-template-compiles ()
  (occ-build-capture-template
   (make-instance 'occ-context
                  :category "the-category"
                  :template org-projectile:capture-template
                  :options nil
                  :strategy org-projectile:capture-strategy)))

(ert-deftest test-project-todo-completing-read ()
  (let ((only-project "only-project")
        place-template-called)
    (org-projectile:per-repo)
    (noflet ((projectile-completing-read (prompt project-names)
                                       (car project-names))
             (org-projectile:known-projects (&rest args) (list only-project))
             (org-capture-place-template (&rest args)
                                         (setq place-template-called t))
             (org-projectile:project-heading (heading) heading))
      (org-projectile:project-todo-completing-read)
      (should place-template-called))))

(provide 'org-projectile-test)
;;; org-projectile-test.el ends here
