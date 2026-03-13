;;; org-project-capture-backend.el --- Repository todo capture and management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ivan Malison

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

;; Version: 3.1.1

;;; Commentary:

;; An interface for defining backends for org-project-capture.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'project)

(defun org-project-capture-category-from-project-root (project-root)
  "Get the category name from PROJECT-ROOT directory path."
  (when project-root
    (file-name-nondirectory (directory-file-name project-root))))

(defclass org-project-capture-backend nil nil :abstract t)

(cl-defmethod org-project-capture-get-all-project-paths
  ((_backend org-project-capture-backend))
  "Return a list of all known project paths.")

(cl-defmethod org-project-capture-switch-to-project
  ((_backend org-project-capture-backend) _directory)
  "Switch to the project at DIRECTORY.")

(cl-defmethod org-project-capture-project-root-of-filepath
  ((_backend org-project-capture-backend) _filepath)
  "Return the project root containing FILEPATH.")

(cl-defmethod org-project-capture-current-project
  ((_backend org-project-capture-backend))
  "Return the current project name.")

(cl-defmethod org-project-capture-build-category-to-project-path
  ((backend org-project-capture-backend))
  "Build an alist mapping category names to project paths for BACKEND."
  (mapcar
   (lambda (path)
     (cons (org-project-capture-category-from-project-root path) path))
   (org-project-capture-get-all-project-paths backend)))

(cl-defmethod org-project-capture-category-from-file
  ((backend org-project-capture-backend) filepath)
  "Get the category for the project containing FILEPATH using BACKEND."
  (org-project-capture-category-from-project-root
   (org-project-capture-project-root-of-filepath backend filepath)))

(cl-defmethod org-project-capture-get-all-categories
  ((backend org-project-capture-backend))
  "Return a list of all category names for BACKEND."
  (mapcar 'org-project-capture-category-from-project-root
          (org-project-capture-get-all-project-paths backend)))


;; project.el backend

(defclass org-project-capture-project-backend (org-project-capture-backend) nil)

(cl-defmethod org-project-capture-get-all-project-paths
  ((_ org-project-capture-project-backend))
  "Return all known project paths using `project-known-project-roots'."
  (project-known-project-roots))

(cl-defmethod org-project-capture-project-root-of-filepath
  ((_ org-project-capture-project-backend) filepath)
  "Return the project root for FILEPATH using `project-current'."
  (project-root (project-current nil filepath)))

(cl-defmethod org-project-capture-switch-to-project
  ((_ org-project-capture-project-backend) directory)
  "Switch to the project at DIRECTORY using `project-switch-project'."
  (when directory
    (project-switch-project directory)))

(cl-defmethod org-project-capture-current-project
  ((_backend org-project-capture-project-backend))
  "Return the current project using `project-current'."
  (project-name (project-current)))

(provide 'org-project-capture-backend)
;;; org-project-capture-backend.el ends here
