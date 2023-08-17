;;; org-projectile.el --- Repository todo capture and management for org-mode with projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode projectile todo tools outlines project capture
;; URL: https://github.com/colonelpanic8/org-project-capture
;; Version: 3.1.1
;; Package-Requires: ((projectile "2.3.0") (dash "2.10.0") (org-project-capture "3.0.1") (org-category-capture "3.0.1"))

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

;; This package provides an easy interface to creating per project org-mode TODO
;; headings, whether in a single file, or in a file stored in each project
;; directory.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'projectile)
(require 'org-category-capture)
(require 'org-project-capture)
(require 'org-project-capture-backend)


;; Projectile backend

(defclass org-project-capture-projectile-backend (org-project-capture-backend)
  nil)

(cl-defmethod org-project-capture-get-all-project-paths
  ((_ org-project-capture-projectile-backend))
  projectile-known-projects)

(cl-defmethod org-project-capture-project-root-of-filepath
  ((_ org-project-capture-projectile-backend) filepath)
  (let ((dir (file-name-directory filepath)))
    (--some (let* ((cache-key (format "%s-%s" it dir))
                   (cache-value (gethash
                                 cache-key projectile-project-root-cache)))
              (if cache-value
                  cache-value
                (let ((value (funcall it dir)))
                  (puthash cache-key value projectile-project-root-cache)
                  value)))
            projectile-project-root-functions)))

(cl-defmethod org-project-capture-switch-to-project
  ((_ org-project-capture-projectile-backend) directory)
    (when directory
      (projectile-switch-project-by-name directory)))

(cl-defmethod org-project-capture-current-project
  ((_backend org-project-capture-projectile-backend))
  (projectile-project-name))

(defun org-projectile-to-project-capture-setter (symbol value)
  (set-default symbol value)
  (let ((new-var (intern (replace-regexp-in-string
                          "org-projectile"
                          "org-project-capture"
                          (symbol-name symbol)))))
    (when value
      (set new-var value)
      (message "Please use `%s` instead of `%s`." new-var symbol))))


;; Backwards compatibility custom

(defgroup org-projectile ()
  "Customizations for org-projectile."
  :group 'org
  :prefix "org-projectile-")

(defcustom org-projectile-projects-file nil
  "The path to the file in which project TODOs will be stored."
  :type '(string)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-projects-directory nil
  "Directory to store per-project `org-projectile' TODOs.
If non-nil, it would serve as a root directory for storing
project specific TODOs. Otherwise,
`org-projectile-per-project-filepath' would be used to build a
filename related to project root."
  :type '(string)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-per-project-filepath nil
  "The path (relative to the project or `org-projectile-projects-directory')
where todos will be stored. Alternatively you may provide a function that will
compute this path."
  :type '(choice string function)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-capture-template nil
  "The default capture template to use for org-projectile TODOs."
  :type '(string)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-force-linked nil
  "Whether to make project category headings links to their projects."
  :type '(boolean)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-counts-in-heading nil
  "Whether or not to make project category headings display counts."
  :type '(boolean)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-subheading-selection nil
  "Controls whether or not project subheading selection is enabled."
  :type '(boolean)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(defcustom org-projectile-allow-tramp-projects nil
  "Whether to use tramp/sudo requiring projects."
  :type '(boolean)
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)


;; Backwards compatibility variables

(defvar org-projectile-backend
  (make-instance 'org-project-capture-projectile-backend))

(defclass org-projectile-combine-strategies
  (org-project-capture-combine-strategies) nil)

(cl-defmethod org-project-capture-strategy-get-backend
  ((_ org-projectile-combine-strategies))
  org-projectile-backend)

(defclass org-projectile-per-project-strategy
  (org-project-capture-per-project-strategy) nil)

(cl-defmethod org-project-capture-strategy-get-backend
  ((_ org-projectile-per-project-strategy))
  org-projectile-backend)

(defclass org-projectile-single-file-strategy
  (org-project-capture-single-file-strategy) nil)

(cl-defmethod org-project-capture-strategy-get-backend
  ((_ org-projectile-single-file-strategy))
  org-projectile-backend)

(defvar org-projectile-strategy (make-instance 'org-projectile-combine-strategies))

(make-obsolete-variable 'org-projectile-strategy 'org-project-capture-strategy "3.0.1")


;; Functions and autoloads

(cl-defun org-projectile-project-todo-entry
    (&rest additional-options &key (capture-character "p")
           (capture-template org-project-capture-capture-template)
           (capture-heading "Project Todo") &allow-other-keys)
  (let ((target-fn
         (lambda ()
           (occ-capture-edit-at-marker
            (make-instance
             'occ-context
             :category (org-project-capture-category-from-file
                        (org-project-capture-strategy-get-backend
                         org-project-capture-strategy)
                        (or (org-capture-get :original-file)
                            (with-current-buffer (org-capture-get :original-buffer)
                              default-directory)))
             :template capture-template
             :strategy org-project-capture-strategy
             :options additional-options)))))
    `(,capture-character ,capture-heading entry
                         (function
                          ,target-fn)
                         ,capture-template ,@additional-options)))

(defun org-projectile-todo-files ()
  (declare (obsolete org-project-capture-todo-files "3.0.1"))
  (--filter (file-readable-p it) (occ-get-todo-files org-projectile-strategy)))

(defun org-projectile-completing-read (prompt &rest args)
  (apply 'completing-read prompt (occ-get-categories org-projectile-strategy)
         args))

;;;###autoload
(defun org-projectile-goto-location-for-project (project)
  "Goto the location at which TODOs for PROJECT are stored."
  (declare (obsolete org-project-capture-goto-location-for-project "3.0.1"))
  (interactive
   (list
    (org-projectile-completing-read
     "Select which project's TODOs you would like to go to:"
     (occ-get-categories org-projectile-strategy))))
  (occ-capture-goto-marker
   (make-instance 'occ-context
                  :category project
                  :template org-project-capture-capture-template
                  :strategy org-projectile-strategy
                  :options nil)))

;;;###autoload
(defun org-projectile-single-file ()
  "Set `org-projectile-strategy' so that captures occur in a single file."
  (declare (obsolete org-project-capture-single-file "3.0.1"))
  (interactive)
  (setq org-projectile-strategy
        (make-instance 'org-projectile-single-file-strategy)))

;;;###autoload
(defun org-projectile-per-project ()
  "Set `org-projectile-strategy' so that captures occur within each project."
  (declare (obsolete org-project-capture-per-project "3.0.1"))
  (interactive)
  (setq org-projectile-strategy
        (make-instance 'org-projectile-per-project-strategy)))

;;;###autoload
(cl-defun org-projectile-project-todo-completing-read
    (&rest additional-options &key capture-template &allow-other-keys)
  "Select a project using a `completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (declare (obsolete org-project-capture-project-todo-completing-read "3.0.1"))
  (interactive)
  (occ-capture
   (make-instance 'occ-context
                  :category (org-projectile-completing-read
                             "Record TODO for project: ")
                  :template (or capture-template
                                org-project-capture-capture-template)
                  :strategy org-projectile-strategy
                  :options additional-options)))

;;;###autoload
(cl-defun org-projectile-capture-for-current-project
    (&rest additional-options &key capture-template &allow-other-keys)
  "Capture a TODO for the current active project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (declare (obsolete org-project-capture-capture-for-current-project "3.0.1"))
  (interactive)
  (let ((project-name
         (org-project-capture-current-project
          (org-project-capture-strategy-get-backend org-projectile-strategy))))
    (if project-name
        (occ-capture
         (make-instance 'occ-context
                        :category project-name
                        :template (or capture-template
                                      org-project-capture-capture-template)
                        :options additional-options
                        :strategy org-projectile-strategy))
      (error (format "%s is not a recognized project."
                     project-name)))))

(provide 'org-projectile)
;;; org-projectile.el ends here
