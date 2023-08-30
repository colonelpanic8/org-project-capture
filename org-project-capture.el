;;; org-project-capture.el --- Repository todo capture and management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode todo tools outlines project capture
;; URL: https://github.com/colonelpanic8/org-project-capture
;; Version: 3.1.1
;; Package-Requires: ((dash "2.10.0") (emacs "28") (s "1.9.0") (org-category-capture "1.0.0"))

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
(require 'org)
(require 'org-category-capture)
(require 'org-project-capture-backend)
(require 's)

(defgroup org-project-capture ()
  "Customizations for org-project-capture."
  :group 'org
  :prefix "org-project-capture-")

(defcustom org-project-capture-projects-file "~/org/projects.org"
  "The path to the file in which project TODOs will be stored."
  :type '(string)
  :group 'org-project-capture)

(defcustom org-project-capture-projects-directory nil
  "Directory to store per-project `org-project-capture' TODOs.
If non-nil, it would serve as a root directory for storing
project specific TODOs. Otherwise,
`org-project-capture-per-project-filepath' would be used to build a
filename related to project root."
  :type '(string)
  :group 'org-project-capture)

(defcustom org-project-capture-per-project-filepath "TODO.org"
  "The path (relative to the project or `org-project-capture-projects-directory')
where todos will be stored. Alternatively you may provide a function that will
compute this path."
  :type '(choice string function)
  :group 'org-project-capture)

(defcustom org-project-capture-capture-template "* TODO %?\n"
  "The default capture template to use for org-project-capture TODOs."
  :type '(string)
  :group 'org-project-capture)

(defcustom org-project-capture-force-linked t
  "Whether to make project category headings links to their projects."
  :type '(boolean)
  :group 'org-project-capture)

(defcustom org-project-capture-counts-in-heading t
  "Whether or not to make project category headings display counts."
  :type '(boolean)
  :group 'org-project-capture)

(defcustom org-project-capture-subheading-selection t
  "Controls whether or not project subheading selection is enabled."
  :type '(boolean)
  :group 'org-project-capture)

(defcustom org-project-capture-allow-tramp-projects nil
  "Whether to use tramp/sudo requiring projects."
  :type '(boolean)
  :group 'org-project-capture)

(defcustom org-project-capture-add-category-to-new-files t
  "Whether or not to automatically add a category property to newly created files."
  :type '(boolean)
  :group 'org-project-capture)

(defvar org-project-capture-strategy nil)

(defvar org-project-capture-default-backend
  (make-instance 'org-project-capture-project-backend))


;; Utility functions

(defun org-project-capture-io-action-permitted (filepath)
  (or org-project-capture-allow-tramp-projects
      (eq nil (find-file-name-handler filepath 'file-truename))))

(defun org-project-capture-open-project (name)
  (let* ((name-to-location
          (org-project-capture-build-category-to-project-path
           org-project-capture-strategy))
         (entry (assoc name name-to-location)))
    (when entry
      (org-project-capture-switch-to-project
       (org-project-capture-strategy-get-backend
        org-project-capture-strategy)
       (cdr entry)))))

(defun org-project-capture-invert-alist (alist)
  (mapcar (lambda (entry)
            (cons (cdr entry) (car entry))) alist))

(defun org-project-capture-get-category-from-heading ()
  (let* ((heading (org-get-heading))
         (no-links
          (replace-regexp-in-string
           org-link-bracket-re
           (lambda (m)
             (if (match-end 2) (match-string 2 m)
               (concat (match-string 2 m)))) heading nil t)))
    (s-trim (replace-regexp-in-string "\[[0-9]*/[0-9]*\]" "" no-links))))


;; Base

(defclass org-project-capture-base-strategy (occ-strategy)
  ((backend :initarg :backend :initform nil)) :abstract t)

(cl-defmethod org-project-capture-strategy-get-backend
  ((strategy org-project-capture-base-strategy))
  (or (oref strategy backend) org-project-capture-default-backend))

(cl-defmethod org-project-capture-build-category-to-project-path
  ((strategy org-project-capture-base-strategy))
  (org-project-capture-build-category-to-project-path
   (org-project-capture-strategy-get-backend strategy)))


;; One file per project strategy

(defun org-project-capture-get-project-todo-file (project-path)
  (let ((project-todos-filepath
         (if (stringp org-project-capture-per-project-filepath)
             org-project-capture-per-project-filepath
           (funcall org-project-capture-per-project-filepath project-path)))
        (org-project-capture-directory
         (if org-project-capture-projects-directory
             org-project-capture-projects-directory
           (file-name-as-directory project-path))))
    (concat org-project-capture-directory project-todos-filepath)))

(defun org-project-capture-get-category-from-project-todo-file (project-path)
  (let ((todo-filepath (org-project-capture-get-project-todo-file project-path)))
    (if (and (org-project-capture-io-action-permitted todo-filepath)
             (file-exists-p todo-filepath))
        (with-current-buffer (find-file-noselect todo-filepath)
          (org-refresh-category-properties)
          org-category)
      (org-project-capture-category-from-project-root project-path))))

(defclass org-project-capture-per-project-strategy
  (org-project-capture-base-strategy) nil)

(cl-defmethod occ-get-categories
  ((strategy org-project-capture-per-project-strategy))
  (org-project-capture-get-all-categories
   (org-project-capture-strategy-get-backend strategy)))

(cl-defmethod occ-get-existing-categories
  ((strategy org-project-capture-per-project-strategy))
  (cl-loop for category in (occ-get-categories strategy)
           when (file-exists-p (occ-get-capture-file strategy category))
           collect category))

(cl-defmethod occ-get-todo-files
  ((strategy org-project-capture-per-project-strategy))
  (->> (org-project-capture-get-all-project-paths
        (org-project-capture-strategy-get-backend strategy))
       (cl-mapcar 'org-project-capture-get-project-todo-file)))

(cl-defmethod occ-get-capture-file
  ((s org-project-capture-per-project-strategy) category)
  (let ((project-root
         (cdr (assoc category
                     (org-project-capture-build-category-to-project-path s)))))
    (org-project-capture-get-project-todo-file project-root)))

(cl-defmethod occ-get-capture-marker
  ((strategy org-project-capture-per-project-strategy) context)
  (with-slots (category) context
    (let* ((filepath (occ-get-capture-file strategy category))
           (file-existed (file-exists-p filepath)))
      (with-current-buffer (find-file-noselect filepath)
        (when (and org-project-capture-add-category-to-new-files (not file-existed))
          (org-set-property "CATEGORY" category))
        (point-max-marker)))))

(cl-defmethod occ-target-entry-p
  ((_ org-project-capture-per-project-strategy) _context)
  nil)


;; Single file strategy

(defun org-project-capture-linked-heading (heading)
  (org-link-make-string
   (format "elisp:(org-project-capture-open-project \"%s\")" heading) heading))

(defun org-project-capture-build-heading (heading)
  (when org-project-capture-force-linked
    (setq heading (org-project-capture-linked-heading heading)))
  (if org-project-capture-counts-in-heading (concat heading " [/]")
    heading))

(defclass org-project-capture-single-file-strategy
  (org-project-capture-base-strategy)
  ((targeter :initarg targeter :initform nil)))

(cl-defmethod occ-get-categories
  ((strategy org-project-capture-single-file-strategy))
  (cl-remove-if
   'null
   (delete-dups
    (nconc
     (org-project-capture-get-all-categories
      (org-project-capture-strategy-get-backend strategy))
     (occ-get-existing-categories strategy)))))

(cl-defmethod occ-get-existing-categories
  ((_ org-project-capture-single-file-strategy))
  (occ-get-categories-from-filepath
   org-project-capture-projects-file
   :get-category-from-element 'org-project-capture-get-category-from-heading))

(cl-defmethod occ-get-todo-files ((_ org-project-capture-single-file-strategy))
  (list org-project-capture-projects-file))

(cl-defmethod occ-get-capture-file
  ((_ org-project-capture-single-file-strategy) _c)
  org-project-capture-projects-file)

(cl-defmethod occ-get-capture-marker
  ((strategy org-project-capture-single-file-strategy) context)
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy category)))
      (with-current-buffer (find-file-noselect filepath)
        (save-excursion
          (org-project-capture-target-projects-heading strategy context)
          (occ-goto-or-insert-category-heading
           category
           :build-heading 'org-project-capture-build-heading
           :get-category-from-element
           'org-project-capture-get-category-from-heading)
          (org-project-capture-target-within-project strategy context)
          (point-marker))))))

(cl-defmethod org-project-capture-target-projects-heading
  ((strategy org-project-capture-single-file-strategy) context)
  (with-slots (targeter) strategy
    (when targeter
      (org-project-capture-target-projects-heading targeter context))))

(cl-defmethod org-project-capture-target-within-project
  ((strategy org-project-capture-single-file-strategy) context)
  (with-slots (targeter) strategy
    (when targeter
      (org-project-capture-target-within-project targeter context))))

(defun org-project-capture-linked-heading-regexp (heading)
  (format "\\[\\[.*?]\\[%s]]" heading))

(cl-defmethod occ-target-entry-p
  ((_ org-project-capture-single-file-strategy) _c) t)


;; Combine strategies

(defclass org-project-capture-combine-strategies
  (org-project-capture-base-strategy)
  ((strategies :initarg :strategies)))

(cl-defmethod initialize-instance
  :after ((obj org-project-capture-combine-strategies) &rest _args)
  (unless (slot-boundp obj 'strategies)
    (setf (slot-value obj 'strategies)
          (list (make-instance 'org-project-capture-per-project-strategy)
                (make-instance 'org-project-capture-single-file-strategy)))))

(cl-defmethod occ-get-categories
  ((strategy org-project-capture-combine-strategies))
  (delete-dups (mapcan #'occ-get-categories (oref strategy strategies))))

(cl-defmethod occ-get-existing-categories
  ((strategy org-project-capture-combine-strategies))
  (delete-dups
   (mapcan #'occ-get-existing-categories (oref strategy strategies))))

(cl-defmethod occ-get-todo-files
  ((strategy org-project-capture-combine-strategies))
  (mapcan #'occ-get-todo-files (oref strategy strategies)))

(cl-defmethod occ-get-capture-marker
  ((strategy org-project-capture-combine-strategies) context)
  (occ-get-capture-marker
   (org-project-capture-select-strategy-from-context strategy context) context))

(cl-defmethod occ-target-entry-p
  ((strategy org-project-capture-combine-strategies) context)
  (occ-target-entry-p
   (org-project-capture-select-strategy-from-context strategy context) context))

(cl-defmethod org-project-capture-select-strategy
  ((strategy org-project-capture-combine-strategies) project-name)
  (cl-loop for substrategy in (oref strategy strategies)
           if (--some (string-equal it project-name)
                      (occ-get-existing-categories substrategy))
           return substrategy
           finally return (car (oref strategy strategies))))

(cl-defmethod org-project-capture-select-strategy-from-context
  ((strategy org-project-capture-combine-strategies) context)
  (org-project-capture-select-strategy strategy (oref context category)))

(setq org-project-capture-strategy
      (make-instance 'org-project-capture-combine-strategies))



(setq org-project-capture-strategy
      (make-instance 'org-project-capture-combine-strategies))

(defun org-project-capture-location-for-project (project)
  (cdr (assoc project
              (org-project-capture-build-category-to-project-path
               org-project-capture-strategy))))

(cl-defun org-project-capture-project-todo-entry
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

(defun org-project-capture-get-marker-for-category (category)
  (occ-get-capture-marker
   org-project-capture-strategy
   (make-instance 'occ-context
                  :category category
                  :options nil
                  :strategy org-project-capture-strategy
                  :template org-project-capture-capture-template)))

(defun org-project-capture-todo-files ()
  (--filter (file-readable-p it)
            (occ-get-todo-files org-project-capture-strategy)))

(defun org-project-capture-completing-read (prompt &rest args)
  (apply 'completing-read prompt (occ-get-categories org-project-capture-strategy)
         args))

;;;###autoload
(defun org-project-capture-goto-location-for-project (project)
  "Goto the location at which TODOs for PROJECT are stored."
  (interactive
   (list
    (org-project-capture-completing-read
     "Select which project's TODOs you would like to go to:")))
  (occ-capture-goto-marker
   (make-instance 'occ-context
                  :category project
                  :template org-project-capture-capture-template
                  :strategy org-project-capture-strategy
                  :options nil)))

;;;###autoload
(defun org-project-capture-single-file ()
  "Set `org-project-capture-strategy' so that captures occur in a single file."
  (interactive)
  (setq org-project-capture-strategy
        (make-instance 'org-project-capture-single-file-strategy)))

;;;###autoload
(defun org-project-capture-per-project ()
  "Set `org-project-capture-strategy' so that captures occur within each project."
  (interactive)
  (setq org-project-capture-strategy
        (make-instance 'org-project-capture-per-project-strategy)))

;;;###autoload
(cl-defun org-project-capture-project-todo-completing-read
    (&rest additional-options &key capture-template &allow-other-keys)
  "Select a project using a `completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (interactive)
  (occ-capture
   (make-instance 'occ-context
                  :category (org-project-capture-completing-read
                             "Record TODO for project: ")
                  :template (or capture-template
                                org-project-capture-capture-template)
                  :strategy org-project-capture-strategy
                  :options additional-options)))

;;;###autoload
(cl-defun org-project-capture-capture-for-current-project
    (&rest additional-options &key capture-template &allow-other-keys)
  "Capture a TODO for the current active project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (interactive)
  (let ((project-name
         (org-project-capture-current-project
          (org-project-capture-strategy-get-backend org-project-capture-strategy))))
    (if project-name
        (occ-capture
         (make-instance 'occ-context
                        :category project-name
                        :template (or capture-template
                                      org-project-capture-capture-template)
                        :options additional-options
                        :strategy org-project-capture-strategy))
      (error (format "%s is not a recognized project."
                     project-name)))))

(provide 'org-project-capture)
;;; org-project-capture.el ends here
