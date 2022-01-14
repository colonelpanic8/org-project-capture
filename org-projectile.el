;;; org-projectile.el --- Repository todo management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode projectile todo tools outlines
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 1.1.0
;; Package-Requires: ((projectile "0.11.0") (dash "2.10.0") (emacs "24") (s "1.9.0") (org-category-capture "0.0.0"))

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

;; This package aims to provide an easy interface to creating per
;; project org-mode TODO headings.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'org)
(require 'org-category-capture)
(require 'projectile)
(require 's)

(defgroup org-projectile ()
  "Customizations for org-projectile."
  :group 'org
  :prefix "org-projectile-")

(defcustom org-projectile-projects-file "~/org/projects.org"
  "The path to the file in which projectile TODOs will be stored."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-projects-directory nil
  "Directory to store per-project `org-projectile' TODOs. If non-nil, it
would serve as a root directory for storing project specific TODOs.
Otherwise, `org-projectile-per-project-filepath' would be used to build a
filename related to project root."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-per-project-filepath "TODO.org"
  "The path (relative to the project or `org-projectile-projects-directory')
where todos will be stored. Alternatively you may provide a function that will
compute this path."
  :type '(choice string function)
  :group 'org-projectile)

(defcustom org-projectile-capture-template "* TODO %?\n"
  "The default capture template to use for org-projectile TODOs."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-force-linked t
  "Whether to make project category headings links to their projects."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile-counts-in-heading t
  "Whether or not to make projectile category headings display counts."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile-subheading-selection t
  "Controls whether or not project subheading selection is enabled."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile-allow-tramp-projects nil
  "Whether to use tramp/sudo requiring projects."
  :type '(boolean)
  :group 'org-projectile)


;; Utility functions

(defvar org-projectile-strategy nil)

(defun org-projectile-io-action-permitted (filepath)
  (or org-projectile-allow-tramp-projects
      (eq nil (find-file-name-handler filepath 'file-truename))))

(defun org-projectile-project-root-of-filepath (filepath)
  (when (org-projectile-io-action-permitted filepath)
    (let ((dir (file-name-directory filepath)))
      (--some (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (gethash
                                   cache-key projectile-project-root-cache)))
                (if cache-value
                    cache-value
                  (let ((value (funcall it dir)))
                    (puthash cache-key value projectile-project-root-cache)
                    value)))
              projectile-project-root-files-functions))))

(defun org-projectile-category-from-project-root (project-root)
  (file-name-nondirectory (directory-file-name project-root)))

(defun org-projectile-category-from-file (filename)
  (let ((project-root (org-projectile-project-root-of-filepath filename)))
    (when project-root
      (org-projectile-category-from-project-root project-root))))

(defun org-projectile-open-project (name)
  (let* ((name-to-location
          (org-projectile-category-to-project-path org-projectile-strategy))
         (entry (assoc name name-to-location)))
    (when entry
      (projectile-switch-project-by-name (cdr entry)))))

(defun org-projectile-default-project-categories ()
  (mapcar (lambda (path)
            (cons (org-projectile-category-from-project-root
                   path) path)) projectile-known-projects))

(defun org-projectile-invert-alist (alist)
  (mapcar (lambda (entry)
            (cons (cdr entry) (car entry))) alist))

(defun org-projectile-get-category-from-heading ()
  (let* ((heading (org-get-heading))
         (no-links
          (replace-regexp-in-string
           org-bracket-link-analytic-regexp
           (lambda (m)
             (if (match-end 5) (match-string 5 m)
               (concat (match-string 1 m) (match-string 3 m))))
           heading nil t)))
    (s-trim (replace-regexp-in-string "\[[0-9]*/[0-9]*\]" "" no-links))))


;; One file per project strategy

(defun org-projectile-get-project-todo-file (project-path)
  (let ((project-todos-filepath
         (if (stringp org-projectile-per-project-filepath)
             org-projectile-per-project-filepath
           (funcall org-projectile-per-project-filepath project-path)))
        (org-projectile-directory
         (if org-projectile-projects-directory
             org-projectile-projects-directory
           (file-name-as-directory project-path))))
    (concat org-projectile-directory project-todos-filepath)))

(defun org-projectile-get-category-from-project-todo-file (project-path)
  (let ((todo-filepath (org-projectile-get-project-todo-file project-path)))
    (if (and (org-projectile-io-action-permitted todo-filepath)
             (file-exists-p todo-filepath))
        (with-current-buffer (find-file-noselect todo-filepath)
          (org-refresh-category-properties)
          org-category)
      (org-projectile-category-from-project-root project-path))))

(defun org-projectile-get-project-file-categories ()
  (mapcar 'org-projectile-category-from-project-root
          projectile-known-projects))

(defclass org-projectile-per-project-strategy nil nil)

(cl-defmethod occ-get-categories ((_ org-projectile-per-project-strategy))
  (org-projectile-get-project-file-categories))

(cl-defmethod occ-get-todo-files ((_ org-projectile-per-project-strategy))
  (mapcar 'org-projectile-get-project-todo-file projectile-known-projects))

(cl-defmethod occ-get-capture-file
    ((s org-projectile-per-project-strategy) category)
  (let ((project-root
         (cdr (assoc category
                     (org-projectile-category-to-project-path s)))))
    (org-projectile-get-project-todo-file project-root)))

(cl-defmethod occ-get-capture-marker
    ((strategy org-projectile-per-project-strategy) context)
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy category)))
      (with-current-buffer (find-file-noselect filepath)
        (point-max-marker)))))

(cl-defmethod occ-target-entry-p
    ((_ org-projectile-per-project-strategy) _context)
  nil)

(cl-defmethod org-projectile-category-to-project-path
    ((_ org-projectile-per-project-strategy))
  (mapcar (lambda (path)
            (cons (org-projectile-category-from-project-root
                   path) path)) projectile-known-projects))


;; Single file strategy

(defun org-projectile-get-categories-from-project-paths ()
  (mapcar 'org-projectile-category-from-project-root projectile-known-projects))

(defun org-projectile-linked-heading (heading)
  (org-make-link-string
   (format "elisp:(org-projectile-open-project \"%s\")" heading) heading))

(defun org-projectile-build-heading (heading)
  (when org-projectile-force-linked
    (setq heading (org-projectile-linked-heading heading)))
  (if org-projectile-counts-in-heading (concat heading " [/]")
    heading))

(defclass org-projectile-top-level-categories-specifier nil nil)

(cl-defmethod org-projectile-get-existing-categories ())

(defclass org-projectile-single-file-strategy nil nil)

(cl-defmethod org-projectile-category-to-project-path
    ((_s org-projectile-single-file-strategy))
  (org-projectile-default-project-categories))

(cl-defmethod occ-get-categories
    ((_s org-projectile-single-file-strategy))
  (cl-remove-if
   'null
   (delete-dups
    (nconc
     (org-projectile-get-categories-from-project-paths)
     (occ-get-categories-from-filepath org-projectile-projects-file)))))

(cl-defmethod occ-get-categories ((_s org-projectile-single-file-strategy))
  (cl-remove-if
   'null
   (delete-dups
    (nconc
     (org-projectile-get-categories-from-project-paths)
     (occ-get-categories-from-filepath
      org-projectile-projects-file
      :get-category-from-element 'org-projectile-get-category-from-heading)))))

(cl-defmethod occ-get-todo-files ((_ org-projectile-single-file-strategy))
  (list org-projectile-projects-file))

(cl-defmethod occ-get-capture-file ((_ org-projectile-single-file-strategy) _c)
  org-projectile-projects-file)

(cl-defmethod occ-get-capture-marker
    ((strategy org-projectile-single-file-strategy) context)
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy category)))
      (with-current-buffer (find-file-noselect filepath)
        (save-excursion
          (occ-goto-or-insert-category-heading
           category
           :build-heading 'org-projectile-build-heading
           :get-category-from-element 'org-projectile-get-category-from-heading)
          (point-marker))))))

(defun org-projectile-linked-heading-regexp (heading)
  (format "\\[\\[.*?]\\[%s]]" heading))

(cl-defmethod occ-target-entry-p ((_ org-projectile-single-file-strategy) _c)
  t)

(cl-defmethod org-projectile-category-to-project-path
    ((_ org-projectile-single-file-strategy))
  (mapcar (lambda (path)
            (cons (org-projectile-category-from-project-root
                   path) path)) projectile-known-projects))



(setq org-projectile-strategy
      (make-instance 'org-projectile-single-file-strategy))

(defun org-projectile-location-for-project (project)
  (cdr (assoc project
              (org-projectile-category-to-project-path
               org-projectile-strategy))))

(cl-defun org-projectile-project-todo-entry
    (&rest additional-options &key (capture-character "p")
           (capture-template org-projectile-capture-template)
           (capture-heading "Project Todo") &allow-other-keys)
  (let ((target-fn
         (lambda ()
           (occ-capture-goto-marker
            (make-instance 'occ-context
                           :category (org-projectile-category-from-file
                                      (org-capture-get :original-file))
                           :template capture-template
                           :strategy org-projectile-strategy
                           :options additional-options)))))
    `(,capture-character ,capture-heading entry
                         (function
                          ,target-fn)
                         ,capture-template ,@additional-options)))

(defun org-projectile-get-marker-for-category (category)
  (occ-get-capture-marker
   org-projectile-strategy
   (make-instance 'occ-context
                  :category category
                  :options nil
                  :strategy org-projectile-strategy
                  :template org-projectile-capture-template)))

(defun org-projectile-todo-files ()
  (occ-get-todo-files org-projectile-strategy))

;;;###autoload
(defun org-projectile-goto-location-for-project (project)
  "Goto the location at which TODOs for PROJECT are stored."
  (interactive
   (list
    (projectile-completing-read
     "Select which project's TODOs you would like to go to:"
     (occ-get-categories org-projectile-strategy))))
  (occ-capture-goto-marker
   (make-instance 'occ-context
                  :category project
                  :template org-projectile-capture-template
                  :strategy org-projectile-strategy
                  :options nil)))

;;;###autoload
(defun org-projectile-single-file ()
  "Set `org-projectile-strategy' so that captures occur in a single file."
  (interactive)
  (setq org-projectile-strategy
        (make-instance 'org-projectile-single-file-strategy)))

;;;###autoload
(defun org-projectile-per-project ()
  "Set `org-projectile-strategy' so that captures occur within each project."
  (interactive)
  (setq org-projectile-strategy
        (make-instance 'org-projectile-per-project-strategy)))

;;;###autoload
(cl-defun org-projectile-project-todo-completing-read
    (&rest additional-options &key capture-template &allow-other-keys)
  "Select a project using a `projectile-completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (interactive)
  (occ-capture
   (make-instance 'occ-context
                  :category (projectile-completing-read
                             "Record TODO for project: "
                             (occ-get-categories org-projectile-strategy))
                  :template (or capture-template
                                org-projectile-capture-template)
                  :strategy org-projectile-strategy
                  :options additional-options)))

;;;###autoload
(cl-defun org-projectile-capture-for-current-project
    (&rest additional-options &key capture-template &allow-other-keys)
  "Capture a TODO for the current active projectile project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (interactive)
  (let ((project-name (projectile-project-name)))
    (if (projectile-project-p)
        (occ-capture
         (make-instance 'occ-context
                        :category project-name
                        :template (or capture-template
                                      org-projectile-capture-template)
                        :options additional-options
                        :strategy org-projectile-strategy))
      (error (format "%s is not a recognized projectile project."
                     project-name)))))

(provide 'org-projectile)
;;; org-projectile.el ends here
