;;; org-projectile.el --- Repository todo management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode projectile todo tools
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.2.6
;; Package-Requires: ((projectile "0.11.0") (dash "2.10.0") (emacs "24"))

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
(require 'eieio)
(require 'org-category-capture)
(require 'projectile)
(require 'dash)

(defgroup org-projectile ()
  "Customizations for org-projectile."
  :group 'org
  :prefix "org-projectile-")

(defcustom org-projectile-projects-file "~/org/projects.org"
  "The path to the file in which projectile TODOs will be stored."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-per-project-filepath "TODO.org"
  "The path (relative to the project) where todos will be stored."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-capture-template "* TODO %?\n"
  "The default capture template to use for org-projectile TODOs."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile-linked-capture-template "* TODO %? %A\n"
  "The default linked capture template to use for org-projectile TODOs."
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
                  (let ((value (funcall it (org-projectile-file-truename dir))))
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
  (let* ((name-to-location (org-projectile-project-name-to-location-alist))
         (entry (assoc name name-to-location)))
    (when entry
      (projectile-switch-project-by-name (cdr entry)))))

(defun org-projectile-default-project-categories ()
    (mapcar (lambda (path)
            (cons (org-projectile-category-from-project-root
                   path) path)) projectile-known-projects))


;; One file per project strategy

(defun org-projectile-get-project-todo-file (project-path)
  (concat
   (file-name-as-directory project-path) org-projectile-per-project-filepath))

(defun org-projectile-get-category-from-project-todo-file (project-path)
  (let ((todo-filepath (org-projectile-get-project-todo-file project-path)))
    (if (and (org-projectile-io-action-permitted todo-filepath)
             (file-exists-p todo-filepath))
        (with-current-buffer (find-file-noselect todo-filepath)
          (org-refresh-category-properties)
          org-category)
      (org-projectile-category-from-project-root project-path))))

(defun org-projectile-get-project-file-categories ()
  (mapcar 'org-projectile-get-category-from-project-todo-file
          projectile-known-projects))

(defclass org-projectile-per-project-strategy nil nil)

(defmethod occ-get-categories ((_ org-projectile-per-project-strategy))
  (org-projectile-get-project-file-categories))

(defmethod occ-get-todo-files ((_ org-projectile-per-project-strategy))
  (mapcar 'org-projectile-get-project-todo-file projectile-known-projects))

(defmethod occ-get-capture-file ((s org-projectile-per-project-strategy) category)
  (let ((project-root
         (cdr (assoc category
                (org-projectile-category-to-project-path s)))))
    (org-projectile-get-project-todo-file project-root)))

(defmethod occ-get-capture-marker
  ((strategy org-projectile-per-project-strategy) context)
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy category)))
      (save-excursion (find-file-noselect filepath)
                      (point-max-marker)))))

(defmethod occ-target-entry-p ((_ org-projectile-per-project-strategy) _context)
  nil)




(defun org-projectile-project-heading-from-file (filename)
  (let ((project-root (org-projectile-project-root-of-filepath filename)))
    (when project-root
      (file-name-nondirectory
       (directory-file-name project-root)))))

(defun org-projectile-get-link-description (heading)
  (with-temp-buffer
    (insert heading)
    (goto-char (point-min))
    (if (re-search-forward org-any-link-re nil t)
        (match-string-no-properties 4) heading)))

  (cl-remove-if
   'null
   (delete-dups
    (nconc

(defun org-projectile-open-project (name)
  (let* ((name-to-location (org-projectile-project-name-to-location-alist))
         (entry (assoc name name-to-location)))
    (when entry
      (projectile-switch-project-by-name (cdr entry)))))


(defun org-projectile-linked-heading (heading)
  (org-make-link-string
   (format "elisp:(org-projectile-open-project \"%s\")" heading) heading))

(defun org-projectile-project-heading (heading)
  (let ((heading-text (org-projectile-insert-or-goto-heading heading)))
    (outline-hide-subtree)
    (org-beginning-of-line)
    (org-set-property "CATEGORY" heading)
    heading-text))

(defun org-projectile-end-of-properties ()
  (let ((end-of-heading (save-excursion (outline-next-heading) (point)))
        (last-match t))
    (while last-match (setq last-match
                            (re-search-forward ":END:" end-of-heading t)))
    (point)))

(defun org-projectile-prompt-for-subheadings (&optional recursive)
  (let ((subheadings-to-point (org-projectile-get-subheadings))
        (point-at-start (save-excursion (org-back-to-heading) (point))))
    (when (> (length subheadings-to-point) 1)
      (org-projectile-prompt-for-and-move-to-subheading subheadings-to-point)
      (when recursive
        (unless (eq point-at-start (save-excursion (org-back-to-heading) (point)))
          (org-projectile-prompt-for-subheadings))))))

(defun org-projectile-get-subheadings (&optional scope)
  (unless scope (setq scope 'tree))
  (org-show-subtree)
  (save-excursion
    (org-map-entries (lambda () `(,(org-get-heading) . ,(point))) nil scope
                     (lambda () (when (and (nth 2 (org-heading-components))
                                           (not (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
                                  (org-end-of-subtree))))))
(cl-defun org-projectile-project-todo-entry
    (&rest additional-options &key (capture-character "p")
           (capture-template org-projectile-capture-template)
           (capture-heading "Project Todo"))
  `(,capture-character ,capture-heading entry
                       (function
                        ,(lambda () (occ-capture-goto-marker
                                    (make-instance 'occ-context
                                                   :category (org-projectile-category-from-file
                                                              (org-capture-get :original-file))
                                                   :template capture-template
                                                   :strategy org-projectile-strategy
                                                   :options additional-options))))
    ,capture-template ,@additional-options))


;;;###autoload
(defun org-projectile-toggle-subheading ()
  "Toggle subheading setting for heading at point.

When a heading is considered a subheading it will appear in
org-projectile search commands."
  (interactive)
  (let ((was-enabled (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
    (if was-enabled
        (org-delete-property "ORG-PROJECTILE-SUBHEADINGS")
      (org-set-property "ORG-PROJECTILE-SUBHEADINGS" "t"))))

;;;###autoload
(defun org-projectile-template-or-project (&optional arg)
  "Select a project or org capture template and record a TODO.

If ARG is provided use `org-projectile-linked-capture-template'
as the capture template."
  (interactive "P")
  (if (require 'helm-org nil 'noerror)
      (helm :sources
	    (list (helm-source-org-capture-templates)
		  (org-projectile-helm-source
		   (if arg org-projectile-linked-capture-template nil)))
	    :candidate-number-limit 99999
	    :buffer "*helm org capture templates*")
    (user-error "%s" "This command is only available to helm users. Install helm and try again.")))

;;;###autoload
(defun org-projectile-project-todo-completing-read
    (&optional capture-template &rest additional-options)
  "Select a project using a `projectile-completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (interactive)
  (occ-capture
   (make-instance 'occ-context
                  :category (projectile-completing-read
                             "Record TODO for project: "
                             (org-projectile-known-projects))
                  :template (or capture-template org-projectile-capture-template)

                  :strategy org-projectile-capture-strategy)))

;;;###autoload
(defun org-projectile-capture-for-current-project
    (&optional capture-template &rest additional-options)
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
                        :template (or capture-template org-projectile-capture-template)
                        :options additional-options
                        :strategy org-projectile-capture-strategy))
      (error (format "%s is not a recognized projectile project." project-name)))))

(provide 'org-projectile)
;;; org-projectile.el ends here
