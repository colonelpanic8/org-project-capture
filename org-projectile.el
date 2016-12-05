;;; org-projectile.el --- Repository todo management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org projectile todo
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

(defcustom org-projectile:projects-file "~/org/projects.org"
  "The path to the file in which projectile TODOs will be stored."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile:per-repo-filename "TODO.org"
  "The path (relative to the project repository) to project TODOs."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile:capture-template "* TODO %?\n"
  "The default capture template to use for org-projectile TODOs."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile:linked-capture-template "* TODO %? %A\n"
  "The default linked capture template to use for org-projectile TODOs."
  :type '(string)
  :group 'org-projectile)

(defcustom org-projectile:force-linked t
  "Whether to make project category headings links to their projects."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile:counts-in-heading t
  "Whether or not to make projectile category headings display counts."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile:subheading-selection t
  "Controls whether or not project subheading selection is enabled."
  :type '(boolean)
  :group 'org-projectile)

(defcustom org-projectile:allow-tramp-projects nil
  "Whether to make project category headings links to their projects."
  :type '(boolean)
  :group 'org-projectile)



(defvar org-projectile:target-entry t)

(defclass org-projectile:migration-strategy (occ-strategy) nil)

(defmethod occ-get-categories ((strategy org-projectile:migration-strategy))
  (org-projectile:known-projects))

(defmethod occ-get-todo-files ((strategy org-projectile:migration-strategy))
  (org-projectile:todo-files))

(defmethod occ-get-capture-file ((strategy org-projectile:migration-strategy) context)
  (with-slots (category) context
      (funcall org-projectile:project-name-to-org-file category)))

(defmethod occ-get-capture-marker ((strategy org-projectile:migration-strategy) context)
  "Return a marker that corresponds to the capture location for CONTEXT."
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy context)))
      (save-excursion (with-current-buffer (find-file-noselect filepath t)
        (funcall org-projectile:project-name-to-location category)
        (point-marker))))))

(defmethod occ-target-entry-p ((strategy org-projectile:migration-strategy) context)
  org-projectile:target-entry)

(defvar org-projectile:capture-strategy
  (make-instance 'org-projectile:migration-strategy))



(defvar org-projectile:project-name-to-org-file
  'org-projectile:project-name-to-org-file-one-file)

(defvar org-projectile:project-name-to-location
  'org-projectile:project-name-to-location-one-file)

(defvar org-projectile:todo-files 'org-projectile:default-todo-files)

;; For a single projects file
(defun org-projectile:project-name-to-org-file-one-file (_project-name)
  org-projectile:projects-file)

(defun org-projectile:project-name-to-location-one-file (project-name)
  (org-projectile:project-heading project-name)
  (when org-projectile:subheading-selection
    (org-projectile:prompt-for-subheadings 'tree))
  t)

(defun org-projectile:one-file ()
  "Use org-projectile in one-file mode."
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file
        'org-projectile:project-name-to-org-file-one-file)
  (setq org-projectile:project-name-to-location
        'org-projectile:project-name-to-location-one-file)
  (setq org-projectile:target-entry t))

;; For repo files in the projectile project path
(defun org-projectile:project-name-to-org-file-per-repo (project-name)
  (concat (org-projectile:project-location-from-name project-name)
          org-projectile:per-repo-filename))

(defun org-projectile:project-name-to-location-per-repo (_project-name)
  (goto-char (point-max))
  nil)

(defun org-projectile:per-repo ()
  "Use org-projectile in per-repo mode."
  (interactive)
  (setq org-projectile:target-entry nil)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file
        'org-projectile:project-name-to-org-file-per-repo)
  (setq org-projectile:project-name-to-location
        'org-projectile:project-name-to-location-per-repo))

;; Hybrid of the two approaches mentioned above
(defvar org-projectile:project-to-approach nil)
(defvar org-projectile:default-approach 'one-file)

(defun org-projectile:get-approach-for-project (project-name)
  (or (cdr (assoc project-name org-projectile:project-to-approach))
      org-projectile:default-approach))

(defun org-projectile:project-name-to-org-file-hybrid (project-name)
  (let ((approach (org-projectile:get-approach-for-project project-name)))
     (cond
     ((equal approach 'one-file)
      (org-projectile:project-name-to-org-file-one-file project-name))
     ((equal approach 'per-repo)
      (org-projectile:project-name-to-org-file-per-repo project-name)))))

(defun org-projectile:project-name-to-location-hybrid (project-name)
  (let ((approach (org-projectile:get-approach-for-project project-name)))
    (cond
     ((equal approach 'one-file)
      (org-projectile:project-name-to-location-one-file project-name))
     ((equal approach 'per-repo)
      (org-projectile:project-name-to-location-per-repo project-name)))))

(defun org-projectile:hybrid ()
  "Use org-projectile in hybrid mode."
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file
        'org-projectile:project-name-to-org-file-hybrid)
  (setq org-projectile:project-name-to-location
        'org-projectile:project-name-to-location-hybrid))

;; Prompt for org file location on a per project basis
(defvar org-projectile:find-org-file-for-project-function nil)
(defvar org-projectile:keep-project-to-org-filepath-in-memory nil)
(defvar org-projectile:project-to-org-filepath 'not-yet-read)
(defvar org-projectile:project-to-org-filepath-filepath
  (concat (file-name-as-directory user-emacs-directory) "project-to-org-filepath"))

(defun org-projectile:write-project-to-org-filepath
    (project-to-org-filepath &optional project-to-org-filepath-filepath)
  (unless project-to-org-filepath-filepath
    (setq project-to-org-filepath-filepath
          org-projectile:project-to-org-filepath-filepath))
  (with-temp-buffer
    (insert (prin1-to-string project-to-org-filepath))
    (write-region (point-min) (point-max) project-to-org-filepath-filepath nil)))

(defun org-projectile:read-project-to-org-filepath
    (&optional project-to-org-filepath-filepath)
  (unless project-to-org-filepath-filepath
    (setq project-to-org-filepath-filepath
          org-projectile:project-to-org-filepath-filepath))
  (when (file-exists-p project-to-org-filepath-filepath)
    (with-temp-buffer
      (insert-file-contents project-to-org-filepath-filepath)
      (read (buffer-string)))))

(defun org-projectile:update-project-to-org-filepath
    (project-name org-file &optional project-to-org-filepath-filepath)
  (let* ((project-to-org-filepath (org-projectile:get-project-to-org-filepath
                                   project-to-org-filepath-filepath))
         (org-file-truename (org-projectile:file-truename org-file))
         (current-value (assoc project-name project-to-org-filepath)))
    (when (or (not (file-exists-p org-file-truename)) (file-directory-p org-file-truename))
      (throw "The provided filepath is invalid" org-file))
    (if current-value
        (setcdr current-value org-file-truename)
      (push (cons project-name org-file-truename)
            project-to-org-filepath))
    (org-projectile:write-project-to-org-filepath
     project-to-org-filepath project-to-org-filepath-filepath)))

(defun org-projectile:get-project-to-org-filepath
    (&optional project-to-org-filepath-filepath)
  (if org-projectile:keep-project-to-org-filepath-in-memory
      (if (eq org-projectile:project-to-org-filepath 'not-yet-read)
          (progn
            (setq org-projectile:project-to-org-filepath
                  (org-projectile:read-project-to-org-filepath
                   project-to-org-filepath-filepath)))
        org-projectile:project-to-org-filepath)
    (org-projectile:read-project-to-org-filepath
     project-to-org-filepath-filepath)))

(defun org-projectile:project-name-to-org-file-prompt
    (project-name &optional project-to-org-filepath-filepath)
  (let ((current (assoc project-name (org-projectile:get-project-to-org-filepath))))
    (if current (cdr current)
      (let ((org-filepath (org-projectile:find-project-in-known-files project-name)))
        (unless org-filepath
          (setq org-filepath
                (org-projectile:no-org-file-for-project project-name
                                                        project-to-org-filepath-filepath)))
        (org-projectile:update-project-to-org-filepath project-name org-filepath) org-filepath))))

(defun org-projectile:no-org-file-for-project
    (project-name &optional project-to-org-filepath-filepath)
  (let ((org-filepath (when org-projectile:find-org-file-for-project-function
                        (funcall
                         org-projectile:find-org-file-for-project-function
                         project-name))))
    (unless org-filepath
      (setq org-filepath (org-projectile:prompt-for-project-name
                          project-name project-to-org-filepath-filepath)))
    org-filepath))

(defun org-projectile:prompt-for-project-name
    (project-name &optional _project-to-org-filepath-filepath)
  (read-file-name (concat "org-mode file for " project-name ": ")
                  (file-name-directory org-projectile:projects-file)))

(defun org-projectile:set-project-file-default
    (&optional project-to-org-filepath-filepath)
  "Set the filepath for any known projects that do not have a filepath.

If PROJECT-TO-ORG-FILEPATH-FILEPATH is provided use that as the
location of the filepath cache."
  (interactive)
  (let ((org-filepath
         (read-file-name "org-mode file: "
                         (file-name-directory org-projectile:projects-file))))
    (cl-loop for project-name being the elements of (org-projectile:known-projects)
             do (org-projectile:update-project-to-org-filepath
                 project-name org-filepath project-to-org-filepath-filepath))
    org-filepath))

(defun org-projectile:find-project-in-known-files (project-name)
  (cl-loop for org-file in (funcall org-projectile:todo-files) when
           (-contains-p
            (org-map-entries (lambda ()
                               (org-projectile:get-link-description
                                (nth 4 (org-heading-components)))) nil
                                (list org-file)
                                (lambda ()
                                  (when (< 1 (nth 1 (org-heading-components)))
                                    (point)))) project-name)
           return org-file))

(fset 'org-projectile:project-name-to-location-prompt
      'org-projectile:project-name-to-location-one-file)

(defun org-projectile:todo-files-project-to-org-filepath ()
  (delete-dups
   (cl-loop for elem in (org-projectile:get-project-to-org-filepath)
            collect (cdr elem))))

(defun org-projectile:set-org-file-for-project ()
  "Set the org file to use for a projectile project."
  (interactive)
  (org-projectile:update-project-to-org-filepath
   (org-projectile:prompt-for-project-name
    (projectile-completing-read "Select project for which to set org file: "
                                (org-projectile:known-projects)))
   (read-file-name "Select an org file: ")))

(defun org-projectile:prompt ()
  "Use the prompt mode of org-projectile."
  (interactive)
  ;; TODO this needs to be more nuanced.
  (setq org-projectile:target-entry t)
  (setq org-projectile:todo-files
        'org-projectile:todo-files-project-to-org-filepath)
  (setq org-projectile:project-name-to-org-file
        'org-projectile:project-name-to-org-file-prompt)
  (setq org-projectile:project-name-to-location
        'org-projectile:project-name-to-location-prompt))

(defun org-projectile:location-for-project (project-name)
  (let* ((filename (funcall org-projectile:project-name-to-org-file project-name)))
    (switch-to-buffer (find-file-noselect filename))
    (funcall org-projectile:project-name-to-location project-name)))

(defun org-projectile:file-truename (filepath)
  (when filepath
    (if (find-file-name-handler filepath 'file-truename)
        filepath ;; skip if the file requires special handling
      (file-truename filepath))))

(defun org-projectile:project-root-of-filepath (filepath)
  (let ((no-handler (eq nil (find-file-name-handler filepath 'file-truename))))
    (when (or no-handler org-projectile:allow-tramp-projects)
      (let ((dir (file-name-directory filepath)))
        (--some (let* ((cache-key (format "%s-%s" it dir))
                       (cache-value (gethash
                                     cache-key projectile-project-root-cache)))
                  (if cache-value
                      cache-value
                    (let ((value (funcall it (org-projectile:file-truename dir))))
                      (puthash cache-key value projectile-project-root-cache)
                      value)))
                projectile-project-root-files-functions)))))

(defun org-projectile:project-todo-entry
    (&optional capture-character capture-template capture-heading
               &rest additional-options)
  (unless capture-template (setq capture-template
                                 org-projectile:capture-template))
  (unless capture-character (setq capture-character "p"))
  (unless capture-heading (setq capture-heading "Project Todo"))
  `(,capture-character ,capture-heading entry
                       (function
                        (lambda () (org-projectile:location-for-project
                                    (org-projectile:project-heading-from-file
                                     (org-capture-get :original-file)))))
    ,capture-template ,@additional-options))

(defun org-projectile:project-heading-from-file (filename)
  (let ((project-root (org-projectile:project-root-of-filepath filename)))
    (when project-root
      (file-name-nondirectory
       (directory-file-name project-root)))))

(defun org-projectile:get-link-description (heading)
  (with-temp-buffer
    (insert heading)
    (goto-char (point-min))
    (if (re-search-forward org-any-link-re nil t)
        (match-string-no-properties 4) heading)))

(defun org-projectile:known-projects ()
  (cl-remove-if
   'null
   (delete-dups
    (nconc
     (mapcar #'org-projectile:project-heading-from-file
                (projectile-relevant-known-projects))
     (org-map-entries
         (lambda () (org-projectile:get-link-description
                     (nth 4 (org-heading-components)))) nil
                     (funcall org-projectile:todo-files)
                     (lambda ()
                       (when (< 1 (nth 1 (org-heading-components)))
                         (point))))))))

(defun org-projectile:todo-files ()
  (funcall org-projectile:todo-files))

(defun org-projectile:default-todo-files ()
  (cl-remove-if-not
   #'file-exists-p
   (delete-dups
    (cl-loop for project-name in
             (mapcar #'org-projectile:project-heading-from-file
                     (projectile-relevant-known-projects))
             collect (funcall org-projectile:project-name-to-org-file
                              project-name)))))

(defun org-projectile:project-name-to-location-alist ()
  (cl-loop for project-location in projectile-known-projects
           collect `(,(file-name-nondirectory
                       (directory-file-name project-location)) .
                       ,project-location)))

(defun org-projectile:project-location-from-name (name)
  (cdr (assoc name (org-projectile:project-name-to-location-alist))))

(defun org-projectile:cleanup-subheading (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion (goto-char (marker-position marker))
    (kill-whole-line))))

(defun org-projectile:open-project (name)
  (let* ((name-to-location (org-projectile:project-name-to-location-alist))
         (entry (assoc name name-to-location)))
    (when entry
      (projectile-switch-project-by-name (cdr entry)))))

(defun org-projectile:insert-or-goto-heading (heading)
  (goto-char (point-min))
  (unless (derived-mode-p 'org-mode)
    (error
     "Target buffer \"%s\" for file+headline should be in Org mode"
     (current-buffer)))
  (let ((linked-heading (org-projectile:linked-heading heading)))
    (if (re-search-forward
         (format org-complex-heading-regexp-format
                 (format "%s\\|%s" (regexp-quote linked-heading)
                         (regexp-quote heading)))
         nil t)
        (progn
          (goto-char (point-at-bol))
          (when (and org-projectile:force-linked
                     (looking-at
                      (format org-complex-heading-regexp-format
                              (regexp-quote heading))))
            (re-search-forward heading)
            (org-show-subtree)
            (delete-char (* (length heading) -1))
            (insert linked-heading)
            (goto-char (point-at-bol))))
      (progn
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (let ((org-insert-heading-respect-content t))
          (org-insert-heading nil nil t))
        (insert linked-heading)
        (when org-projectile:counts-in-heading (insert " [/]"))))
    (nth 4 (org-heading-components))))

(defun org-projectile:linked-heading (heading)
  (org-make-link-string
   (format "elisp:(org-projectile:open-project \"%s\")" heading) heading))

(defun org-projectile:project-heading (heading)
  (let ((heading-text (org-projectile:insert-or-goto-heading heading)))
    (hide-subtree)
    (org-beginning-of-line)
    (org-set-property "CATEGORY" heading)
    heading-text))

(defun org-projectile:end-of-properties ()
  (let ((end-of-heading (save-excursion (outline-next-heading) (point)))
        (last-match t))
    (while last-match (setq last-match
                            (re-search-forward ":END:" end-of-heading t)))
    (point)))

(defun org-projectile:prompt-for-subheadings (&optional recursive)
  (let ((subheadings-to-point (org-projectile:get-subheadings))
        (point-at-start (save-excursion (org-back-to-heading) (point))))
    (when (> (length subheadings-to-point) 1)
      (org-projectile:prompt-for-and-move-to-subheading subheadings-to-point)
      (when recursive
        (unless (eq point-at-start (save-excursion (org-back-to-heading) (point)))
          (org-projectile:prompt-for-subheadings))))))

;; Only define the following functions if helm is installed
(when (require 'helm-source nil 'noerror)
  (defun org-projectile:prompt-for-and-move-to-subheading (subheadings-to-point)
    (cond ((eq projectile-completion-system 'helm)
           (let ((selection
                  (helm :sources (org-projectile:helm-subheadings-source
                                  subheadings-to-point))))
             (goto-char selection)))))

  (defun org-projectile:helm-subheadings-source (subheadings-to-point)
    (helm-build-sync-source "Choose a subheading:"
      :candidates subheadings-to-point))

  (defun org-projectile:helm-source (&optional capture-template)
    (helm-build-sync-source "Org Capture Options:"
      :candidates (cl-loop for project in (org-projectile:known-projects)
                           collect `(,project . ,project))
      :action `(("Do capture" .
                 ,(lambda (project)
                    (occ-capture
                     (make-instance 'occ-context
                                    :category project
                                    :options nil
                                    :template (or capture-template org-projectile:capture-template)
                                    :strategy org-projectile:capture-strategy))))))))

(defun org-projectile:get-subheadings (&optional scope)
  (unless scope (setq scope 'tree))
  (org-show-subtree)
  (save-excursion
    (org-map-entries (lambda () `(,(org-get-heading) . ,(point))) nil scope
                     (lambda () (when (and (nth 2 (org-heading-components))
                                           (not (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
                                  (org-end-of-subtree))))))

;;;###autoload
(defun org-projectile:toggle-subheading ()
  "Toggle subheading setting for heading at point.

When a heading is considered a subheading it will appear in
org-projectile search commands."
  (interactive)
  (let ((was-enabled (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
    (if was-enabled
        (org-delete-property "ORG-PROJECTILE-SUBHEADINGS")
      (org-set-property "ORG-PROJECTILE-SUBHEADINGS" "t"))))

;;;###autoload
(defun org-projectile:template-or-project (&optional arg)
  "Select a project or org capture template and record a TODO.

If ARG is provided use `org-projectile:linked-capture-template'
as the capture template."
  (interactive "P")
  (if (require 'helm-org nil 'noerror)
      (helm :sources
	    (list (helm-source-org-capture-templates)
		  (org-projectile:helm-source
		   (if arg org-projectile:linked-capture-template nil)))
	    :candidate-number-limit 99999
	    :buffer "*helm org capture templates*")
    (user-error "%s" "This command is only available to helm users. Install helm and try again.")))

;;;###autoload
(defun org-projectile:project-todo-completing-read
    (&optional capture-template &rest additional-options)
  "Select a project using a `projectile-completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template for the TODO."
  (interactive)
  (occ-capture
   (make-instance 'occ-context
                  :category (projectile-completing-read
                             "Record TODO for project: "
                             (org-projectile:known-projects))
                  :template (or capture-template org-projectile:capture-template)
                  :options additional-options
                  :strategy org-projectile:capture-strategy)))

;;;###autoload
(defun org-projectile:capture-for-current-project
    (&optional capture-template &rest additional-options)
  "Capture a TODO for the current active projectile project.

If CAPTURE-TEMPLATE is provided use it as the capture template for the TODO."
  (interactive)
  (let ((project-name (projectile-project-name)))
    (if (projectile-project-p)
        (occ-capture
         (make-instance 'occ-context
                        :category project-name
                        :template (or capture-template org-projectile:capture-template)
                        :options additional-options
                        :strategy org-projectile:capture-strategy))
      (error (format "%s is not a recognized projectile project." project-name)))))

(provide 'org-projectile)
;;; org-projectile.el ends here
