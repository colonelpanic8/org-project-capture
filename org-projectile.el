;;; org-projectile.el --- Repository todo management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org projectile todo
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.2.0
;; Package-Requires: ((projectile "0.11.0") (dash "2.10.0"))

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
(require 'org-capture)
(require 'projectile)

(defvar org-projectile:projects-file "~/org/projects.org")
(defvar org-projectile:per-repo-filename "todo.org")

(defvar org-projectile:capture-template "* TODO %?\n")
(defvar org-projectile:linked-capture-template "* TODO %? %A\n")

(defvar org-projectile:force-linked t)
(defvar org-projectile:counts-in-heading t)
(defvar org-projectile:subheading-selection t)

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
    (org-projectile:prompt-for-subheadings 'tree)))

(defun org-projectile:one-file ()
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-one-file)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-one-file))

;; For repo files in the projectile project path
(defun org-projectile:project-name-to-org-file-per-repo (project-name)
  (concat (org-projectile:project-location-from-name project-name)
          org-projectile:per-repo-filename))

(defun org-projectile:project-name-to-location-per-repo (_project-name)
  (goto-char (point-max)))

(defun org-projectile:per-repo ()
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-per-repo)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-per-repo))

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
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:default-todo-files)
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-hybrid)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-hybrid))

;; Prompt for org file location on a per project basis
(defvar org-projectile:find-org-file-for-project-function nil)
(defvar org-projectile:keep-project-to-org-filepath-in-memory nil)
(defvar org-projectile:project-to-org-filepath 'not-yet-read)
(defvar org-projectile:project-to-org-filepath-filepath
  (concat (file-name-as-directory user-emacs-directory) "project-to-org-filepath"))

(defun org-projectile:write-project-to-org-filepath (project-to-org-filepath
                                                     &optional project-to-org-filepath-filepath)
  (unless project-to-org-filepath-filepath
    (setq project-to-org-filepath-filepath org-projectile:project-to-org-filepath-filepath))
  (with-temp-buffer
    (insert (prin1-to-string project-to-org-filepath))
    (write-region (point-min) (point-max) project-to-org-filepath-filepath nil)))

(defun org-projectile:read-project-to-org-filepath (&optional project-to-org-filepath-filepath)
  (unless project-to-org-filepath-filepath
    (setq project-to-org-filepath-filepath org-projectile:project-to-org-filepath-filepath))
  (when (file-exists-p project-to-org-filepath-filepath)
    (with-temp-buffer
      (insert-file-contents project-to-org-filepath-filepath)
      (read (buffer-string)))))

(defun org-projectile:update-project-to-org-filepath (project-name
                                                      org-file &optional project-to-org-filepath-filepath)
  (let* ((project-to-org-filepath (org-projectile:get-project-to-org-filepath
                                   project-to-org-filepath-filepath))
         (org-file-truename (org-projectile:file-truename org-file))
         (current-value (assoc project-name project-to-org-filepath)))
    (when (or (not (file-exists-p org-file-truename)) (file-directory-p org-file-truename))
      (throw "The provided filepath is invalid" org-file))
    (if current-value (setcdr current-value org-file-truename)
      (cl-pushnew 'project-to-org-filepath `(,project-name . ,org-file-truename)))
    (org-projectile:write-project-to-org-filepath project-to-org-filepath project-to-org-filepath-filepath)))

(defun org-projectile:get-project-to-org-filepath (&optional project-to-org-filepath-filepath)
  (if org-projectile:keep-project-to-org-filepath-in-memory
      (if (eq org-projectile:project-to-org-filepath 'not-yet-read)
          (progn
            (setq org-projectile:project-to-org-filepath
                  (org-projectile:read-project-to-org-filepath project-to-org-filepath-filepath)))
        org-projectile:project-to-org-filepath)
    (org-projectile:read-project-to-org-filepath project-to-org-filepath-filepath)))

(defun org-projectile:project-name-to-org-file-prompt (project-name &optional project-to-org-filepath-filepath)
  (let ((current (assoc project-name (org-projectile:get-project-to-org-filepath))))
    (if current (cdr current)
      (let ((org-filepath (org-projectile:find-project-in-known-files project-name)))
        (unless org-filepath
          (setq org-filepath (org-projectile:no-org-file-for-project project-name
                                                                     project-to-org-filepath-filepath)))
        (org-projectile:update-project-to-org-filepath project-name org-filepath)
        org-filepath))))

(defun org-projectile:no-org-file-for-project (project-name
                                               &optional project-to-org-filepath-filepath)
  (let ((org-filepath (when org-projectile:find-org-file-for-project-function
                        (funcall org-projectile:find-org-file-for-project-function project-name))))
    (unless org-filepath
      (setq org-filepath (org-projectile:prompt-for-project-name
                          project-name project-to-org-filepath-filepath)))
    org-filepath))

(defun org-projectile:prompt-for-project-name (project-name
                                               &optional _project-to-org-filepath-filepath)
  (read-file-name (concat "org-mode file for " project-name ": ")
                  (file-name-directory org-projectile:projects-file)))

(defun org-projectile:set-project-file-default (&optional project-to-org-filepath-filepath)
  (interactive)
  (let ((org-filepath (read-file-name "org-mode file: "
                                      (file-name-directory org-projectile:projects-file))))
    (cl-loop for project-name being the elements of (org-projectile:known-projects) do
             (org-projectile:update-project-to-org-filepath project-name
                                                            org-filepath
                                                            project-to-org-filepath-filepath))
    org-filepath))

(defun org-projectile:find-project-in-known-files (project-name)
  (cl-loop for org-file in (funcall org-projectile:todo-files) when
           (-contains-p (org-map-entries
                         (lambda () (org-projectile:get-link-description
                                     (nth 4 (org-heading-components)))) nil
                                     (list org-file)
                                     (lambda ()
                                       (when (< 1 (nth 1 (org-heading-components)))
                                         (point)))) project-name)
           return org-file))

(fset 'org-projectile:project-name-to-location-prompt
      'org-projectile:project-name-to-location-one-file)

(defun org-projectile:todo-files-project-to-org-filepath ()
  (interactive)
  (delete-dups (cl-loop for elem in (org-projectile:get-project-to-org-filepath) collect
                        (cdr elem))))

(defun org-projectile:set-org-file-for-project ()
  (interactive)
  (org-projectile:prompt-for-project-name (projectile-completing-read
                                           "Select project for which to set org file: "
                                           (org-projectile:known-projects))))

(defun org-projectile:prompt ()
  (interactive)
  (setq org-projectile:todo-files 'org-projectile:todo-files-project-to-org-filepath)
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-prompt)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-prompt))

(defun org-projectile:location-for-project (project-name &optional for-insert)
  (let* ((filename (funcall org-projectile:project-name-to-org-file project-name)))
    (switch-to-buffer (find-file-noselect filename))
    (funcall org-projectile:project-name-to-location project-name)
    (org-end-of-line)
    (org-projectile:end-of-properties)
    ;; It sucks that this has to be done, but we have to insert a
    ;; subheading if the entry does not have one in order to convince
    ;; capture to actually insert the template as a subtree of the
    ;; selected entry. We return a marker where the dummy subheading
    ;; was created so that it can be deleted later.
    (when (and for-insert (not (save-excursion (org-goto-first-child))))
      (save-excursion (org-insert-subheading nil) (point-marker)))))

(defun org-projectile:file-truename (filepath)
  (when filepath
    (file-truename filepath)))

(defun org-projectile:project-root-of-filepath (filepath)
  (org-projectile:file-truename
   (let ((dir (file-name-directory filepath)))
     (--some (let* ((cache-key (format "%s-%s" it dir))
                    (cache-value (gethash cache-key projectile-project-root-cache)))
               (if cache-value
                   cache-value
                 (let ((value (funcall it (org-projectile:file-truename dir))))
                   (puthash cache-key value projectile-project-root-cache)
                   value)))
             projectile-project-root-files-functions))))

(defun org-projectile:project-todo-entry (&optional capture-character capture-template capture-heading)
  (unless capture-template (setq capture-template org-projectile:capture-template))
  (unless capture-character (setq capture-character "p"))
  (unless capture-heading (setq capture-heading "Project Todo"))
  `(,capture-character ,capture-heading entry
                       (function
                        (lambda () (org-projectile:location-for-project
                                    (org-projectile:project-heading-from-file
                                     (org-capture-get :original-file)))))
    ,capture-template))

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
  (remove-if #'null (delete-dups `(,@(mapcar #'org-projectile:project-heading-from-file
                           (projectile-relevant-known-projects))
                 ,@(org-map-entries
                    (lambda () (org-projectile:get-link-description (nth 4 (org-heading-components)))) nil
                    (funcall org-projectile:todo-files)
                    (lambda ()
                      (when (< 1 (nth 1 (org-heading-components)))
                        (point))))))))

(defun org-projectile:todo-files ()
  (funcall org-projectile:todo-files))

(defun org-projectile:default-todo-files ()
  (cl-remove-if-not #'file-exists-p
                 (delete-dups (cl-loop for project-name in
                                       (mapcar #'org-projectile:project-heading-from-file
                                               (projectile-relevant-known-projects))
                        collect (funcall org-projectile:project-name-to-org-file
                                         project-name)))))

(defun org-projectile:project-name-to-location-alist ()
  (cl-loop for project-location in projectile-known-projects
           collect `(,(file-name-nondirectory (directory-file-name project-location)) .
                     ,project-location)))

(defun org-projectile:project-location-from-name (name)
  (cdr (assoc name (org-projectile:project-name-to-location-alist))))

(defvar dired-buffers)

(defun org-projectile:capture-for-project (project-name &optional capture-template)
  (org-capture-set-plist (org-projectile:project-todo-entry nil capture-template))
  ;; TODO: super gross that this had to be copied from org-capture,
  ;; Unfortunately, it does not seem to be possible to call into org-capture
  ;; because it makes assumptions that make it impossible to set things up
  ;; properly
  (let ((orig-buf (current-buffer))
        (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                             org-capture-link-is-already-stored)
                        (plist-get org-store-link-plist :annotation)
                      (ignore-errors (org-store-link nil))))
        org-projectile:subheading-cleanup-marker)
    (org-capture-put :original-buffer orig-buf
                     :original-file (or (buffer-file-name orig-buf)
                                        (and (featurep 'dired)
                                             (car (rassq orig-buf dired-buffers))))
                     :original-file-nondirectory
                     (and (buffer-file-name orig-buf)
                          (file-name-nondirectory
                           (buffer-file-name orig-buf)))
                     :annotation annotation
                     :initial ""
                     :return-to-wconf (current-window-configuration)
                     :default-time
                     (or org-overriding-default-time
                         (org-current-time)))
    (org-capture-put :template (org-capture-fill-template capture-template))
    (org-capture-set-target-location
     `(function ,(lambda () (setq org-projectile:subheading-cleanup-marker
                                  (org-projectile:location-for-project project-name t)))))
    ;; Apparently this needs to be forced because (org-at-heading-p)
    ;; will not be true and so `org-capture-set-target-location` will
    ;; set this value to nil.
    ;; TODO(@IvanMalison): Perhaps there is a better way to do this?
    ;; Maybe something that would allow us to get rid of the horrible
    ;; subheading-cleanup-marker hack?
    (org-capture-put :target-entry-p t)
    (org-capture-place-template)
    (when org-projectile:subheading-cleanup-marker
      (org-projectile:cleanup-subheading org-projectile:subheading-cleanup-marker))))

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
                 (format "%s\\|%s" (regexp-quote linked-heading) (regexp-quote heading)))
         nil t)
        (progn
          (goto-char (point-at-bol))
          (when (and org-projectile:force-linked
                     (looking-at
                      (format org-complex-heading-regexp-format (regexp-quote heading))))
            (re-search-forward heading)
            (org-show-subtree)
            (delete-char (* (length heading) -1))
            (insert linked-heading)
            (goto-char (point-at-bol))))
      (progn
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " linked-heading)
        (when org-projectile:counts-in-heading (insert " [/]"))))
    (nth 4 (org-heading-components))))

(defun org-projectile:linked-heading (heading)
  (org-make-link-string (format "elisp:(org-projectile:open-project \"%s\")" heading) heading))

(defun org-projectile:project-heading (heading)
  (let ((heading-text (org-projectile:insert-or-goto-heading heading)))
    (hide-subtree)
    (org-beginning-of-line)
    (org-set-property "CATEGORY" heading)
    heading-text))

(defun org-projectile:end-of-properties ()
  (interactive)
  (let ((end-of-heading (save-excursion (outline-next-heading) (point)))
        (last-match t))
    (while last-match (setq last-match (re-search-forward ":END:" end-of-heading t)))
    (point)))

(defun org-projectile:prompt-for-subheadings (&optional recursive)
  (let ((subheadings-to-point (org-projectile:get-subheadings))
        (point-at-start (save-excursion (org-back-to-heading) (point))))
    (when (> (length subheadings-to-point) 1)
      (org-projectile:prompt-for-and-move-to-subheading subheadings-to-point)
      (when recursive
        (unless (eq point-at-start (save-excursion (org-back-to-heading) (point)))
          (org-projectile:prompt-for-subheadings))))))

;; Assure the byte compiler that helm functions exist since we don't
;; explicitly depend on helm.
(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source" t t)
(declare-function helm-source-org-capture-templates "helm-org")

(defun org-projectile:prompt-for-and-move-to-subheading (subheadings-to-point)
  (cond ((eq projectile-completion-system 'helm)
         (let ((selection (helm :sources (org-projectile:helm-subheadings-source subheadings-to-point))))
           (goto-char selection)))))

(defun org-projectile:helm-subheadings-source (subheadings-to-point)
  (helm-build-sync-source "Choose a subheading:"
    :candidates subheadings-to-point))

(defun org-projectile:get-subheadings (&optional scope)
  (interactive)
  (unless scope (setq scope 'tree))
  (org-show-subtree)
  (save-excursion
    (org-map-entries (lambda () `(,(org-get-heading) . ,(point))) nil scope
                     (lambda () (when (and (nth 2 (org-heading-components))
                                           (not (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
                                  (org-end-of-subtree))))))

(defun org-projectile:helm-source (&optional capture-template)
  (helm-build-sync-source "Org Capture Options:"
    :candidates (cl-loop for project in (org-projectile:known-projects)
                         collect `(,project . ,project))
    :action `(("Do capture" .
               ,(lambda (project)
                  (org-projectile:capture-for-project project capture-template))))))

;;;###autoload
(defun org-projectile:toggle-subheading ()
  (interactive)
  (let ((was-enabled (org-entry-get nil "ORG-PROJECTILE-SUBHEADINGS")))
    (if was-enabled
        (org-delete-property "ORG-PROJECTILE-SUBHEADINGS")
      (org-set-property "ORG-PROJECTILE-SUBHEADINGS" "t"))))

;;;###autoload
(defun org-projectile:template-or-project (&optional arg)
  (interactive "P")
  (helm :sources
        (list (helm-source-org-capture-templates)
              (org-projectile:helm-source
               (if arg org-projectile:linked-capture-template nil)))
        :candidate-number-limit 99999
        :buffer "*helm org capture templates*"))

;;;###autoload
(defun org-projectile:project-todo-completing-read (&optional capture-template)
  (interactive)
  (org-projectile:capture-for-project
   (projectile-completing-read "Record TODO for project: "
                               (org-projectile:known-projects))
   capture-template))

;;;###autoload
(defun org-projectile:capture-for-current-project (&optional capture-template)
  (interactive)
  (let ((project-name (projectile-project-name)))
    (if (projectile-project-p)
        (org-projectile:capture-for-project project-name capture-template)
      (error (format "%s is not a recognized projectile project." project-name)))))

(provide 'org-projectile)
;;; org-projectile.el ends here
