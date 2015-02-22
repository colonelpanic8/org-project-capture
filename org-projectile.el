;;; org-projectile.el --- Repository todo management for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org projectile todo
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.0.2
;; Package-Requires: ((projectile "0.11.0"))

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

(require 'org-capture)
(require 'projectile)

(defvar org-projectile:projects-file "~/org/projects.org")
(defvar org-projectile:per-repo-filename "todo.org")

(defvar org-projectile:capture-template "* TODO %?\n")
(defvar org-projectile:linked-capture-template "* TODO %? %A\n")

(defvar org-projectile:force-linked t)

(defvar org-projectile:project-name-to-org-file
  'org-projectile:project-name-to-org-file-one-file)
(defvar org-projectile:project-name-to-location
  'org-projectile:project-name-to-location-one-file)

;; For a single projects file
(defun org-projectile:project-name-to-org-file-one-file (project-name)
  org-projectile:projects-file)

(defun org-projectile:project-name-to-location-one-file (project-name)
  (org-projectile:project-heading project-name)
  (org-end-of-line))

(defun org-projectile:one-file ()
  (interactive)
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-one-file)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-one-file))

;; For repo files in the projectile project path
(defun org-projectile:project-name-to-org-file-per-repo (project-name)
  (concat (org-projectile:project-location-from-name project-name)
          org-projectile:per-repo-filename))

(defun org-projectile:project-name-to-location-per-repo (project-name)
  (end-of-buffer))

(defun org-projectile:per-repo ()
  (interactive)
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
  (setq org-projectile:project-name-to-org-file 'org-projectile:project-name-to-org-file-hybrid)
  (setq org-projectile:project-name-to-location 'org-projectile:project-name-to-location-hybrid))

(defun org-projectile:location-for-project (project-name)
  (let* ((filename (funcall org-projectile:project-name-to-org-file project-name)))
    (switch-to-buffer (find-file-noselect filename))
    (funcall org-projectile:project-name-to-location project-name)))

(defun org-projectile:project-root-of-filepath (filepath)
  ;; TODO(@IvanMalison): Replace this with projectile function if
  ;; https://github.com/bbatsov/projectile/pull/571 is ever accepted.
  (file-truename
   (let ((dir (file-truename filepath)))
     (--reduce-from
          (or acc
              (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (gethash cache-key
                                           projectile-project-root-cache)))
                (if cache-value
                    (if (eq cache-value 'no-project-root)
                        nil
                      cache-value)
                  (let ((value (funcall it dir)))
                    (puthash cache-key (or value 'no-project-root)
                             projectile-project-root-cache)
                    value))))
          nil
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
  (file-name-nondirectory
   (directory-file-name (org-projectile:project-root-of-filepath filename))))

(defun org-projectile:get-link-description (heading)
  (with-temp-buffer
    (insert heading)
    (beginning-of-buffer)
    (if (re-search-forward org-any-link-re nil t)
        (match-string-no-properties 4) heading)))

(defun org-projectile:known-projects ()
  (delete-dups `(,@(mapcar #'org-projectile:project-heading-from-file
                           (projectile-relevant-known-projects))
                 ,@(org-map-entries
                    (lambda () (org-projectile:get-link-description (nth 4 (org-heading-components)))) nil
                    (org-projectile:todo-files)
                    (lambda ()
                      (when (< 1 (nth 1 (org-heading-components)))
                        (point)))))))

(defun org-projectile:todo-files ()
  (remove-if-not #'file-exists-p
                 (delete-dups (cl-loop for project-name in (org-projectile:known-projects)
                        collect (funcall org-projectile:project-name-to-org-file
                                         project-name)))))

(defun org-projectile:project-name-to-location-alist ()
  (cl-loop for project-location in projectile-known-projects
           collect `(,(file-name-nondirectory (directory-file-name project-location)) .
                     ,project-location)))

(defun org-projectile:project-location-from-name (name)
  (cdr (assoc name (org-projectile:project-name-to-location-alist))))

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
	   initial)
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
     `(function ,(lambda () (org-projectile:location-for-project project-name)))))
  (org-capture-place-template))

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
        (insert "* " linked-heading)))
    (nth 4 (org-heading-components))))

(defun org-projectile:linked-heading (heading)
  (org-make-link-string (format "elisp:(org-projectile:open-project \"%s\")" heading) heading))

(defun org-projectile:project-heading (heading)
  (let ((heading-text (org-projectile:insert-or-goto-heading heading)))
    (hide-subtree)
    (org-beginning-of-line)
    (org-set-property "CATEGORY" heading)
    heading-text))

(defun org-projectile:helm-source (&optional capture-template)
  (helm-build-sync-source "Org Capture Options:"
    :candidates (cl-loop for project in (org-projectile:known-projects)
                         collect `(,project . ,project))
    :action `(("Do capture" .
               ,(lambda (project)
                  (org-projectile:capture-for-project project capture-template))))))

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

(provide 'org-projectile)
;;; org-projectile.el ends here
