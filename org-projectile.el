;;; org-projectile.el --- Repository todo management for org-mode

;; Copyright (C) 2014 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org projectile todo
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.0.0

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

(defun org-projectile:project-root-of-filepath (filepath)
  "Retrieves the root directory of the project to which filepath
belongs, if available."
  (file-truename
   (let ((dir (file-truename filepath)))
     (--reduce-from
          (or acc
              (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (gethash cache-key projectile-project-root-cache)))
                (if cache-value
                    (if (eq cache-value 'no-project-root)
                        nil
                      cache-value)
                  (let ((value (funcall it dir)))
                    (puthash cache-key (or value 'no-project-root) projectile-project-root-cache)
                    value))))
          nil
          projectile-project-root-files-functions))))

(defun org-projectile:project-todo-entry (&optional todo-format)
  (unless todo-format (setq todo-format "* TODO %?\n"))
  `("p" "Project Todo" entry
    (file+function ,org-projectile:projects-file
                   (lambda () (let ((heading
                                     (org-projectile:insert-heading-for-filename
                                      (org-capture-get :original-file))))
                                (org-projectile:insert-or-goto-heading heading)
                                (org-end-of-line)
                                heading)))
    ,todo-format))

(defun org-projectile:project-heading-from-file (filename)
  (file-name-nondirectory
   (directory-file-name (org-projectile:project-root-of-filepath filename))))

(defun org-projectile:insert-heading-for-filename (filename)
  (let ((project-heading
         (org-projectile:project-heading-from-file
          filename)))
    (with-current-buffer (find-file-noselect org-projectile:projects-file)
      (org-projectile:project-heading project-heading))
    project-heading))

(defun org-projectile:known-projects ()
  (delete-dups `(,@(mapcar #'org-projectile:project-heading-from-file
                           (projectile-relevant-known-projects))
                 ,@(org-map-entries
                    (lambda () (nth 4 (org-heading-components))) nil
                    (list org-projectile:projects-file)
                    (lambda ()
                      (when (< 1 (nth 1 (org-heading-components)))
                        (point)))))))

(defun org-projectile:capture-for-project (heading)
  (org-capture-set-plist (org-projectile:project-todo-entry))
  (with-current-buffer (find-file-noselect org-projectile:projects-file)
    (org-projectile:project-heading heading))  
  (org-capture-set-target-location `(file+headline
                                     ,org-projectile:projects-file ,heading))
  (org-capture-place-template))

(defun org-projectile:insert-or-goto-heading (heading)
  (interactive
   (list (read-string "Heading: ")))
  (goto-char (point-min))
  (unless (derived-mode-p 'org-mode)
    (error
     "Target buffer \"%s\" for file+headline should be in Org mode"
     (current-buffer)))
  (if (re-search-forward
       (format org-complex-heading-regexp-format (regexp-quote heading))
       nil t)
      (goto-char (point-at-bol))
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (insert "* " heading)))

(defun org-projectile:project-heading (heading)
  (interactive
   (list (read-string "Heading: ")))
  (org-projectile:insert-or-goto-heading heading)
  (hide-subtree)
  (org-beginning-of-line)
  (org-set-property "CATEGORY" heading))

;;;###autoload
(defun org-projectile:project-todo-completing-read ()
  (interactive)
  (org-projectile:capture-for-project
   (projectile-completing-read "Record TODO for project:"
                               (org-projectile:known-projects))))

(provide 'org-projectile)
;;; org-projectile.el ends here
