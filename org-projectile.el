;;; org-projectile.el --- Repository todo capture and management for org-mode with projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode projectile todo tools outlines project capture
;; URL: https://github.com/colonelpanic8/org-project-capture
;; Version: 3.0.0
;; Package-Requires: ((projectile "2.3.0") (dash "2.10.0") (org-project-capture "3.0.0"))

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
(require 'org-project-capture)
(require 'org-project-capture-backend)

(defun org-projectile-to-project-capture-setter (symbol value)
  (set-default symbol value)
  (let ((new-var (intern (replace-regexp-in-string
                          "org-projectile"
                          "org-project-capture"
                          (symbol-name symbol)))))
    (when value
      (set new-var value)
      (message "Please use `%s` instead of `%s`." new-var symbol))))

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

(defcustom org-projectile-strategy nil
  "The strategy that should be used for org-projectile."
  :group 'org-projectile
  :set 'org-projectile-to-project-capture-setter)

(make-obsolete-variable 'org-projectile-strategy 'org-project-capture-strategy "4.0.0")

(defclass org-projectile-combine-strategies
  (org-project-capture-combine-strategies) nil)

(defclass org-projectile-per-project-strategy
  (org-project-capture-per-project-strategy) nil)

(defclass org-projectile-single-file-strategy
  (org-project-capture-single-file-strategy) nil)


;; Projectile backend

(defclass org-project-capture-projectile-backend (org-project-capture-backend) nil)

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
      (projectile-switch-project directory)))

(cl-defmethod org-project-capture-current-project
  ((_backend org-project-capture-projectile-backend))
  (projectile-project-name))

(setq org-project-capture-backend
      (make-instance 'org-project-capture-projectile-backend))

(provide 'org-projectile)
;;; org-projectile.el ends here
