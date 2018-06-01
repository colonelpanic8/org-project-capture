;;; org-projectile-helm.el --- helm functions for org-projectile  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org projectile todo helm outlines
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.0.0
;; Package-Requires: ((org-projectile "1.0.0") (helm "2.3.1") (emacs "25"))

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

(require 'helm)
(require 'helm-org)
(require 'helm-source)
(require 'org-category-capture)
(require 'org-projectile)

(defun org-projectile-helm-prompt-for-and-move-to-subheading
  (subheadings-to-point)
  (cond ((eq projectile-completion-system 'helm)
          (let ((selection
                  (helm :sources (org-projectile-helm-subheadings-source
                                   subheadings-to-point))))
            (goto-char selection)))))

(defun org-projectile-helm-subheadings-source (subheadings-to-point)
  (helm-build-sync-source "Choose a subheading:"
    :candidates subheadings-to-point))

(defun org-projectile-helm-source (&optional capture-template)
  (helm-build-sync-source "Org Capture Options:"
    :candidates (cl-loop for project in
                         (occ-get-categories org-projectile-strategy) collect
                         (cons project project))
    :action `(("Do capture" .
                ,(lambda (project)
                   (occ-capture
                     (make-instance 'occ-context :category project
                       :options nil
                       :template (or capture-template
                                   org-projectile-capture-template)
                       :strategy org-projectile-strategy)))))))

;;;###autoload
(defun org-projectile-helm-template-or-project
    (&optional capture-template-for-project)
  "Select a project or `org-capture' template and record a TODO.

If provided, CAPTURE-TEMPLATE-FOR-PROJECT will be the capture
template used for project TODO capture."
  (interactive "P")
  (helm :sources
	(list (helm-source-org-capture-templates)
	  (org-projectile-helm-source capture-template-for-project))
	:candidate-number-limit 99999
	:buffer "*helm org capture templates*"))

(provide 'org-projectile-helm)
;;; org-projectile-helm.el ends here
