;;; org-category-capture.el --- Tools for the contextual capture of org-mode TODOs. -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

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

(require 'eieio)
(require 'org)
(require 'org-capture)

(defclass occ-strategy ()
  ())

(defmethod occ-get-categories ((strategy occ-strategy)))

(defmethod occ-get-todo-files ((strategy occ-strategy)))

(defmethod occ-get-capture-location ((strategy occ-strategy) category)
  (let ((filepath (occ-get-capture-file strategy category)))
    (org-find-exact-headline-in-buffer
     category (find-file-noselect filepath t))))

(defmethod occ-get-capture-file ((strategy occ-strategy) category))

(defmethod occ-build-context ((strategy occ-strategy) &rest args)
  (apply 'make-instance :strategy strategy args))

(defclass occ-context ()
  ((category :initarg :category)
   (template :initarg :template)
   (options :initarg :options)
   (strategy :initarg :strategy)))

(cl-defmethod occ-build-capture-template
    ((context occ-context) &key (character "p") (heading "Category TODO"))

  (with-slots (template options strategy) context
    (apply 'list character heading 'entry
           (list 'function (apply-partially 'occ-get-capture-location strategy context))
           template options)))

(defmethod occ-capture ((context occ-context))
  (with-slots (category template options strategy)
      context
    (org-capture-set-plist (occ-build-capture-template context))
    ;; TODO/XXX: super gross that this had to be copied from org-capture,
    ;; Unfortunately, it does not seem to be possible to call into org-capture
    ;; because it makes assumptions that make it impossible to set things up
    ;; properly
    (let ((orig-buf (current-buffer))
          (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                               org-capture-link-is-already-stored)
                          (plist-get org-store-link-plist :annotation)
                        (ignore-errors (org-store-link nil))))
          org-projectile:subheading-cleanup-marker
          org-projectile:do-target-entry)
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
       `(function ,(lambda () (setq org-projectile:do-target-entry
                                    (org-projectile:location-for-project project-name)))))
      ;; Apparently this needs to be forced because (org-at-heading-p)
      ;; will not be true and so `org-capture-set-target-location` will
      ;; set this value to nil.
      ;; TODO(@IvanMalison): Perhaps there is a better way to do this?
      ;; Maybe something that would allow us to get rid of the horrible
      ;; subheading-cleanup-marker hack?
      (org-capture-put :target-entry-p org-projectile:do-target-entry)
      (when org-projectile:do-target-entry
        (setq org-projectile:subheading-cleanup-marker
              (org-projectile:target-subheading-and-return-marker)))
      (org-capture-place-template)
      (when org-projectile:subheading-cleanup-marker
        (org-projectile:cleanup-subheading
         org-projectile:subheading-cleanup-marker)))))

(provide 'org-category-capture)
;;; org-category-capture.el ends here
