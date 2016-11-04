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

(require 'cl-lib)
(require 'eieio)
(require 'org)
(require 'org-capture)
;; XXX: dired-buffers is used below
(require 'dired)

(defclass occ-strategy ()
  ())

(defmethod occ-get-categories ((strategy occ-strategy)))

(defmethod occ-get-todo-files ((strategy occ-strategy)))

(defmethod occ-get-capture-file ((strategy occ-strategy) category))

(defmethod occ-get-capture-marker ((strategy occ-strategy) context)
  "Return a marker that corresponds to the capture location for CONTEXT."
  (with-slots (category) context
    (let ((filepath (occ-get-capture-file strategy context)))
      (org-find-exact-headline-in-buffer
       category (find-file-noselect filepath t)))))

(defmethod occ-target-entry-p ((strategy occ-strategy) context)
  t)

(defclass occ-context ()
  ((category :initarg :category)
   (template :initarg :template)
   (options :initarg :options)
   (strategy :initarg :strategy)))

(defmethod occ-build-capture-template
  ((context occ-context) &rest args)
  (apply 'occ-build-capture-template-emacs-24-hack context args))

;; This is needed becaused cl-defmethod doesn't exist in emacs24
(cl-defun occ-build-capture-template-emacs-24-hack
    (context &key (character "p") (heading "Category TODO"))
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
    ;; properly. Specifically, the business logic of `org-capture' is tightly
    ;; coupled to the UI/user interactions that usually take place.
    (let ((orig-buf (current-buffer))
          (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                               org-capture-link-is-already-stored)
                          (plist-get org-store-link-plist :annotation)
                        (ignore-errors (org-store-link nil)))))
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
      (org-capture-put :template (org-capture-fill-template template))
      (org-capture-set-target-location
       (list 'function (lambda ()
                         (occ-capture-goto-marker context))))
      (org-capture-put :target-entry-p (occ-target-entry-p strategy context))
      (org-capture-place-template))))

(defun occ-capture-goto-marker (context)
  (let ((marker (occ-get-capture-marker context)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defmethod occ-get-capture-marker ((context occ-context))
  (occ-get-capture-marker (oref context strategy) context))

(provide 'org-category-capture)
;;; org-category-capture.el ends here
