;;; org-category-capture.el --- Contextualy capture of org-mode TODOs. -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: org-mode todo tools outlines
;; URL: https://github.com/IvanMalison/org-projectile
;; Version: 0.0.0
;; Package-Requires: ((org "9.0.0") (emacs "24"))

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

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'org)
(require 'org-capture)
;; XXX: dired-buffers is used below
(require 'dired)

(defclass occ-strategy nil nil)

(cl-defmethod occ-get-categories ((_ occ-strategy)))

(cl-defmethod occ-get-todo-files ((_ occ-strategy)))

(cl-defmethod occ-get-capture-marker ((_ occ-strategy) _context)
  "Return a marker that corresponds to the capture location for CONTEXT.")

(cl-defmethod occ-target-entry-p ((_ occ-strategy) _context))

(defclass occ-context ()
  ((category :initarg :category)
   (template :initarg :template)
   (options :initarg :options)
   (strategy :initarg :strategy)))

(cl-defmethod occ-build-capture-template
    (context &key (character "p") (heading "Category TODO"))
  (with-slots (template options strategy) context
    (apply 'list character heading 'entry
           (list 'function
                 (apply-partially 'occ-get-capture-location strategy context))
           template options)))

(cl-defmethod occ-capture ((context occ-context))
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

(cl-defmethod occ-get-capture-marker ((context occ-context))
  (occ-get-capture-marker (oref context strategy) context))

(cl-defun occ-get-category-heading-location
    (category &rest args &key do-tree &allow-other-keys)
  "Find a heading with text or category CATEGORY."
  (save-excursion
    (let (result)
      (org-map-entries
       (lambda ()
         (when (and (not result)
                    (equal (apply 'occ-get-heading-category args) category))
           (setq result (point))))
       nil (when do-tree 'tree)
       (1+ (org-current-level))
       (occ-level-filter (if do-tree (1+ (org-current-level)) 1)))
      result)))

(defun occ-insert-after-current-heading ()
  (org-end-of-line)
  (org-insert-heading t t t))

(defun occ-insert-at-end-of-file ()
  (goto-char (point-max))
  (org-insert-heading t t t))

(cl-defun occ-goto-or-insert-category-heading
    (category &rest args &key (build-heading 'identity)
              (insert-heading-fn 'occ-insert-at-end-of-file)
              &allow-other-keys)
  "Create a heading for CATEGORY unless one is found with `occ-goto-category-heading'.

BUILD-HEADING will be applied to category to create the heading
text. INSERT-HEADING-FN is the function that will be used to
create the new bullet for the category heading. This function is
tuned so that by default it looks and creates top level headings."
  (let ((category-location
         (apply 'occ-get-category-heading-location category args)))
    (if category-location
        (goto-char category-location)
      (funcall insert-heading-fn)
      (org-set-property "CATEGORY" category)
      (insert (funcall build-heading category)))))

(defun occ-end-of-properties ()
  (let ((pb (org-get-property-block (point))))
    (when pb (goto-char (cdr pb))))
  (end-of-line))

(defun occ-insert-subheading ()
  (occ-end-of-properties)
  (org-insert-subheading t))

(defun occ-goto-or-insert-category-heading-subtree (category &rest args)
  "Call `occ-goto-or-insert-category-heading' with CATEGORY forwarding ARGS.

Provide arguments that will make it consider subheadings of the
current heading."
  (apply 'occ-goto-or-insert-category-heading
         category :insert-heading-fn 'occ-insert-subheading :do-tree t args))

(defun occ-level-filter (level)
  (lambda ()
    (unless (equal (org-current-level) level)
      (point))))

(defun occ-get-value-by-category-from-filepath (filepath &rest args)
  (with-current-buffer (find-file-noselect filepath)
    (apply 'occ-get-value-by-category args)))

(cl-defun occ-get-heading-category
    (&key (get-category-from-element 'org-get-heading) &allow-other-keys)
  (let ((element-end (plist-get (cadr (org-element-at-point)) :end)))
    (if (save-excursion
          (re-search-forward (org-re-property "CATEGORY") element-end t))
        (org-get-category)
      (funcall get-category-from-element))))

(cl-defun occ-get-value-by-category
    (&rest args &key goto-subtree property-fn &allow-other-keys)
  (org-refresh-category-properties)
  (when goto-subtree (funcall goto-subtree))
  (org-map-entries
   (lambda ()
     (cons (apply 'occ-get-heading-category args)
           (when property-fn (funcall property-fn))))
   nil (when goto-subtree 'tree)
   (occ-level-filter (if goto-subtree (1+ (org-current-level)) 1))))

(defun occ-get-property-by-category-from-filepath (filepath property &rest args)
  (apply 'occ-get-value-by-category-from-filepath filepath
         :property-fn (lambda () (org-entry-get (point) property)) args))

(defun occ-read-property-by-category-from-filepath (filepath property &rest args)
  (apply 'occ-get-value-by-category-from-filepath filepath
         :property-fn (lambda () (let ((p (org-entry-get (point) property)))
                                   (when p (read p)))) args))

(defun occ-get-categories-from-filepath (&rest args)
  (mapcar 'car (apply 'occ-get-value-by-category-from-filepath args)))

(defun occ-get-categories-from-headline (filepath headline)
  (occ-get-categories-from-filepath
   filepath :goto-subtree
   (lambda () (goto-char (org-find-exact-headline-in-buffer
                          headline (current-buffer) t)))))

(provide 'org-category-capture)
;;; org-category-capture.el ends here
