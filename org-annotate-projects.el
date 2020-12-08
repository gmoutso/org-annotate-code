;;; org-annotate-projects.el --- Annotate project folders using org-capture
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides a function-finding-location for org-mode capture.
;; In any file or dired buffer, capturing will insert at the root of workspace in an org file
;; a hierarchy of nodes with CUSTOM_ID and DIR properties pointing to the folders

;;; Code:
(require 'projectile)
(defcustom org-annotate-projects-root nil
  "Root of project. If nil then find using projectile.")

(defcustom org-annotate-projects-org-file "projects.org" "Org file for projects")

(defun org-annotate-projects-get-project-root ()
  (or org-annotate-projects-root (projectile-project-root)))

(defun org-annotate-projects-get-org-file ()
  (concat (org-annotate-projects-get-project-root) "/" org-annotate-projects-org-file))


(defun org-annotate-projects-get-folder ()
  "Return filename at point with no directory."
  (let ((this-dir (file-name-directory
		      (cond ((derived-mode-p 'dired-mode)
			     (dired-get-filename nil t))
			    ((buffer-file-name)
			     (buffer-file-name))
			    (t
			     (buffer-file-name))))))
    (expand-file-name (read-directory-name "Add folder to projects: "  this-dir)
		      (org-annotate-projects-get-project-root))))

(defun org-annotate-code-cumulate-mapconcat (sequence seperator)
  "On the SEQUENCE of strings (a b c) creates (a a/b a/b/c)."
  (let ((candidates (list))
	(last nil))
    (dolist (leaf sequence candidates)
	(setq last (if last (concat last seperator leaf) leaf))
	(setq candidates (cons last candidates)))
    (reverse candidates)))

(defun org-annotate-projects-cumulate-mapconcat (sequence)
  (org-annotate-code-cumulate-mapconcat sequence "/"))

(defun org-annotate-projects-info-at-point ()
  (let ((wspfolder (org-annotate-projects-get-folder)))
      (mapcar (lambda (node) (list :id (org-link-make-string (concat "file:" node))
			       :heading (car (last (split-string node "/")))
			       :properties (list (cons "DIR" (org-link-make-string (concat "file:" node))))))
	  (org-annotate-projects-cumulate-mapconcat (split-string wspfolder "/")))))


(defun org-annotate-projects-capture-finding-location ()
  "To be used in capture templates."
  (let ((org-annotate-code-org-file (org-annotate-projects-get-org-file))
	(org-annotate-code-add-heading0 nil)
	(org-annotate-code-info-override 'org-annotate-projects-info-at-point))
    (org-annotate-code-capture-finding-location)))


(defun org-annotate-projects-create ()
  (interactive)
  (let ((org-annotate-code-org-file (org-annotate-projects-get-org-file))
	(org-annotate-code-add-heading0 nil)
	(org-annotate-code-info-override 'org-annotate-projects-info-at-point))
    (org-annotate-code-create)))

(defun org-annotate-projects-visit-org-file ()
  (interactive)
  (let ((org-annotate-code-org-file (org-annotate-projects-get-org-file))
	(org-annotate-code-add-heading0 nil)
	(org-annotate-code-info-override 'org-annotate-projects-info-at-point))
  (org-annotate-code-visit-org-file)))

(provide 'org-annotate-projects)
;;; org-annotate-projects.el ends here
