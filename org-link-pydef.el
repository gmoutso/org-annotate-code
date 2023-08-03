;;; org-link-pydef.el --- Support for links to python defintions in Org mode
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments, python

;;; Commentary:

;; This package provides links that point to python definitions.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:

(require 'ol)
(org-link-set-parameters "pydef"
                         :follow 'org-link-pydef-follow
                         :export #'org-link-pydef-export
                         :store #'org-link-pydef-store)

(defun org-link-pydef-project-root ()
  "Use one of `flycheck-python-project-files' or git to find root"
       (expand-file-name
	(or
	 (flycheck-python-find-project-root 'checker_)
	 (vc-root-dir))))

(defun org-link-pydef-relative-filename ()
  "Get filename relative to root. If in dired, return current line, else return buffer file."
  (let* ((pyroot (org-link-pydef-project-root))
	 (filename
	  (cond ((derived-mode-p 'dired-mode)
		 (dired-get-filename nil t))
		((buffer-file-name))
		((null (buffer-file-name))
		 (user-error "Current buffer is not associated with a file."))
		)))
    (file-relative-name filename pyroot)))

(defun org-link-pydef-get-pydef (&optional with-variable no-module absolute-path)
  (let* ((relative-filename (org-link-pydef-relative-filename))
	 (project-root (org-link-pydef-project-root))
	 (module (string-replace "/" "." relative-filename))
	 (module (replace-regexp-in-string "\\.py$" "" module))
	 (funname (python-info-current-defun))
	 (varname (save-excursion (python-nav-beginning-of-statement)
				  (python-info-current-symbol)))
	 (pydef-no-module (if with-variable
			      (cond ((not funname) varname)
				    ((not varname) funname)
				    ((concat funname "." varname)))
			    (or funname varname)
			    ))
	 (pydef-with-module (concat module "::" pydef-no-module))
	 (pydef (if no-module pydef-no-module (if absolute-path
						  (concat project-root pydef-with-module)
						pydef-with-module))))
    pydef
    ))

(defun org-link-pydef-store (&optional with-variable)
  "Store a link to a man page."
  (when (memq major-mode '(python-mode))
    (let* ((link (org-link-pydef-get-pydef with-variable nil t))
	   (description nil))
      (org-link-store-props
       :type "pydef"
       :link (concat "pydef:" link)
       :description description))))

(defun org-link-pydef-export (link)
  "Export LINK with DESCRIPTION into FORMAT."
  (let ((path link)
        (desc (or description link)))
    (pcase format
      (`html (format "%s" path))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ (if description (format "%s (%s)" desc path) path)))))

(defun org-link-pydef-split (link)
  "Return cons of file and dotted pydef from pydef LINK."
  (string-match "^\\(?1:.*\\)::\\(?2:.*\\)$" link)
  (cons (match-string-no-properties 1 link) (match-string-no-properties 2 link)))

(defun org-link-pydef-get-dotted-point (dotted)
  "Goto DOTTED definition."
  (save-excursion
    (let* ((names (split-string dotted "\\."))
	   (first-names (butlast names 1))
	   (last-name (car (last names)))
	   (beg (point-min))
	   (end (point-max)))
      (dolist (name first-names)
	(goto-char beg)
	(search-forward-regexp
	 (format "\\(?:def\\|class\\) *%s(" name) end)
	(save-excursion (python-nav-beginning-of-defun)
			(setq beg (point)))
	(save-excursion (python-nav-end-of-defun)
			(setq end (point))))
      (goto-char beg)
      (search-forward-regexp
       (format "\\(?:\\(?:def\\|class\\) *%s\\)(\\|\\(?:%s *=\\)" last-name last-name) end)
      (python-nav-beginning-of-statement)
      (point))))
    
(defun org-link-pydef-follow (link)
  (let* ((splitlink (org-link-pydef-split link))
	 (part1 (concat (string-replace "." "/" (car splitlink))
			".py"))
	 (part2 (cdr splitlink))
	 (project-root (org-link-pydef-project-root))
	 (filename part1)
	 (dotted part2))
    (if filename (find-file filename))
    (goto-char (org-link-pydef-get-dotted-point dotted))))

(provide 'org-link-pydef)
