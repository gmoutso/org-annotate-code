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

(defun org-link-pydef-relative-filename (&optional absolute-path)
  "Get filename relative to root. If in dired, return current line, else return buffer file."
  (let* ((pyroot (expand-file-name
		  (or (file-name-concat (vc-root-dir) "..")
		      (flycheck-python-find-project-root 'checker_))))
	 (filename
	  (cond ((derived-mode-p 'dired-mode)
		 (dired-get-filename nil t))
		((buffer-file-name))
		((null (buffer-file-name))
		 (user-error "Current buffer is not associated with a file."))
		)))
    (if absolute-path filename (file-relative-name filename pyroot))))

(defun org-link-pydef-get-pydef (&optional with-variable no-module absolute-path)
  (let* ((filename (org-link-pydef-relative-filename absolute-path))
	 (dotted-filename (string-replace "/" "." filename))
	 (module (replace-regexp-in-string "\\.py$" "" dotted-filename))
	 (module (replace-regexp-in-string "^.*python\\.ev\\." "ev." module))
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
	 (pydef (if no-module pydef-no-module pydef-with-module)))
    pydef
    ))

(defun org-link-pydef-store (&optional with-variable without-file relative-path)
  "Store a link to a man page."
  (when (memq major-mode '(python-mode))
    (let* ((link (org-link-pydef-get-pydef with-variable without-file (not relative-path)))
	   (description nil))
      (org-link-store-props
       :type "pydef"
       :link link
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
	(goto beg)
	(search-forward-regexp
	 (format "\\(?:def\\|class\\) *%s\\)(" name) end)
	(save-excursion (setq beg (python-nav-beginning-of-defun)))
	(save-excursion (setq end (python-nav-end-of-defun))))
      (goto beg)
      (search-forward-regexp
       (format "\\(?:\\(?:def\\|class\\) *%s\\)(\\|\\(?:%s *=\\)" last-name) end)
      (python-nav-beginning-of-statement)
      (point))))
    
(defun org-link-pydef-follow (link)
  (let* ((splitlink (org-link-pydef-split link))
	 (filename (car splitlink))
	 (dotted (cdr splitlink)))
    (if filename (find-file filename))
    (goto (org-link-pydef-get-dotted-point dotted))))

(provide 'org-link-pydef)
