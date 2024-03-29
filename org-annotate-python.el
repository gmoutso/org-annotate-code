;;; org-annotate-python.el --- Support for links to python defintions in Org mode
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments, python

;;; Commentary:

;; This package provides links that point to python definitions.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:
(require 'org-annotate-code)
(require 'ol)
(require 'org-link-pydef)

(defconst org-annotate-code-level-regex
  "^\\(?: {%d}\\)"
  "Block level regex format string.")
(defconst org-annotate-code-def-regex
  "\\(?:\\(?2:\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)\\)(\\)(\\|\\(?:\\(?2:\\(?1:[[:alnum:]_]*\\) *=\\)\\)"
  "Class, def or variable definition regex.")
(defconst org-annotate-code-function-regex
  "\\(?2:\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)\\)("
  "Class or def definition.")
(defconst org-annotate-code-variable-regex
  "\\(?2:\\(?1:[[:alnum:]_]*\\) *=\\)"
  "Class or def definition.")
(defconst org-annotate-code-def-regex-name-format
  "\\(?:\\(?:def\\|class\\) *%s\\)(\\|\\(?:%s *=\\)"
  "Class, def or variable definition regex.")
(defconst org-annotate-code-link-regex
  "^\\(?1:[[:alnum:]\\./_-]*\\)::\\(?2:[[:alnum:]_\\.]*\\)$"  ; [^<>\\:;,\\?\"\\*|/]
  ;; "\\(?:\\(?1:[[^<>:;,\\?\"\\*|/]]*\\)::\\)?\\(?2:[[:alnum:]_\\.]*\\)"  ;  [[:alnum:]_\\./]*
  "Link regex eg dir/filename.py::a.b.c")
(defcustom org-annotate-python-squash-candidates-level nil
  "Present candidates up to level. It does not include the heading0.

If nil then show all candidates.
Here zero can be used to only keep filename node.
If 0 then only filename node will be used and present no candidates,
if 1 then only top-level functions will be used and present no candidates.
If 2 then a pydef a.b.c presents candidates a and a.b."
  :type 'integer
  :group 'org-annotate-python)

(defcustom org-annotate-python-squash-annotation-level nil
  "Squash annotation to these n level nodes per file node.
They are the first n-1 nodes plus the last one.

It does not include the heading0 and the filename in the counting.
However zero can be used to force keep the filename node only.

This squash happens after candidate is selected.
If nil then keep all. If 2 and a.b.c was selected then a.b.c becomes annotation
filename->a->a.b.c
possibly under a heading0."
  :group 'org-annotate-python
  :type 'integer)
(make-variable-buffer-local 'org-annotate-python-squash-annotation-level)

(defun org-annotate-python-get-defun-name ()
  "Return python function name. Is dotted if it is a subfunction."
  (python-info-current-defun))

(defun org-annotate-python-get-variable-name ()
  "Return variable defintion at point."
  (save-excursion
  (let ((beg (progn (python-nav-beginning-of-statement) (point)))
	(end (progn (python-nav-end-of-statement) (point))))
    (goto-char beg)
    (if (search-forward-regexp org-annotate-code-variable-regex end t)
    (match-string-no-properties 1)))))



(defun org-annotate-python-squash-list-to-level (listnames level)
  "Squash candidates in LISTNAMES up to LEVEL.

If nil then all. Eg. with level 2, (a b c) becomes (a b)."
 (if level (seq-take listnames level) listnames))

(defun org-annotate-python-squash-list-keep-and-last (listnames level)
  "Squash LISTNAMES to required LEVEL.
The first level-1 items appear and the last one.

If nil then all. Eg. with level 1, (a b c) becomes (a c)"
 (if level (append (seq-take listnames (1- level)) (last listnames)) listnames))

(defun org-annotate-python-make-list-from-dotted (dotted &optional squash)
"Make cadidates in DOTTED name, eg a.b.c become '(\"a\" \"a.b\" \"a.b.c.\")."
    (let* ((listname (split-string dotted "\\."))
	   (candidates (list))
	   (last nil))
      (dolist (leaf listname candidates)
	(setq last (if last (concat last "." leaf) leaf))
	(setq candidates (cons last candidates)))
      (org-annotate-python-squash-list-to-level (reverse candidates) squash)))  ;squash list of candidates

(defun org-annotate-python-pydef-select-candidate (dotted &optional squash)  ; what if dotted is nil!
  (let* ((candidates (org-annotate-python-make-list-from-dotted dotted squash))
	(levels (length candidates)))
    (cond ((equal levels 1) (first candidates))
	  ((equal levels 0) nil)
	  ((completing-read "Select pydef: " candidates)))))

(defun org-annotate-python-pydef-split-filename-searchname (link)
  "Return cons of file and dotted pydef from pydef LINK."
  (string-match org-annotate-code-link-regex link)
  (cons (match-string-no-properties 1 link) (match-string-no-properties 2 link)))

(defun org-annotate-python-mapcar-dotted-to-node (filename listdotted)
  (mapcar (lambda (dotted) (list :id (org-link-make-string (concat "pydef:" filename "::" dotted))
				 :heading dotted))
	  listdotted))

(defcustom org-annotate-python-add-filename-node-use-projectile t
  "Use projectile to make relative path in filename heading.")
(defcustom org-annotate-python-add-filename-node-remove-directory nil
  "Remove directory in filename heading.")
(defcustom org-annotate-python-add-filename-node-make-folders-dotted t
  "Use projectile to make dotted path in filename heading.")

(defun org-annotate-python-add-filename-node (filename annotation)
  "Add the FILENAME as top-level node to ANNOTATION."
  (let* ((filename (expand-file-name filename))
	 (id (org-annotate-code-make-file-link filename))
	 (directory (file-name-directory filename))
	 (heading (file-name-sans-extension filename))
	 filenode)
    (if (and org-annotate-python-add-filename-node-use-projectile
	     (featurep 'projectile)
	     (not org-annotate-python-add-filename-node-remove-directory)
	     (projectile-project-root directory))
	(setq heading (file-relative-name heading (projectile-project-root directory))))
    (if org-annotate-python-add-filename-node-remove-directory
	(setq heading (file-name-nondirectory heading)))
    (if org-annotate-python-add-filename-node-make-folders-dotted
	(setq heading (replace-regexp-in-string "/" "." heading)))
    (setq filenode
	(list :id id
	      :heading heading))
    (cons filenode annotation)))

(defun org-annotate-python-make-annotation-from-pydef (filename dotted &optional squash)
  "Make annotation from dotted pydef.

Optional squash for final annotation, if nil keep all, if zero keeps only filename, etc."
  (let* ((levels (org-annotate-python-make-list-from-dotted dotted)) ; note selection was made before
	 (dottedannotation (org-annotate-python-mapcar-dotted-to-node filename levels))
	 (annotation (org-annotate-python-add-filename-node filename dottedannotation)))
    (org-annotate-python-squash-list-keep-and-last annotation (when squash (1+ squash))))) ; here squash=0 means keeping only filename.




(defun org-annotate-python-make-search-string (name)
  (format org-annotate-code-def-regex-name-format name name))

(defun org-annotate-python-goto-dotted (dotted)
  "Goto DOTTED definition."
  (let* ((names (split-string dotted "\\."))
	(beg (point-min))
	(end (point-max))
	(name (car names))
	(last (point)))
    (goto-char beg)
    (while name
      (search-forward-regexp (org-annotate-python-make-search-string name) end)
      (setq last (point))
      (python-nav-beginning-of-defun)
      (setq beg (point))
      (python-nav-end-of-defun)
      (setq end (point))
      (setq names (cdr names)
	    name (car names))
      (goto-char beg))
    (goto-char last)))

(defun org-annotate-python-info-at-point (&optional ask)
  "Return a plist with python info at point."
  (interactive "P")
  (let* ((filename (buffer-file-name))
	 (dotted (org-link-pydef-get-pydef t t))
	 (selection (if ask
			(org-annotate-python-pydef-select-candidate
			 dotted
			 org-annotate-python-squash-candidates-level)
		      dotted)))
    (org-annotate-python-make-annotation-from-pydef filename selection org-annotate-python-squash-annotation-level)))


(add-to-list 'org-annotate-code-info-alist (cons 'python-mode 'org-annotate-python-info-at-point))

(defun org-annotate-python-capture-finding-location ()
  (let ((org-annotate-code-info-override 'org-annotate-python-info-at-point))
    (org-annotate-code-capture-finding-location)))

(provide 'org-annotate-python)
;;; org-annotate-python.el ends here

