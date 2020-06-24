;;; org-annotate-code.el --- Annotate code using org-capture
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides a function-finding-location for org-mode capture.
;; In a source code buffer, capturing will insert two headings if not already there.
;; The top-level heading is a link to the file and the second a link to the function definition at point.
;; A line can be inserted with the line or word at point.

(require 'org)

;;; Customization
(defgroup org-annotate-code nil "Org-annotate-code")
(defcustom org-annotate-code-org-file "~/Documents/org/code-annotations.org"
  "The org file where annotations are saved."
  :type 'string
  :group 'org-annotate-code)
(defcustom org-annotate-code-heading0 nil
  "The first heading in a three-level hierarchy, or nil for two levels."
  :type 'string
  :group 'org-annotate-code)
(defcustom org-annotate-code-info-alist '((python-mode . org-annotate-code-info-at-point-python))
  "Custom info parsers"
  :group 'org-annotate-code
  :type '(alist :key-type symbolp :value-type function))

;;; Info functions
(defun org-annotate-code-info-at-point-python ()
  "Returns a plist with python info at point. 

The name used is the last def, class, or variable definition after one has moved up to beginning of defun.
It is referenced with a definition search, eg 'def aname' for name 'aname'."
  (interactive)
  (let*
  ((searchmatch 
  (save-excursion
    (python-nav-beginning-of-defun)
    (search-forward-regexp "^ *\\(?:\\(?2:\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)\\)(\\|\\(?2:\\(?1:[[:alnum:]_]*\\) *=\\)\\)" (line-end-position))
    ))
   (searchname (match-string-no-properties 2))
   (name (match-string-no-properties 1))
   )
  (list :name name
    :filename (buffer-file-name)
    :module (file-name-base (buffer-file-name))
    :searchname searchname
    )))

(defun org-annotate-code-info-at-point-unknown ()
  "Returns a plist with generic info at point.

The name used is the symbol at point, but it is referenced by its line number only."
  (let* ((filename (buffer-file-name))
	 (module (file-name-nondirectory filename))
	 (lineno (line-number-at-pos))
	 (searchname (format "%s" lineno))
	 (symbol (thing-at-point 'symbol t))
	 (name (if symbol
		   (format "%s at line %s" symbol lineno)
		 (format "line %s" lineno))))
  (list :filename filename
    :module module
    :searchname searchname
    :name name
  )))

(defun org-annotate-code-predicate-mode-cons (carcon)
  "Helper function to return cdr if mode is car"
(if (derived-mode-p (car carcon)) (cdr carcon)))
(defun org-annotate-code-choose-info-function ()
  "Return approprate function according to mode. See 'org-annotate-code-info-alist."
    (cond ((cl-some 'org-annotate-code-predicate-mode-cons org-annotate-code-info-alist))
     (t 'org-annotate-code-info-at-point-unknown)))

;;; Search or create functions
(defun org-annotate-code-search-or-create-level0 (level &optional create)
  "Searches for a fuzzy heading and if not found, it can create it.

Returns t if it was found or created. Returns nil if not found and not created."
  (condition-case nil
  (let ((org-link-search-must-match-exact-headline t))
    (org-link-open-from-string (org-make-link-string level))
    t)
  (error
   (when create
   (goto-char (point-max))
   (unless (bolp) (newline))
   (org-insert-heading nil t t)
   (insert level "\n")
   (beginning-of-line 0)
   t))))

(defun org-annotate-code-search-or-create-level (heading id &optional create)
  "Searches for a heading by its custom id and if not found, it can create it.

Returns t if it was found or created. Returns nil if not found and not created."
  (condition-case nil
      (progn (org-link-search (concat "#" id))
	     t)
    (error
     (when create
     (goto-char (point-min))
     (org-insert-heading '(4))
     (insert heading)
     (org-set-property "CUSTOM_ID"  id)
     t))))


(defun org-annotate-code-search-or-create-level1 (heading1 link1 &optional heading0)
  "Locate or insert org-mode heading levels 1. Return position of second level."
  (widen)
  (goto-char (point-min))
  ;; insert heading0 if set
  (when heading0
    (org-annotate-code-search-or-create-level0 heading0 t)
    (org-narrow-to-subtree))
  ;; insert first
  (org-annotate-code-search-or-create-level heading1 link1 t)
  (widen)
  (point)
  )

(defun org-annotate-code-search-or-create-levels (heading1 link1 heading2 link2 &optional heading0)
  "Locate or insert org-mode heading levels 1 and 2. Return position of second level."
  (org-annotate-code-search-or-create-level1 heading1 link1 heading0)
  ;; insert second
  (org-narrow-to-subtree)
  (org-annotate-code-search-or-create-level heading2 link2 t)
  (org-demote)
  (org-narrow-to-subtree)
  (goto-char (point-max))
  (unless (bolp) (newline))
  (widen)
  (point)
  )

(defun org-annotate-code-search-levels (heading1 link1 heading2 link2 &optional heading0)
  "Locate org-mode heading levels 1 and 2. Do not create if non-existing.
   Stop and return position of outer-most existing level."
  (widen)
  (goto-char (point-min))
  ;; find heading0 if set
  (when (if heading0
	    (when (org-annotate-code-search-or-create-level0 heading0 nil)
	      (org-narrow-to-subtree)
	      t) ; t if heading0 found
	  t) ; t if no heading0
  ;; find first
  (when (org-annotate-code-search-or-create-level heading1 link1 nil)  ; t if found
  ;; find second
  (org-narrow-to-subtree)
  (org-annotate-code-search-or-create-level heading2 link2 nil))
  (widen)
  (point)))

;;; User interaction
;;;###autoload
(defun org-annotate-code-capture-finding-location (&optional org-file heading0)
  "To be used in capture templates."
  (let  ((org-file (or org-file org-annotate-code-org-file))
	 (heading0 (or heading0 org-annotate-code-heading0))
	 (the-buffer (org-capture-target-buffer org-file))
	 (plist (funcall (org-annotate-code-choose-info-function)))
	 (filename (plist-get plist :filename))
	 (name (plist-get plist :name))
	 (module (plist-get plist :module))
	 (searchname (plist-get plist :searchname))
	 (link1 (org-link-make-string (concat "file:" filename)))
	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
	 )
    (set-buffer the-buffer)
    (org-annotate-code-search-or-create-levels module link1 name link2 heading0)
    ))

;; (defun org-annotate-code-create-in-buffer (the-buffer &optional heading0)
;;   "Has to be called from the annotated file."
;;   (let* (;(org-file (or org-file org-annotate-code-org-file))
;; 	 (heading0 (or heading0 org-annotate-code-heading0))
;; 	 ;(the-buffer (find-file-noselect org-file))
;; 	 (plist (funcall (org-annotate-code-choose-info-function)))
;; 	 (filename (plist-get plist :filename))
;; 	 (name (plist-get plist :name))
;; 	 (module (plist-get plist :module))
;; 	 (searchname (plist-get plist :searchname))
;; 	 (link1 (org-link-make-string (concat "file:" filename)))
;; 	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
;; 	 )
;;     (with-current-buffer the-buffer
;;     (org-annotate-code-search-or-create-levels module link1 name link2 heading0))))

;;;###autoload
(defun org-annotate-code-create (&optional org-file heading0)
  "Has to be called from the annotated file. Creates and visits."
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (heading0 (or heading0 org-annotate-code-heading0))
	 (the-buffer (find-file-noselect org-file))
	 (plist (funcall (org-annotate-code-choose-info-function)))
	 (filename (plist-get plist :filename))
	 (name (plist-get plist :name))
	 (module (plist-get plist :module))
	 (searchname (plist-get plist :searchname))
	 (link1 (org-link-make-string (concat "file:" filename)))
	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
	 )
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-or-create-levels module link1 name link2 heading0)))

;;;###autoload
(defun org-annotate-code-visit-file (&optional org-file heading0)
  "Creates and visits level1 file section only."
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (heading0 (or heading0 org-annotate-code-heading0))
	 (the-buffer (find-file-noselect org-file))
	 (plist (funcall (org-annotate-code-choose-info-function)))
	 (filename (plist-get plist :filename))
	 (name (plist-get plist :name))
	 (module (plist-get plist :module))
	 (searchname (plist-get plist :searchname))
	 (link1 (org-link-make-string (concat "file:" filename)))
	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
	 )
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-or-create-level1 module link1 heading0)))

;;;###autoload
(defun org-annotate-code-visit-org-file (&optional org-file heading0)
  "Visits org-file in the relevant section of current buffer. It does not create levels.

It stops at the outer-most level that already exists. This might stop at the org-file itself, if the heading0 should exist but does not.  
It might subsequently stop at the level1 file section, if that subsequently exists but not the level2."
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (heading0 (or heading0 org-annotate-code-heading0))
	 (the-buffer (find-file-noselect org-file))
	 (plist (funcall (org-annotate-code-choose-info-function)))
	 (filename (plist-get plist :filename))
	 (name (plist-get plist :name))
	 (module (plist-get plist :module))
	 (searchname (plist-get plist :searchname))
	 (link1 (org-link-make-string (concat "file:" filename)))
	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
	 )
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-levels module link1 name link2 heading0)))



(provide 'org-annotate-code)

