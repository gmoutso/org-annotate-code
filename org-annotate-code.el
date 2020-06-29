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
(make-variable-buffer-local 'org-annotate-code-org-file)

(defcustom org-annotate-code-heading0 nil
  "The first heading in a three-level hierarchy, or nil for two levels."
  :type 'string
  :group 'org-annotate-code)
(make-variable-buffer-local 'org-annotate-code-heading0)

(defcustom org-annotate-code-info-alist '((python-mode . org-annotate-code-info-at-point-python))
  "Custom info parsers"
  :group 'org-annotate-code
  :type '(alist :key-type symbolp :value-type function))

(defcustom org-annotate-code-search-annotation-method 'org-annotate-code-search-annotation-reverse
  "Method to search and place annotation. Top-down strict hierarchy or bottom-up loose."
  :group 'org-annotate-code
  :type 'function
  :options '(org-annotate-code-search-annotation-reverse org-annotate-code-search-annotation-strict))

(defun org-annotate-code-add-heading0 (annotation)
  (if org-annotate-code-heading0 (conds (list :heading org-annotate-code-heading0) annotation) annotation))

(defun org-annotate-code-make-file-link (filename &optional search)
  (if search
      (org-link-make-string (concat "file:" filename "::" search))
      (org-link-make-string (concat "file:" filename)))
  
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
   (filename (buffer-file-name))
   )
  (let ((annotation
    (list
   (list :id (org-annotate-code-make-file-link filename)
	 :heading (file-name-base filename))
   (list :heading name
	 :id (org-annotate-code-make-file-link filename searchname)))))
    (org-annotate-code-add-heading0 annotation))))

(defun org-annotate-code-info-at-point-lineno ()
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
    (let ((annotation
    (list
     (list :id (org-annotate-code-make-file-link filename)
	   :heading module)
     (list :heading name
	   :id (org-annotate-code-make-file-link filename searchname)))))
    (org-annotate-code-add-heading0 annotation))))

(defun org-annotate-code-predicate-mode-cons (carcon)
  "Helper function to return cdr if mode is car"
(if (derived-mode-p (car carcon)) (cdr carcon)))
(defun org-annotate-code-choose-info-function ()
  "Return approprate function according to mode. See 'org-annotate-code-info-alist."
    (cond ((cl-some 'org-annotate-code-predicate-mode-cons org-annotate-code-info-alist))
     (t 'org-annotate-code-info-at-point-lineno)))

;;; Search or create functions
(defun org-annotate-code-search-heading (heading)
  "Searches for heading in (narrowed) buffer. Returns t if found and goes to position"
  (condition-case nil
  (let ((org-link-search-must-match-exact-headline t))
    (org-link-search heading)
    t)
  (error nil)
  ))

(defun org-annotate-code-search-id (id)
  "Searches for id in (narrowed) buffer. Returns t if found and goes to position"
  (condition-case nil
  (progn (org-link-search (concat "#" id)) t)
  (error nil)
  ))

(defun org-annotate-code-search-heading-or-id (node)
  "Search for id if set, otherwise heading, in node. Returns t if found and goes to position."
  (let ((id (plist-get node :id))
	(heading (plist-get node :heading)))
    (if id
	(org-annotate-code-search-id id)
      (org-annotate-code-search-heading heading))))

(defun org-annotate-code-search-annotation-strict (annotation)
  "Searches for nodes of annotation. Hierarchy is strict (unlike reverse search).

Places position at first success and returns non-existing sub-annotation of rest. Returns nil if all are found."
  (let* ((subannotation annotation)
	 (first (car subannotation)))
    (while (and (org-annotate-code-search-heading-or-id first) first)
      (org-narrow-to-subtree)  ;; to only find subtrees
      (setq subannotation (cdr subannotation)
	    first (car subannotation)))
    subannotation))

(defun org-annotate-code-search-annotation-reverse (annotation)
  "Searches for nodes of annotation in reverse option. 

Places position at first success and returns non-existing sub-annotation. Returns nil if bottom node is found."
  (let* ((subannotation)
	 (reversed (reverse annotation))
	 (last (car reversed))
	 (reversed (cdr reversed)))
    (while (and (not (org-annotate-code-search-heading-or-id last)) last)
      (setq subannotation (cons last subannotation)
	    last (car reversed)
	    reversed (cdr reversed)))
    subannotation))

(defun org-annotate-code-create-child-node (node)
  "Creates node as child at point at point."
  (let ((id (plist-get node :id))
	(heading (plist-get node :heading))
	(properties (plist-get node :properties)))
    (org-insert-subheading '(4))
    (insert heading)
    (if id (org-set-property "CUSTOM_ID"  id))
    (dolist (property properties)
	     (org-set-property (car property) (cdr property))
    )))

(defun org-annotate-code-create-sibling-node (node)
  "Creates node as child at point at point."
  (let ((id (plist-get node :id))
	(heading (plist-get node :heading))
	(properties (plist-get node :properties)))
    (org-insert-heading '(4))
    (insert heading)
    (if id (org-set-property "CUSTOM_ID"  id))
    (dolist (property properties)
	     (org-set-property (car property) (cdr property))
    )))

(defun org-annotate-code-create-toplevel-node (node)
  "Creates node as toplevel."
  (let ((id (plist-get node :id))
	(heading (plist-get node :heading))
	(properties (plist-get node :properties)))
    (goto-char (point-max))
    (unless (bolp) (newline))
    (insert "* " heading)
    (beginning-of-line)
    (if id (org-set-property "CUSTOM_ID"  id))
    (dolist (property properties)
	     (org-set-property (car property) (cdr property))
    )))

(defun org-annotate-code-goto-entry-text()
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (unless (org-at-heading-p) ; we might have moved to the next heading
    (outline-next-heading))  ; if not, ensure we are at a heading
  ;; that usually takes us to bol if there were more headings
  ;; end of metadata might take us to an eol. ditto if no more headings
  (if (eolp) (newline)  
    ;; assume at the next heading
    (beginning-of-line) ; must be true anyway..
    (newline)  ; note org-capture might remove new lines
    (previous-line)  ; back to empty space
  ))

(defun org-annotate-code-create-subannotation (subannotation)
  "Creates nodes as a subannotation at point. Places position in last."
  (dolist (node subannotation)
    (if (org-annotate-code-search-heading-or-id node)
      (user-error (format "Node exists %s" node)))
    (org-annotate-code-create-child-node node)
    ))

(defun org-annotate-code-create-annotation (annotation)
  "Creates nodes in annotation from top. Places position in last."
  (let ((toplevel (car annotation))
	(subannotation (cdr annotation)))
    (widen)
    (beginning-of-buffer)
    (if (org-annotate-code-search-heading-or-id toplevel)
	(user-error (format "Node exists %s" toplevel)))
    (org-annotate-code-create-toplevel-node toplevel)
    (org-annotate-code-create-subannotation subannotation)
    ))


(defun org-annotate-code-search-or-create-child-node (node &optional create)
  "Searches for a heading by its custom id, or heading if id not set. If not found, it can create it.

Returns t if it was found or created. Returns nil if not found and not created."
  (let (found (org-annotate-code-search-heading-or-id))
    (if (and (not found) create)
	(org-annotate-code-create-child-node node))))

(defun org-annotate-code-search-and-create-levels (annotation &optional heading)
  "Locate or insert org-mode levels. Places point at bottom node.

The current method is to first search bottom-top for ids (or headings if not set). The bottom non-existing nodes are created from the first existing node. This way, subtrees can be moved.
It also means that the bottom node is the only significant entry when it exists, as the search stops there. Alternative is to use org-annotate-code-search-annotation. TBD."
(let* ((annotation (if heading (cons (list :heading heading) annotation) annotation))
	(subannotation (org-annotate-code-search-annotation-method annotation)))
  (if (equal annotation subannotation)
      (org-annotate-code-create-annotation annotation)
      (org-annotate-code-create-subannotation subannotation)
      )
  (org-annotate-code-goto-entry-text)))


;;; User interaction
;;;###autoload
(defun org-annotate-code-capture-finding-location (&optional org-file)
  "To be used in capture templates."
  (let*  ((org-file (or org-file org-annotate-code-org-file))
	 (the-buffer (org-capture-target-buffer org-file))
	 (annotation (funcall (org-annotate-code-choose-info-function)))
	 )
    (set-buffer the-buffer)
    (org-annotate-code-search-and-create-levels annotation)
    ))


;;;###autoload
(defun org-annotate-code-create (&optional org-file)
  "Has to be called from the annotated file. Creates and visits."
  (interactive)
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (the-buffer (find-file-noselect org-file))
	 (annotation (funcall (org-annotate-code-choose-info-function)))
	 )
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-and-create-levels annotation)
    ))


;;;###autoload
(defun org-annotate-code-visit-org-file (&optional org-file)
  "Visits org-file in the relevant section of current buffer. It does not create levels.

It stops at the outer-most level that already exists. This might stop at the org-file itself, if the heading0 should exist but does not.  
It might subsequently stop at the level1 file section, if that subsequently exists but not the level2."
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (the-buffer (find-file-noselect org-file))
	 (annotation (funcall (org-annotate-code-choose-info-function)))
	 )
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-annotation-method annotation)))

(provide 'org-annotate-code)

;; annotation-mode (gets updated)
;; (("/home/moutsopoulosg/dev/dotemacs/lisp/org-annotate-code.el" ((8960 8965 "stops is here" "stops")) "a54424ef63923af57d6c9b6a0cd4fff3")
;; bookmark
;; (("visits is here"
;;  (filename . "~/dev/dotemacs/lisp/org-annotate-code.el")
;;  (front-context-string . "isits org-file i")
;;  (rear-context-string . "e heading0)\n  \"V")
;;  (position . 8871))

