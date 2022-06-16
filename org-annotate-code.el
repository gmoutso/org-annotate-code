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

;;; Code:
(require 'org)
(require 'ol)
(require 'org-capture)

;;; Customization
(defgroup org-annotate-code nil "Org-Annotate-code")

(defcustom org-annotate-code-org-file "~/Documents/org/code-annotations.org"
  "The org file where annotations are saved."
  :type 'string
  :group 'org-annotate-code)
(make-variable-buffer-local 'org-annotate-code-org-file)

(defcustom org-annotate-code-heading0 nil
  "A top-level heading0. nil means use only code annotation."
  :type 'string
  :group 'org-annotate-code)
(make-variable-buffer-local 'org-annotate-code-heading0)

(defcustom org-annotate-code-info-alist nil
  "Custom annotation parsers per mode. An alist of mode to function."
  :group 'org-annotate-code
  :type '(alist :key-type symbolp :value-type function))

(defcustom org-annotate-code-info-blacklist nil
  "Custom annotation parsers per mode. An alist of mode to function."
  :group 'org-annotate-code
  :type 'list)

(defcustom org-annotate-code-info-default 'org-annotate-code-info-at-point-lineno 
  "Default annotation parser. It is filename -> line number."
  :group 'org-annotate-code
  :type 'function)

(defcustom org-annotate-code-info-override nil
  "A parser that will override mode choices and default."
  :type 'function
  :group 'org-annotate-code)
(make-variable-buffer-local 'org-annotate-code-info-override)

(defcustom org-annotate-code-search-annotation-method 'org-annotate-code-search-annotation-reverse
  "Method to search for annotation and create if not found. Top-down strict hierarchy or bottom-up loose."
  :group 'org-annotate-code
  :type 'function
  :options '(org-annotate-code-search-annotation-reverse org-annotate-code-search-annotation-strict))

(defun org-annotate-code-add-heading0 (annotation)
  (if org-annotate-code-heading0 (cons (list :heading org-annotate-code-heading0) annotation) annotation))

(defun org-annotate-code-wrap-id (id)
  "Wrap ID with brackets or not."
  (org-link-make-string id))

(defun org-annotate-code-make-file-link (filename &optional search)
  (org-annotate-code-wrap-id (if search
       (concat "file:" filename "::" search)
       (concat "file:" filename))))

(defun org-annotate-code-add-filename-node (filename annotation)
  "Add the FILENAME as top-level node to ANNOTATION."
  (let* ((filename (expand-file-name filename))
	 (id (org-annotate-code-make-file-link filename))
	 (heading (file-name-nondirectory filename))
	 filenode)
    (setq filenode
	(list :id id
	      :heading heading))
    (cons filenode annotation)))

;;; Info functions
(defun org-annotate-code-info-at-point-lineno ()
  "Return an annotation filename -> linenumber. Annotations are list of node plists."
  (let* ((filename (buffer-file-name))
	 (lineno (line-number-at-pos))
	 (searchname (format "%s" lineno))
	 (symbol (thing-at-point 'symbol t))
	 (name (if symbol
		   (format "%s at line %s" symbol lineno)
		 (format "line %s" lineno))))
    (list
     (list :id (org-annotate-code-make-file-link filename)
	   :heading (file-name-nondirectory filename))
     (list :heading name
	   :id (org-annotate-code-make-file-link filename searchname)))))

(defun org-annotate-code-predicate-mode-cons (carcon)
  "Helper function to return cdr if mode is car in CARCON."
(if (derived-mode-p (car carcon)) (cdr carcon)))

(defun org-annotate-code-choose-info-function ()
  "Return approprate function according to mode, default or override. See 'org-annotate-code-info-alist."
  (cond (org-annotate-code-info-override)
	((member major-mode org-annotate-code-info-blacklist)
	 (lambda () nil))
	 ((cl-some 'org-annotate-code-predicate-mode-cons org-annotate-code-info-alist))
	 (org-annotate-code-info-default)))

(defun org-annotate-code-search-heading (heading)
  "Search for HEADING in (narrowed) buffer. Return t if found and go to position."
  (condition-case nil
  (let ((org-link-search-must-match-exact-headline t))
    (org-link-search heading)
    t)
  (error nil)))

(defun org-annotate-code-search-id (id)
  "Search for ID in (narrowed) buffer. Return t if found and go to position."
  (condition-case nil
  (progn (org-link-search (concat "#" id)) t)
  (error nil)
  ))

(defun org-annotate-code-search-heading-or-id (node)
  "Search for ID if set, otherwise heading, in NODE. Return t if found and go to position."
  (let ((id (plist-get node :id))
	(heading (plist-get node :heading)))
    (if id
	(org-annotate-code-search-id id)
      (org-annotate-code-search-heading heading))))

(defun org-annotate-code-search-annotation-strict (annotation)
  "Searche for nodes of ANNOTATION. Hierarchy is strict (unlike reverse search).

Places position at first success and returns non-existing sub-annotation of rest. Returns nil if all are found."
  (let* ((subannotation annotation)
	 (first (car subannotation)))
    (while (and (org-annotate-code-search-heading-or-id first) first)
      (org-narrow-to-subtree)  ;; to only find subtrees
      (setq subannotation (cdr subannotation)
	    first (car subannotation)))
    subannotation))

(defun org-annotate-code-search-annotation-reverse (annotation)
  "Searche for nodes of ANNOTATION in reverse option.

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
  "Create NODE as child at point."
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
  "Create NODE as child at point at point."
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
  "Create NODE as toplevel."
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

(defun org-annotate-code-goto-entry-text ()
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
    (forward-line -1)  ; back to empty space
  ))

(defun org-annotate-code-create-subannotation (subannotation)
  "Create nodes as a SUBANNOTATION from org level at point. Places position in last."
  (dolist (node subannotation)
    (if (org-annotate-code-search-heading-or-id node)
      (user-error (format "Node exists %s" node)))
    (org-annotate-code-create-child-node node)
    ))

(defun org-annotate-code-create-annotation (annotation)
  "Create nodes in ANNOTATION from top. Places position in last."
  (let ((toplevel (car annotation))
	(subannotation (cdr annotation)))
    (widen)
    (goto-char (point-min))
    (if (org-annotate-code-search-heading-or-id toplevel)
	(user-error (format "Node exists %s" toplevel)))
    (org-annotate-code-create-toplevel-node toplevel)
    (org-annotate-code-create-subannotation subannotation)
    ))


(defun org-annotate-code-search-or-create-child-node (node &optional create)
  "Search for a heading by the NODE's custom id, or heading if id not set.
If not found and CREATE is true then create it.

Return t if it was found or created. Return nil if not found and not created."
  (let (found (org-annotate-code-search-heading-or-id))
    (if (and (not found) create)
	(org-annotate-code-create-child-node node))))

(defun org-annotate-code-search-and-create-levels (annotation &optional heading)
  "Locate or insert `org-mode' levels. Places point at bottom node.

The current method is to first search bottom-top for ids (or headings).
The bottom non-existing nodes are created from the first existing node.
This way, subtrees can be moved.

It also means that the bottom node is the only significant entry when it exists,
as the search stops there.

Alternative is to use `org-annotate-code-search-annotation'."
(let* ((annotation (if heading (cons (list :heading heading) annotation) annotation))
	(subannotation (funcall org-annotate-code-search-annotation-method annotation)))
  (if (equal annotation subannotation)
      (org-annotate-code-create-annotation annotation)
      (org-annotate-code-create-subannotation subannotation))
  (org-annotate-code-goto-entry-text)))


;;; User interaction
;;;###autoload
(defun org-annotate-code-capture-finding-location ()
  "To be used in capture templates."
  (let*  ((org-file org-annotate-code-org-file)
	  (the-buffer (org-capture-target-buffer org-file))
	  (info-function (org-annotate-code-choose-info-function))
	  (annotation (funcall info-function))
	  (annotation1 (org-annotate-code-add-heading0 annotation)))
    (set-buffer the-buffer)
    (if annotation 
	(org-annotate-code-search-and-create-levels annotation1)
      (user-error (format "%s: No annotation possible." info-function)))
    ))


;;;###autoload
(defun org-annotate-code-create ()
  "Has to be called from the annotated file. Create and visit."
  (interactive)
  (let* ((org-file org-annotate-code-org-file)
	 (the-buffer (find-file-noselect org-file))
	 (annotation (funcall (org-annotate-code-choose-info-function)))
	 (annotation (org-annotate-code-add-heading0 annotation)))
    (switch-to-buffer the-buffer)
    (org-annotate-code-search-and-create-levels annotation)))


;;;###autoload
(defun org-annotate-code-visit-org-file (&optional org-file)
  "Visit org-file in the relevant section of current buffer.

It does not create levels.

It stops at the outer-most level that already exists.
This might stop at the ORG-FILE."
  (interactive)
  (let* ((org-file (or org-file org-annotate-code-org-file))
	 (the-buffer (find-file-noselect org-file))
	 (annotation (funcall (org-annotate-code-choose-info-function)))
	 (annotation (org-annotate-code-add-heading0 annotation)))
    (switch-to-buffer the-buffer)
    (funcall org-annotate-code-search-annotation-method annotation)))

(defun org-annotate-code-parse-link-from-string (s)
  "Return plist by parsing string S. 

See eg `org-open-link-from-string'. 
Useful properties are :type and :raw-link.
"
  (with-temp-buffer
	   (let ((org-inhibit-startup nil))
	     (insert s)
	     (org-mode)
	     (goto-char (point-min))
	     (cdr (org-element-link-parser)))))

(defun org-annotate-code-get-link-type (link)
  "Get type of link"
  (if (string-match "\\(?1:[[:alnum:]]*\\):.*" link)
  (match-string-no-properties 1 link)))

(defun org-annotate-code-get-filename (link)
  (if (string-match "\\(?1:[-[:alnum:]_\\./]*\\)::.*" link)
      (expand-file-name (match-string-no-properties 1 link))))


(provide 'org-annotate-code)
;;; org-annotate-code.el ends here
