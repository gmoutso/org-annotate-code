;;; org-annotate-index.el --- Support for adding filename to index.org in same folder
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides links to a file in the index.org file in the same directory.
;; It does not register a mode for org-annotate-code.
;; Instead a separate capture template must be made.
;; It can be called from any file or dired.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:
(require 'org-annotate-code)
(require 'ol)

(defcustom org-annotate-index-org-file "README.org"
  "Name of file relative to default-directory."
  :group 'org-annotate-code)

(defcustom org-annotate-index-heading0 "Files"
  "Name of heading0. 

If nil then none. If true (but not a string) the use `org-annotate-code-heading0'.
Supersedes choice in `org-annotate-code-heading0'."
  :group 'org-annotate-code)


(defun org-annotate-index-get-filename ()
  "Return filename at point with no directory." 
  (file-name-nondirectory
   (cond ((derived-mode-p 'dired-mode)
	  (dired-get-filename nil t))
	 ((buffer-file-name)
	  (buffer-file-name))
	 (t
	  (buffer-file-name)))))

(defun org-annotate-index-make-annotation-from-filename (filename &optional link)
  "Make annotation from FILENAME."
  (let* ((link (or link (format "[[file:%s]]" filename)))
	 (filename (if (equal filename ".") "Summary" filename)))
    (list (list :id link :heading filename))))

(defun org-annotate-code-info-at-point-index ()
  "Return a plist with word info at point."
  (let* ((filename (org-annotate-index-get-filename)))
    (org-annotate-index-make-annotation-from-filename filename)))

(defun org-annotate-index-capture-finding-location ()
  "Captures the filename of the buffer or in dired and inserts in index.org in the same folder."
  (let*  ((org-file (concat default-directory org-annotate-index-org-file))
	  (the-buffer (org-capture-target-buffer org-file))
	  (filename (org-annotate-index-get-filename))
	  (annotation (org-annotate-code-info-at-point-index))
	  (org-annotate-code-heading0 (cond
				       ((equal filename ".") nil)
				       ((equal org-annotate-index-heading0 t) org-annotate-code-heading0)
				       (org-annotate-index-heading0))))
    (set-buffer the-buffer)
    (org-annotate-code-search-and-create-levels (org-annotate-code-add-heading0 annotation)))
    )

(provide 'org-annotate-index)
;;; org-annotate-index.el ends here

