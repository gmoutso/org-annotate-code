;;; org-annotate-live.el --- Annotate code using org-capture
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides a live updating of annotations.

;;; Code:
(require 'org-annotate-code)

;; (defvar org-annotate-live-markers-alist nil
;;   "Alist of markers to links. Links are stored like org-store-link.")
;; (make-variable-buffer-local 'org-annotate-live-markers-alist)

;; (defvar org-annotate-live-stale-links nil
;;   "List of stale links.")
;; (make-variable-buffer-local 'org-annotate-live-stale-links)

;; on org-file

(defun org-annotate-live-get-link-ids-from-orgfile (test)
  "Get all id links from org file if the org-elements pass the TEST."
  (let ((tree (with-current-buffer (find-file-noselect org-annotate-code-org-file)
		(org-element-parse-buffer 'headline))))
	(org-element-map tree 'headline
	  (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline)))
				(if (funcall test id) id))))))

(defun org-annotate-live-replace-ids-in-orgfile (oldid newid)
  "Modify ids in orgfile."
  (with-current-buffer (find-file-noselect org-annotate-code-org-file)  ; see also insert-file-contents
    (if (org-annotate-code-search-id id)
    (org-set-property "CUSTOM_ID" newid))))

;; registering

(defun org-annotate-live-register-link (test link markers-alist stale-links-symbol)
  "Register LINK with marker or stale.
The TEST must go to the point or return nil.
Here markers are created from points."
  (if (funcall test link)
      (org-annotate-live-add-or-update-link-at-marker link (point) markers-alist)
    (add-to-list stale-links-symbol link)))

(defun org-annotate-live-register-links (test links markers-alist stale-links-symbol)
  "Register live links with markers and stale links for the first time.
LINKS are a list of links. 
The TEST must go to the point or return nil.
Here markers are created from points."
  (dolist (link links)
    (org-annotate-live-register-link test link markers-alist stale-links-symbol)))

;; adding to register

(defun org-annotate-live-add-link-at-marker (link marker-or-position markers-alist)
  "Add LINK at MARKER-OR-POINT even if marker is registered. 
A new marker is created."
  (setq marker (copy-marker marker-or-position t))
  (set-alist markers-alist marker link))

(defun org-annotate-live-update-link-at-marker (link marker-or-position markers-alist)
  "Update LINK at MARKER-OR-POINT when marker is registered."
  (let ((marker (copy-marker marker-or-position t)))
	(setf (alist-get marker (symbol-value markers-alist) nil nil 'equal) link)))

(defun org-annotate-live-add-or-update-link-at-marker (link marker-or-position markers-alist)
  "Update or add LINK at MARKER-OR-POINT. 
A marker is created if the MARKER-OR-POSITION was not registered. 
If MARKER-OR-POSITION was registered, the old link is discarded."
  (if (org-annotate-live-get-link-at-marker-or-position marker-or-position markers-alist)
      (org-annotate-live-update-link-at-marker link marker-or-position markers-alist)
      (org-annotate-live-add-link-at-marker link marker-or-position markers-alist)))

;; correcting

(defun org-annotate-live-correct-link-at-marker (marker-or-position)
  "Correct link that is registered by marker. 
The type of link is guessed from the old link.
Return the cons of replacement."
  (save-excursion
    (goto-char marker-or-position)
    (let* ((oldlink (org-annotate-live-get-link-at-marker-or-position marker-or-position))
	   (type (org-annotate-code-get-link-type oldlink))
	   (store (org-link-get-parameter type :store))
	   (plist (funcall store))
	   (rawlink (plist-get plist :link))
	   (newlink (org-link-make-string (concat type ":" rawlink))))
      (when (not (equal oldlink newlink))
	(org-annotate-live-update-link-at-marker newlink marker-or-position)
	(cons oldlink newlink)))))

(defun org-annotate-live-correct-before-register-new-marker-link (marker-or-position link)
  "Update link at marker in registers and org-file.
Assume LINK is correct at MARKER-OR-POSITION. 
Then register might be out of sync."
  (let* ((marker (copy-marker marker-or-position t))
	 (existinglink (org-annotate-live-get-link-at-marker-or-position marker-or-position))
	 (existingmarker (org-annotate-live-get-marker-of-link link))
	 (existingstale (org-annotate-live-link-is-stale link)))
    (cond ((and (not (equal marker existingmarker)) (equal link existinglink))
	   ;; link at existingmarker is wrong
	   (org-annotate-live-add-or-update-link-at-marker existingmarker))
	   ;; returns cons of oldlink newlink
	   ((and (equal marker existingmarker) (not (equal link existinglink)))
	   ;; existinglink should be updated to link
	   (org-annotate-live-add-or-update-link-at-marker existingmarker))
	   ;; returns cons of oldlink newlink
	   (existingstale
	    ;; link in stale links must be corrected
	    (org-annotate-live-correct-and-register-stale link))
	   ;; returns cons of oldlink newlink
	   )))

;; accessing

(defun org-annotate-live-link-is-stale (link stale-links-symbol)
  "Return true if link is stale"
  (member link stale-links-symbol))

(defun org-annotate-live-get-link-at-marker-or-position (marker-or-position markers-alist)
  "Get link at marker if link registered."
  (let* ((marker (copy-marker marker-or-position t))
	 (found (assoc marker (symbol-value markers-alist))))
    (if found (cdr found))))

(defun org-annotate-live-get-marker-of-link (link markers-alist)
  "Get marker if link is registered."
  (let ((found (rassoc link (symbol-value markers-alist))))
    (if found (car found))))

;; (defun org-annotate-code-parse-link-from-string (link)
;;   "Parse LINK string."
;;   (string-match "\\(?1:[[:alnum:]]*\\):\\(?2:[^]]*\\)" link)
;;   (let ((type (match-string-no-properties 1 link))
;; 	(rawlink (match-string-no-properties 2 link)))
;;   (list :type type :link rawlink)))

;; (defun org-annotate-code-parse-link-from-string-maybe (link)
;;   "Parse LINK string, unless it is parsed."
;;   (cond ((stringp link) (org-annotate-code-parse-link-from-string link))
;; 	(t link)))

;; (defun org-annotate-live-test-links (a b)
;;   "Test two parsed links A and B."
;;   (equal (plist-get a :link) (plist-get b :link)))

;; (defun org-annotate-live-updated-list-of-links (link links &optional test)
;;   "Return new list of LINKS with LINK added or updated according to TEST."
;;   (let ((link (org-annotate-code-parse-link-from-string-maybe link))
;; 	(test (or test 'org-annotate-live-test-links)))
;; 	  (if (not (seq-contains links link test))
;; 		 (cons link links)
;; 	    (substitute-if link (lambda (i) (funcall test i link)) links))))

;; (defun org-annotate-live-add-or-update-link-at-marker (link &optional marker test)
;;   "Update the markers-alist of markers with a LINK at the MARKER. Use test to know whether to add or replace."
;;   (setq marker (if marker (copy-marker marker)
;; 		 (point-marker)))
;;   (set-marker-insertion-type marker t)
;;   (let* ((link (org-annotate-code-parse-link-from-string-maybe link))
;; 	 (links (alist-get marker org-annotate-live-markers-alist nil nil 'equal)))
;;     (when links ; exist
;;       (setf (alist-get marker org-annotate-live-markers-alist nil nil 'equal)
;; 	      (org-annotate-live-updated-list-of-links link links test))
;;       (set-marker marker nil))
;;     (when (not links)
;;       (add-to-list 'org-annotate-live-markers-alist (cons marker (list link))))
;;     (if links (message "links at marker updated")
;;       (message "new marker with link added"))))

(provide 'org-annotate-live)
;;; org-annotate-live.el ends here
