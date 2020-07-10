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

(defvar org-annotate-live-markers-alist nil
  "Alist of type to alist of markers to links. Links are stored like org-store-link.")
(make-variable-buffer-local 'org-annotate-live-markers-alist)

;; on org-file
(defun org-annotate-live-get-filename-from-link (link)
  (if (string-match "\\(?1:[-[:alnum:]_\\./]*\\)::.*" link)
      (expand-file-name (match-string-no-properties 1 link))))

(defun org-annotate-live-get-link-ids-from-orgfile (type)
  "Get all id links of TYPE from org file that point to file."
  (let ((this_file (expand-file-name (buffer-file-name)))
	(tree (with-current-buffer (find-file-noselect org-annotate-code-org-file)
		(org-element-parse-buffer 'headline))))
	(org-element-map tree 'headline
	  (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline)))
				(if (and (equal type (org-annotate-code-get-link-type id))
					 (equal this_file
						(org-annotate-live-get-filename-from-link id)))
				    id))))))

(defun org-annotate-live-replace-org-id (oldid newid)
  "Modify ids in orgfile."
  (with-current-buffer (find-file-noselect org-annotate-code-org-file)  ; see also insert-file-contents
    (if (org-annotate-code-search-id oldid)
    (org-set-property "CUSTOM_ID" newid))))

(defun org-annotate-live-make-stale-org-id (id)
  "Modify id to stale_link property in orgfile."
  (with-current-buffer (find-file-noselect org-annotate-code-org-file)  ; see also insert-file-contents
    (when (org-annotate-code-search-id id)
      (org-delete-property "CUSTOM_ID")
      (org-set-property "STALE_LINK" id))))

;; registering

(defun org-annotate-live-register-link (link &optional test)
  "Register LINK with marker or stale."
  (save-excursion 
    (let* ((type (org-annotate-code-get-link-type link))
	   (test (or test (org-link-get-parameter type :followstrict))))
      (if (funcall test link)
	  (org-annotate-live-add-or-update-link-at-marker link (point))
	(org-annotate-live-make-stale-org-id link)))))

(defun org-annotate-live-register-links (type)
  "Register all links to current buffer of TYPE."
  (let ((links (org-annotate-live-get-link-ids-from-orgfile type)))
    (mapcar org-annotate-live-register-link links)))

;; adding to register

(defun org-annotate-live-add-link-at-marker (link)
  "Add LINK at MARKER-OR-POINT even if marker is registered. 
A new marker is created."
  (setq marker (copy-marker marker-or-position t))
  (set-alist org-annotate-live-markers-alist marker link))

(defun org-annotate-live-update-link-at-marker (link marker-or-position)
  "Update LINK at MARKER-OR-POINT when marker is registered."
  (let ((marker (copy-marker marker-or-position t)))
	(setf (alist-get marker (symbol-value org-annotate-live-markers-alist) nil nil 'equal) link)))

(defun org-annotate-live-add-or-update-link-at-marker (link marker-or-position)
  "Update or add LINK at MARKER-OR-POINT. 
A marker is created if the MARKER-OR-POSITION was not registered. 
If MARKER-OR-POSITION was registered, the old link is discarded."
  (if (org-annotate-live-get-link-at-marker-or-position marker-or-position)
      (org-annotate-live-update-link-at-marker link marker-or-position)
      (org-annotate-live-add-link-at-marker link marker-or-position)))

;; correcting

(defun org-annotate-live-guess-link-from-point-and-link-copy (marker-or-position link-copy)
  "Get the new link at MARKER-OR-POISITION from the same type as LINK-COPY."
  (save-excursion
    (goto-char marker-or-position)
  (let* ((type (org-annotate-code-get-link-type link-copy))
	 (store (org-link-get-parameter type :store))
	 (plist (funcall store))
	 (rawlink (plist-get plist :link)))
    (org-link-make-string (concat type ":" rawlink)))))

(defun org-annotate-live-correct-register-link-at-marker (marker-or-position)
  "Correct link that is registered at MARKER-OR-POSITION. 
The type of link is guessed from the old link.
The new link is taken from `org-store-link' at the MARKER-OR-POSITION.
Return the cons of replacement."
    (let* ((oldlink (org-annotate-live-get-link-at-marker-or-position marker-or-position))
	   (newlink (org-annotate-live-guess-link-from-point-and-link-copy marker-or-position)))
      (when (not (equal oldlink newlink))
	(org-annotate-live-update-link-at-marker newlink marker-or-position)
	(cons oldlink newlink))))


(defun org-annotate-live-correct-register-before-new-marker-link (marker-or-position link)
  "Update link at marker in registers and org-file.

Assume LINK is correct at MARKER-OR-POSITION. 
However the register might be out of sync:
1) the MARKER-OR-POSITION is registered to a different link that should be corrected.
The correct link is already provided as LINK.
2) the same LINK is registered at another marker that should be corrected.
The correct link can be guess from calling the store function at the other marker.
3) the same link is a stale link that has to be deactivated.
The stale link and its marker can only be guessed with the input of the user.
"
  (let* ((marker (copy-marker marker-or-position t))
	 (existinglink (org-annotate-live-get-link-at-marker-or-position marker-or-position))
	 (existingmarker (org-annotate-live-get-marker-of-link link))
	 correctlink)
    (setq correctlink
    (cond ((and (not (equal marker existingmarker)) (equal link existinglink))
	   ;; same link at other existingmarker is wrong (2)
	   (org-annotate-live-correct-register-link-at-marker existingmarker))
	   ((and (equal marker existingmarker) (not (equal link existinglink)))
	   ;; existinglink should be updated to link (1)
	    (org-annotate-live-update-link-at-marker link existingmarker)
	    (cons existinglink link))
	   ;; returns cons of oldlink newlink
	   ))
    (if correctlink
	(org-annotate-live-replace-org-id (car correctlink) (cdr correctlink)))))

;; accessing

(defun org-annotate-live-get-link-at-marker-or-position (marker-or-position)
  "Get link at marker if link registered."
  (let* ((marker (copy-marker marker-or-position t))
	 (found (assoc marker (symbol-value org-annotate-live-markers-alist))))
    (if found (cdr found))))

(defun org-annotate-live-get-marker-of-link (link)
  "Get marker if link is registered."
  (let ((found (rassoc link (symbol-value org-annotate-live-markers-alist))))
    (if found (car found))))


(provide 'org-annotate-live)
;;; org-annotate-live.el ends here
