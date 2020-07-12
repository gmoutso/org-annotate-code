;;; org-annotate-live.el --- Annotate code using org-capture
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.1
;; Package-Requires: ((org-mode))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides a live updating of annotations.

;;; Code:
(require 'org-annotate-code)

(defvar org-annotate-live-markers nil
  "Alist of type to alist of markers to links. Links are stored like org-store-link.")
(make-variable-buffer-local 'org-annotate-live-markers)

(defcustom org-annotate-live-use-hash nil
  "Use hash to invalidate links in org-file."
  :group 'org-annotate-word)
(defcustom org-annotate-live-forgive-window nil
  "Integer window of lines to correct and forgive links that are not exact. 
This applies to a wrong hash that will be updated.
Set to 0 means only in the line of the link's position."
  :group 'org-annotate-word)

;; parsing org-file

(defun org-annotate-live-get-org-tree (&optional tree)
  "Return headline AST with id links of TYPE that point to the current file.

if TREE is non nil, parse this tree. Else use the tree of the org file for current file."
  (let ((type "word")
	(this_file (expand-file-name (buffer-file-name)))
	(tree (or tree (with-current-buffer (find-file-noselect org-annotate-code-org-file)
		(org-element-parse-buffer 'headline)))))
	(org-element-map tree 'headline
	  (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline)))
				(if (and
				     id
				     (equal type (org-annotate-code-get-link-type id))
				     (equal this_file
					    (org-annotate-code-get-filename id)))
				    headline))))))


(defun org-annotate-live-stale-from-search ()
  "Return invalid ids if strict search fails."
  (let* ((type "word")
	 (test (org-link-get-parameter type :followstrict))
	 (tree (org-annotate-live-get-org-tree)))
	 (org-element-map tree 'headline
	   (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline)))
				 (save-excursion
				   (if (and
					id
					(not (funcall test id))) id)))))))

(defun org-annotate-live-valid-from-search ()
  "Return invalid ids if strict search fails."
  (let* ((type "word")
	 (test (org-link-get-parameter type :followstrict))
	 (tree (org-annotate-live-get-org-tree)))
	 (org-element-map tree 'headline
	   (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline)))
				 (save-excursion
				   (if (and
					id
					(funcall test id)) id)))))))

(defun org-annotate-live-stale-from-hash ()
  "Return stale if hash not correct."
  (let* ((type "word")
	 (this_file (expand-file-name (buffer-file-name)))
	 (this_hash (secure-hash 'md5 (current-buffer)))
	 (tree (org-annotate-live-get-org-tree)))
    (org-element-map tree 'headline
      (lambda (headline)  (let ((id (org-element-property :CUSTOM_ID headline))
				(hash (org-element-property :HASH headline)))
			    (if (and
				 id
				 ;; type must be word
				 (equal type (org-annotate-code-get-link-type id))
				 ;; file must be this_file
				 (equal this_file	(org-annotate-code-get-filename id))
				 ;; hash must be set
				 hash
				 ;; and must not be this_hash
				 (not (equal this_hash hash)))
				id))))))

;; modify org file

(defun org-annotate-live--replace-org-id (oldid newid)
  "Modify ids in orgfile."
  (with-current-buffer (find-file-noselect org-annotate-code-org-file)  ; see also insert-file-contents
    (if (org-annotate-code-search-id oldid)
	(org-set-property "CUSTOM_ID" newid))))

(defun org-annotate-live--make-stale-org-id (id)
  "Modify id to stale_link property in orgfile."
  (with-current-buffer (find-file-noselect org-annotate-code-org-file)  ; see also insert-file-contents
    (when (org-annotate-code-search-id id)
      (org-delete-property "CUSTOM_ID")
      (org-set-property "STALE" id))))

;; creating register

(defun org-annotate-live-kill-register ()
  (setq-local org-annotate-live-markers nil))

(defun org-annotate-live-create-register ()
  "Register for first time."
  (interactive)
  (when org-annotate-live-mode
    (org-annotate-live-kill-register)
    (let* ((type "word")
	   (test (org-link-get-parameter type :followstrict))
	   valid stale)
      ;; invalidate if there is a hash
      (setq stale (org-annotate-live-stale-from-hash))
      (mapcar 'org-annotate-live--make-stale-org-id stale)
      ;; invalidate when stric search fails
      (setq stale (org-annotate-live-stale-from-search))
      (mapcar 'org-annotate-live--make-stale-org-id stale)
      ;; add markers for valid
      (setq valid (org-annotate-live-valid-from-search))
      (save-excursion
	(dolist (link valid)
	  (if (funcall test link)
	      (org-annotate-live-add-or-update-link-at-marker link (point))))))))

;; editting register

(defun org-annotate-live--get-link-of-marker (marker-or-position)
  "Return link of marker if registered, else nil."
  (let* ((marker (copy-marker marker-or-position t))
	 (found (assoc marker  org-annotate-live-markers)))
    (if found (cdr found))))

(defun org-annotate-live--get-marker-of-link (link)
  "Return marker of link if registered, else nil."
  (let ((found (rassoc link org-annotate-live-markers)))
    (if found (car found))))

(defun org-annotate-live--add-link-at-marker (link marker-or-position)
  "Add LINK at MARKER-OR-POINT. A new marker is created."
  (setq marker (copy-marker marker-or-position t))
  (set-alist 'org-annotate-live-markers marker link))

(defun org-annotate-live--update-link-at-marker (link marker-or-position)
  "Update LINK at MARKER-OR-POINT when marker is registered."
  (let ((marker (copy-marker marker-or-position t)))
	(setf (alist-get marker org-annotate-live-markers nil nil 'equal) link)))

(defun org-annotate-live-add-or-update-link-at-marker (link marker-or-position)
  "Update or add LINK at MARKER-OR-POINT. 
A marker is created if the MARKER-OR-POSITION was not registered. 
If MARKER-OR-POSITION was registered, the old link is discarded."
  (if (org-annotate-live--get-link-of-marker marker-or-position)
      (org-annotate-live--update-link-at-marker link marker-or-position)
      (org-annotate-live--add-link-at-marker link marker-or-position)))

;; correcting register

(defun org-annotate-live--guess-link-at-point-from-link-copy (marker-or-position link-copy)
  "Get the new link at MARKER-OR-POISITION from the same type as LINK-COPY."
  (save-excursion
    (goto-char marker-or-position)
  (let* ((type (org-annotate-code-get-link-type link-copy))
	 (store (org-link-get-parameter type :store))
	 (plist (funcall store))
	 (rawlink (plist-get plist :link)))
    (org-link-make-string (concat type ":" rawlink)))))

(defun org-annotate-live--correct-link-at-marker (marker-or-position)
  "Correct link that is registered at MARKER-OR-POSITION. 
The type of link is guessed from the old link.
The new link is taken from `org-store-link' at the MARKER-OR-POSITION.
Return the cons of replacement."
    (let* ((oldlink (org-annotate-live--get-link-of-marker marker-or-position))
	   (newlink (org-annotate-live--guess-link-at-point-from-link-copy marker-or-position oldlink)))
      (when (not (equal oldlink newlink))
	;; update link in register
	(org-annotate-live--update-link-at-marker newlink marker-or-position))))

(defun org-annotate-live-correct-register-before-new-marker-link (marker-or-position link)
  "Correct before a link at marker is updated.

Assume provided LINK is correct at MARKER-OR-POSITION. 
The register and org-file might be out of sync:
1) the MARKER-OR-POSITION is registered to a different link:
An existing link at MARKER-OR-POSITION should become LINK at MARKER-OR-POSITION.
The org-file should change the old link. We do not update the register here!
2) the LINK is registered an existing different marker:
The link at the other marker should change.
The org-file should change the link to a new link at other marker.
The first stage is to correct the register.
The second stage changes the link in the org-file.
"
  (let* ((marker (copy-marker marker-or-position t))
	 (existinglink (org-annotate-live--get-link-of-marker marker-or-position))
	 (existingmarker (org-annotate-live--get-marker-of-link link))
	 correction)
    ;; correct links same as LINK that is elsewhere
    (when (and existingmarker (not (equal existingmarker marker)))
      (org-annotate-live--correct-link-at-marker existingmarker)
      ;; the change in org-file
      (org-annotate-live--replace-org-id link (org-annotate-live--get-link-of-marker existingmarker)))
    ;; correct link at marker that is wrong in org-file only
    (when (and existinglink (not (equal link existinglink)))
      ;; (org-annotate-live--update-link-at-marker link marker)
      (org-annotate-live--replace-org-id existinglink link))))

(defun org-annotate-live-marker-and-link-out-of-sync (marker link)
  (not (equal marker (org-annotate-word-get-position link))))

(defun org-annotate-live-register-link (marker link)
  "Add safely a new link."
  ;; (org-annotate-live-sync-register)  ;; also updates org
  (org-annotate-live-correct-register-before-new-marker-link marker link) ;; also updates org
  (org-annotate-live-add-or-update-link-at-marker link marker)) ; this does not update org

(defun org-annotate-live-sync-register ()
  "Change markers and org-file links that are out of sync."
  (interactive)
  (let ((forgotten (seq-filter (lambda (x) (org-annotate-live-marker-and-link-out-of-sync
					   (car x) (cdr x)))
			       org-annotate-live-markers)))
    (dolist (c forgotten)
      (let ((marker (car c))
	    (oldlink (cdr c)))
	(org-annotate-live--correct-link-at-marker marker)
	(org-annotate-live--replace-org-id oldlink
					   (org-annotate-live--get-link-of-marker marker))))
    (if forgotten (message "Org-file synced"))))

;; live mode
(define-minor-mode org-annotate-live-mode
  "Toggle live mode."
  :init-value nil
  :lighter "lv"
  :keymap nil
  :group 'org-annotate-live
  (org-annotate-live-initialize-maybe))

(defun org-annotate-live-initialize-maybe ()
  (if org-annotate-live-mode
      (org-annotate-live-initialize)
    (org-annotate-live-shutdown)))

(defun org-annotate-live-initialize ()
  (org-annotate-live-create-register)
  (add-hook 'after-save-hook 'org-annotate-live-save-file t t))

(defun org-annotate-live-save-file ()
  (org-annotate-live-sync-register))

(defun org-annotate-live-shutdown ()
  (org-annotate-live-sync-register)
  (setq-local org-annotate-live-markers nil)
  (remove-hook 'after-save-hook 'org-annotate-live-save-file t))

(provide 'org-annotate-live)
;;; org-annotate-live.el ends here
