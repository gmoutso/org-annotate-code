;;; org-annotate-word.el --- Support for links to words in Org mode
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.1
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides links that point to words near a position.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:
(require 'org-annotate-code)
(require 'org-annotate-live)
(require 'ol)

;; links

(org-link-set-parameters "word"
                         :follow 'org-annotate-word-open-link
			 :followstrict 'org-annotate-word-goto-link
                         :export 'orgorg-annotate-word-export
                         :store 'org-annotate-word-store-link)

(defcustom org-annotate-word-mode-blacklist '(org-mode pdf-view-mode python-mode dired-mode)
  "Mode blacklist."
  :group 'org-annotate-code
  :type 'list)

(defun org-annotate-word-store-link ()
  "Store a link."
  (unless (member major-mode org-annotate-word-mode-blacklist)
  (let* ((filename (expand-file-name (buffer-file-name)))
	 (word (org-annotate-word-word-at-point))
	 (position (org-annotate-word-position-beginning))
	 (description nil))
      (if word (org-link-store-props
		:type "word"
		:link (format "%s::%s::%s" filename position word)
		:position position
		:file filename
		:word word)))))

(defun org-annotate-word-export (link description format)
  "Export LINK with DESCRIPTION into FORMAT."
  (let ((path link)
        (desc (or description link)))
    (pcase format
      ;; (`html (format "%s" path))
      ;; (`latex (format "\\href{%s}{%s}" path desc))
      ;; (`texinfo (format "@uref{%s,%s}" path desc))
      ;; (`ascii (format "%s (%s)" desc path))
      (_ (if description (format "%s (%s)" desc path) path)))))

;; parsing

(defun org-annotate-word-word-at-point  ()
  "Return word at point."
  (when (word-at-point)
    (substring-no-properties (word-at-point))))

(defun org-annotate-word-move-beginning ()
  "Move at beginning of word."
  (if (word-at-point) (beginning-of-thing 'word)))

(defun org-annotate-word-move-end ()
  "Move at end of word."
  (if (word-at-point) (beginning-of-thing 'word)))

(defun org-annotate-word-position-beginning ()
  "Get position of beginning of word if at word."
  (when (word-at-point)
    (save-excursion
      (org-annotate-word-move-beginning)
      (point))))

(defun org-annotate-word-position-end ()
  "Get position of end of word if at word."
  (when (word-at-point)
    (save-excursion
      (org-annotate-word-move-end)
      (point))))

;; follow

(defun org-annotate-word-goto-word-choose (position word)
  "Choose WORD at POSITION.
Return nil if not found."
  (org-annotate-word-goto-word-near-position position word)
  (let ((list-matching-lines-jump-to-current-line t))
    (occur word)
    (org-annotate-word-goto-word-at-position (point) word)
    (org-annotate-word-move-beginning)
    (org-annotate-word-position-beginning)))

(defun org-annotate-word-goto-word-at-position (position word)
  "Goto first WORD at POSITION.
Return nil if not found."
  (let ((found
	 (save-excursion
	   (goto-char position)
	   (equal (org-annotate-word-word-at-point) word))))
    (when found
      (goto-char position)
      (org-annotate-word-move-beginning)
      (org-annotate-word-position-beginning))))

(defun org-annotate-word-goto-word-near-position (position word &optional window)
  "Goto WORD near POSITION.
Return nil if not found."
  (let ((p (point))
	(found (org-annotate-word-goto-word-at-position position word))
	find1 find2)
    (unless found
      (setq find1 (save-excursion
		     (goto-char position)
		     (when (search-forward word
					 (when window (point-at-eol (+ 1 window)))
					 t)
			 (point))))
      (setq find2 (save-excursion
		     (goto-char position)
		     (when (search-backward word
					  (when window (point-at-bol (- 1 window)))
					  t)
			 (point))))
      (setq found (cond ((and (not find2) (not find2))
			   nil) ;; return nil
			  ((and (not find1) find2)
			   find2)
			  ((and (not find2) find1)
			   find1)
			  ((> (- position find2) (- find1 position))
			   find1)
			  ((< (- position find2) (- find1 position))
			   find2))))
    (when found
      (goto-char found)
      (org-annotate-word-move-beginning)
      (org-annotate-word-position-beginning))))

;; folowing link

(defun org-annotate-word-split-filename-position-searchname (link)
  "Return list of file::no::word from LINK."
  (string-match "\\(?1:[-[:alnum:]_\\./]*\\)::\\(?2:[0-9]*\\)::\\(?3:[-[:alnum:]_\\./]*\\)" link)
  (list (match-string-no-properties 1 link) (string-to-number (match-string-no-properties 2 link)) (match-string-no-properties 3 link)))

(defun org-annotate-word-get-position (link)
  "Return position of LINK."
  (nth 1 (org-annotate-word-split-filename-position-searchname link)))

(defun org-annotate-word-open-link (link &optional window)
  "Search for word near link's position."
  (let* ((splitlink (org-annotate-word-split-filename-position-searchname link))
	 (filename (nth 0 splitlink))
	 (position (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word-near-position position word window)))

(defun org-annotate-word-goto-link (link)
  "Go to precise link position, if word is there."
  (let* ((splitlink (org-annotate-word-split-filename-position-searchname link))
	 (filename (nth 0 splitlink))
	 (position (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word-at-position position word)))

(defun org-annotate-word-search-choose (link)
  (let* ((splitlink (org-annotate-word-split-filename-position-searchname link))
	 (filename (nth 0 splitlink))
	 (position (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word-choose position word)))

(defun org-annotate-word-add-filename-node (filename annotation)
  "Add the FILENAME as top-level node to ANNOTATION."
  (if (and (featurep 'org-annotate-python) (derived-mode-p 'python-mode))
      (org-annotate-python-add-filename-node filename annotation)
    (org-annotate-code-add-filename-node filename annotation)))

;; annotation

(defun org-annotate-word-make-annotation-from-word (filename position word)
  "Make annotation from WORD."
  (let ((annotation (list (list :id (format "[[word:%s::%s::%s]]" filename position word)
				:heading (read-string "Description: " (format "%s at %s" word position))))))
	(org-annotate-word-add-filename-node filename annotation)))

(defun org-annotate-word-info-at-point ()
  "Return a plist with word info at point."
  (let ((word (org-annotate-word-word-at-point)))
    (when word
      (let* ((type "word")
	     (store (org-link-get-parameter type :store))
	     (plist (funcall store))
	     (rawlink (plist-get plist :link))
	     (position (plist-get plist :position))
	     (filename (plist-get plist :file))
	     (link (org-link-make-string (concat  type ":" rawlink ))))
	(when (and (featurep 'org-annotate-live) org-annotate-live-mode)
	  (org-annotate-live-register-link position
					   link))
	(org-annotate-word-make-annotation-from-word filename position word)))))

(defun org-annotate-word-capture-finding-location ()
  (let ((org-annotate-code-info-override 'org-annotate-word-info-at-point))
    (org-annotate-code-capture-finding-location)))

(provide 'org-annotate-word)
;;; org-annotate-word.el ends here

