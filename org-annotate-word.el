;;; org-annotate-word.el --- Support for links to words in Org mode
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides links that point to words near a line.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:
(require 'org-annotate-code)
(require 'ol)

;; links

(org-link-set-parameters "word"
                         :follow 'org-annotate-word-search
			 :followstrict 'org-annotate-word-search-strict
			 :followchoose 'org-annotate-word-search-choose
                         :export 'orgorg-annotate-word-export
                         :store 'org-annotate-word-store-link)

(defun org-annotate-word-store-link ()
  "Store a link."
  (let* ((filename (expand-file-name (buffer-file-name)))
	 (word (org-annotate-word-word-at-point))
	 (lineno (line-number-at-pos))
	 (description nil))
      (if word (org-link-store-props
		:type "word"
		:link (format "%s::%s::%s" filename lineno word)))))

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

;; follow

(defun org-annotate-word-move-within-word ()
  "Move at beginning of word."
  (if (word-at-point) (beginning-of-thing 'word)))

(defun org-annotate-word-position-word-at-point ()
  "Get position of beginning of word if at word."
  (when (word-at-point)
    (save-excursion
      (org-annotate-word-move-within-word)
      (point))))

(defun org-annotate-word-goto-word-choose (lineno word)
  "Choose WORD at LINENO.
Return nil if not found."
  (org-annotate-word-goto-word lineno word)
  (let ((list-matching-lines-jump-to-current-line t))
    (occur word)
    (org-annotate-word-goto-word-strict (line-number-at-pos) word)
    (org-annotate-word-move-within-word)
    (org-annotate-word-position-word-at-point)))

(defun org-annotate-word-goto-word-strict (lineno word)
  "Goto first WORD at LINENO.
Return nil if not found."
  (let ((found
	 (save-excursion 
	   (goto-char (point-min))
	   (forward-line (- lineno 1))
	   (search-forward word (point-at-eol) t))))
    (when found
      (goto-char found)
      (org-annotate-word-move-within-word)
      (org-annotate-word-position-word-at-point))))

(defun org-annotate-word-goto-word (lineno word &optional window)
  "Goto WORD near LINENO.
Return nil if not found."
  (let ((p (point))
	(found (org-annotate-word-goto-word-strict lineno word))
	find1 find2)
    (unless found
      (setq find1 (save-excursion
		     (goto-char (point-min))
		     (forward-line (- lineno 1))
		     (when (search-forward word
					 (when window (point-at-eol (+ 1 window)))
					 t)
			 (point))))
      (setq find2 (save-excursion
		     (goto-char (point-min))
		     (forward-line (- lineno 1))
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
			  ((> (- lineno (line-number-at-pos find2)) (- (line-number-at-pos find1) lineno))
			   find1)
			  ((< (- lineno (line-number-at-pos find2)) (- (line-number-at-pos find1) lineno))
			   find2))))
    (when found
      (goto-char found)
      (org-annotate-word-move-within-word)
      (org-annotate-word-position-word-at-point)
      )
    ))

;; folowing link

(defun org-annotate-word-split-filename-lineno-searchname (link)
  "Return list of file::no::word from LINK."
  (string-match "\\(?1:[-[:alnum:]_\\./]*\\)::\\(?2:[0-9]*\\)::\\(?3:[-[:alnum:]_\\./]*\\)" link)
  (list (match-string-no-properties 1 link) (string-to-number (match-string-no-properties 2 link)) (match-string-no-properties 3 link)))

(defun org-annotate-word-get-lineno (link)
  "Return line number of LINK."
  (nth 1 (org-annotate-word-split-filename-lineno-searchname link)))

(defun org-annotate-word-search (link &optional window)
  "Search for link.
Return nil if not found."
  (let* ((splitlink (org-annotate-word-split-filename-lineno-searchname link))
	 (filename (nth 0 splitlink))
	 (lineno (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word lineno word window)))

(defun org-annotate-word-search-strict (link)
  (let* ((splitlink (org-annotate-word-split-filename-lineno-searchname link))
	 (filename (nth 0 splitlink))
	 (lineno (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word-strict lineno word)))

(defun org-annotate-word-search-choose (link)
  (let* ((splitlink (org-annotate-word-split-filename-lineno-searchname link))
	 (filename (nth 0 splitlink))
	 (lineno (nth 1 splitlink))
	 (word (nth 2 splitlink)))
    (find-file filename)
    (org-annotate-word-goto-word-choose lineno word)))

;; annotation

(defun org-annotate-word-add-filename-node (filename annotation)
  "Add the FILENAME as top-level node to ANNOTATION."
  (let* ((filename (expand-file-name filename))
	 (id (org-annotate-code-make-file-link filename))
	 (heading (file-name-nondirectory filename))
	 filenode)
    (setq filenode
	(list :id id
	      :heading heading))
    (cons filenode annotation)))

(defun org-annotate-word-make-annotation-from-word (filename lineno word)
  "Make annotation from WORD."
  (let ((annotation (list (list :id (format "[[word:%s::%s::%s]]" filename lineno word)
				:heading (format "%s near %s" word lineno)))))
	(org-annotate-word-add-filename-node filename annotation)))

(defun org-annotate-word-info-at-point ()
  "Return a plist with word info at point."
  (let ((word (org-annotate-word-word-at-point)))
    (when word
      (let* ((type "word")
	     (filename (buffer-file-name))
	     (lineno (line-number-at-pos))
	     (store (org-link-get-parameter type :store))
	     (plist (funcall store))
	     (rawlink (plist-get plist :link))
	     (link (org-link-make-string (concat  type ":" rawlink ))))
	(when (and (featurep 'org-annotate-live) org-annotate-live-mode)
	  (org-annotate-live-register-link (org-annotate-word-position-word-at-point)
					   link))
	(org-annotate-word-make-annotation-from-word filename lineno word)))))

(provide 'org-annotate-word)
;;; org-annotate-word.el ends here

