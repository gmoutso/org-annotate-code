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
(org-link-set-parameters "word"
                         :follow 'org-annotate-word-search
                         :export #'orgorg-annotate-word-export
                         :store #'org-annotate-word-store-link)

(defun org-annotate-word-get-word ()
  "Return word at point."
  (word-at-point))

(defun org-annotate-word-split-filename-lineno-searchname (link)
  "Return list of file::no::word from LINK."
  (string-match "\\(?1:[^:]*\\)::\\(?2:[^:]\\)::\\(?3:[^:]\\)" link)
  (list (match-string-no-properties 1 link) (match-string-no-properties 2 link) (match-string-no-properties 3 link)))

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
				:heading (concat word " near " lineno)))))
	(org-annotate-word-add-filename-node filename annotation)))

(defun org-annotate-word-store-link ()
  "Store a link."
  (let* ((filename (expand-file-name (buffer-file-name)))
	 (word (org-annotate-word-get-name))
	 (lineno (line-number-at-pos))
	 (description nil))
      (org-link-store-props
       :type "word"
       :link (format "%s::%s::%s" filename lineno word)
       :description description)))

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

(defun org-annotate-word-goto-word (lineno word)
  "Goto WORD near LINENO."
  (let (find1 find2)
    (goto-line lineno)
    (search-forward word nil t)
    (setq find1 (point))
    (goto-line lineno)
    (search-backward word nil t)
    (setq find2 (point))
    (cond ((and (not find2) (not find2))
	   (message (format "word %s not found" word))
	   (goto-char (point-min)))
	  ((and (not find1) find2)
	   (goto-char find2))
	  ((and (not find2) find1)
	   (goto-char find1))
	  ((> (- (line-number-at-pos find2) lineno) (- (line-number-at-pos find1) lineno))
	   (goto-char find1))
	  (t (goto-char find2)))))

(defun org-annotate-word-search (link)
  (let* ((splitlink (org-annotate-word-split-filename-lineno-searchname link))
	 (filename (nth 1 splitlink))
	 (lineno (nth 2 splitlink))
	 (word (nth 3 splitlink)))
    (if filename (find-file filename))
    (org-annotate-word-goto-word lineno word)))

(defun org-annotate-word-info-at-point ()
  "Return a plist with word info at point."
  (let* ((filename (buffer-file-name))
	 (lineno (line-number-at-pos))
	 (word (org-annotate-word-get-name))
	 (org-annotate-word-make-annotation-from-word filename lineno word))))

(provide 'org-annotate-word)
;;; org-annotate-word.el ends here

