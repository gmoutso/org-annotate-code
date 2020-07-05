;;; org-annotate-dired.el --- Support for links to files in dired in Org mode
;; Copyright (C) 2020

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode) (org-annotate-code))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides links that point to files when in dired.
;; It integrates with org-annotate-code to provide annotations to them.

;;; Code:
(require 'org-annotate-code)
(require 'ol)
(defun org-annotate-dired-get-filename ()
  "Return filename at point."
  (expand-file-name (dired-get-filename)))

(defun org-annotate-dired-make-annotation-from-filename (filename)
  "Make annotation from FILENAME."
  (list (list :id (format "[[file:%s]]" filename)
				:heading filename)))

(defun org-annotate-code-info-at-point-dired ()
  "Return a plist with word info at point."
  (let ((filename (org-annotate-dired-get-filename)))
    (org-annotate-dired-make-annotation-from-filename filename)))

(add-to-list 'org-annotate-code-info-alist (cons 'dired-mode 'org-annotate-code-info-at-point-dired))

(provide 'org-annotate-dired)
;;; org-annotate-dired.el ends here

