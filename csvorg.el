;;; csvorg.el --- Convert between csv and org -*- lexical-binding: t; -*-

;; Copyright (C) 2023 orgtre

;; Author: orgtre
;; Package-Requires: ((pcsv "1.3.6"))
;; URL: https://github.com/orgtre/csvorg

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'pcsv)
(require 'org)
(require 'org-element)

(defgroup csvorg nil
  "Convert between csv and org."
  :group 'text)

(defcustom csvorg-default-topheading "entries"
  "See `csvorg-import'."
  :type 'string)

(defcustom csvorg-default-heading-constructor #'car
  "See `csvorg-import'."
  :type 'function)

(defcustom csvorg-import-transform-functions
  (list (lambda (f) (string-trim f)))
  "Functions applied to each field when importing to org.
Each function in this list will be called (in order) with the
field value string to be imported as the sole argument, and should
return the (possibly) transformed field value."
  :type 'list)

(defcustom csvorg-export-transform-functions
  (list (lambda (f) (if (not f) "" f))
	(lambda (f) (string-trim f))
	(lambda (f) (string-replace "\n" "\\n" f)))
  "Functions applied to each field when exporting from org.
Each function in this list will be called (in order) with the
field value string to be exported as the sole argument, and should
return the (possibly) transformed field value."
  :type 'list)

(defcustom csvorg-ch-sep "\n\n"
  "Separator between org entry content and next heading.
Used when constructing org entries from csv."
  :type 'string)

(defcustom csvorg-hc-sep "\n"
  "Separator between org heading and (non-subheading) content.
Used when constructing org entries from csv."
  :type 'string)

(defcustom csvorg-hh-sep "\n"
  "Separator between two org headings when there is no content.
Used when constructing org entries from csv."
  :type 'string)


;;;; Importing csv to org

;;;###autoload
(defun csvorg-import (csvfile orgfile &optional
			      colnames propcols subhcols
			      topheading heading-constructor
			      transforms coding-system)
  "Import CSVFILE as `org-mode' entries into ORGFILE.

Each row in CSVFILE corresponds to one `org-mode' entry in ORGFILE.
The entries are placed at the end of ORGFILE under a newly created
top-level heading TOPHEADING, which defaults to
`csvorg-default-topheading'.

HEADING-CONSTRUCTOR is a function which creates an entry heading
given as argument one parsed CSVFILE-row in list form. It defaults
to `csvorg-default-heading-constructor'.

COLNAMES is a list of column names. If it is nil, column names
are read from the first row of CSVFILE. Avoid COLNAMES in
`org-special-properties' if they are to be PROPCOLS, also if
their case differs.

PROPCOLS lists the names of columns to be inserted as
org-entry properties. SUBHCOLS lists the names of columns to
be inserted as org-entry subheadings. If neither is given,
PROPCOLS defaults to COLNAMES and SUBHCOLS to nil. If only one
of them is given, the other defaults to the remaining COLNAMES.

TRANSFORMS is a list of functions used to transform the field
values. It defaults to `csvorg-import-transform-functions'.

CODING-SYSTEM is passed on to `pcsv-parse-file', which does the
parsing. If ^M occurs in output, try setting this to utf-8-dos."
  (interactive (list (read-file-name "Input csv-file: ")
		     (read-file-name "Output org-file: ")))
  (let* ((data (pcsv-parse-file csvfile coding-system))
	 (colnames (or colnames (pop data)))
	 (propcols (if (and (not propcols) (not subhcols))
		       colnames
		     (if subhcols
			 (seq-difference colnames subhcols)
		       propcols)))
	 (subhcols (or subhcols (seq-difference colnames propcols)))
	 (proppos (mapcar
		   (lambda (x) (cl-position x colnames :test 'equal))
		   propcols))
	 (subhpos (mapcar
		   (lambda (x) (cl-position x colnames :test 'equal))
		   subhcols))
	 (max-prop-length (apply #'max
				 (mapcar (lambda (x)
					   (length (nth x colnames)))
					 proppos))))
    (unless (equal (length (nth 0 data))
		   (length colnames))
      (message
       "Warning: colnames doesn't match the number of columns in data."))
    (unless (seq-set-equal-p colnames (append propcols subhcols))
      (message
       "Warning: propcols and subhcols are not compatible with colnames."))
    (with-current-buffer (find-file-noselect orgfile)
      (goto-char (point-max))
      (org-insert-heading '(4) t t)
      (insert (or topheading csvorg-default-topheading) "\n" csvorg-hh-sep)
      (insert
       (mapconcat
	(lambda (row)
	  (csvorg--row-to-entry-string
	   row colnames proppos subhpos max-prop-length
	   (or transforms
	       'csvorg-import-transform-functions)
	   (or heading-constructor
	       csvorg-default-heading-constructor)))
	data))
      (save-buffer))
    (message "Finished importing.")))


(defun csvorg--row-to-entry-string (row colnames proppos subhpos
					max-prop-length transforms
					heading-constructor)
  "Format parsed csv-file row ROW as org-entry string.
Subroutine of `csvorg-import', which see."
  (let* ((heading (funcall heading-constructor row))
	 (out (concat "** " heading "\n"))
	 (nsub 0))
    (when proppos
      (setq out (concat out ":PROPERTIES:\n"))
      (dolist (i proppos)
	(let ((cell (csvorg--transform-field (nth i row) transforms)))
	  (when (and cell (not (equal cell "")))
	    (setq out
		  (concat out
			  ":" (nth i colnames) ": "
			  (make-string (- max-prop-length
					  (length (nth i colnames)))
				       ? )
			  cell
			  "\n")))))
      (setq out (concat out ":END:\n")))
    (dolist (i subhpos)
      (let ((cell (csvorg--transform-field (nth i row) transforms)))
	(when (and cell (not (equal cell "")))
	  (setq nsub (+ nsub 1))
	  (setq out (concat out
			    csvorg-hh-sep "*** " (nth i colnames)
			    "\n" csvorg-hc-sep cell "\n" csvorg-ch-sep)))))
    (when (equal nsub 0)
      (setq out (concat out csvorg-hh-sep)))
    out))


(defun csvorg--transform-field (field transforms)
  "Apply TRANSFORMS to FIELD."
  (run-hook-wrapped transforms
		    (lambda (fun)
		      (setq field (funcall fun field)) nil))
  field)


;;;; Exporting org to csv

;;;###autoload
(defun csvorg-export (csvfile idprop colnames &optional
			      match scope replace inherit transforms)
  "Export org entries as csv to CSVFILE.

The string IDPROP is the name of a property which
is used to identify the entries to be exported. Only
entires with a non-empty value of this property are exported.

COLNAMES is a list of properties and subheadings of the
org entries, which will be used as columns in CSVFILE.
`org-special-properties' are allowed.

MATCH and SCOPE are as in `org-map-entries', which see.
IF MATCH is non-nil it is used instead of IDPROP.

When REPLACE is non-nil, CSVFILE is overwritten if it exists.
Set INHERIT non-nil to retrieve properties with inheritance.

TRANSFORMS is a list of functions used to transform the field
values. It defaults to `csvorg-export-transform-functions'."
  (interactive
   (list (read-file-name "Output csv-file: ")
	 (org-read-property-name)
	 (split-string
	  (read-string
	   (concat "List names of properties and subheadings "
		   "to export,\nwithout quotes and "
		   "separated by spaces, e.g. COL1 COL2:\n")))))
  (let (s)
    (setq s (orgtbl-to-csv
	     (cons
	      colnames
	      (org-map-entries
	       (lambda () (csvorg--entry-to-list
			   colnames inherit
			   (or transforms
			       'csvorg-export-transform-functions)))
	       (if match match (concat idprop "<>\"\""))
	       scope))
	     nil))
    (if (or replace
	    (not (file-exists-p csvfile))
	    (y-or-n-p (format "Overwrite %s?" csvfile)))
	(with-current-buffer (find-file-noselect csvfile)
	  (erase-buffer)
	  (insert s)
	  (save-buffer)
	  (kill-buffer (current-buffer))
	  (message "Finished exporting."))
      (message "Nothing exported."))))


(defun csvorg--entry-to-list (colnames &optional inherit transforms)
  "Convert org entry at point to a list.
The list contains the values of the properties and contents
of child-entries whose name or heading matches an item
in COLNAMES, in the order of colnames. Newlines in values
are quoted. When INHERIT is non-nil, properties are inherited."
  (let (subhs outcols)
    (save-excursion
      (when (org-goto-first-child)
	(push (csvorg--entry-get-item-and-contents) subhs)
	(while (org-goto-sibling)
	  (push (csvorg--entry-get-item-and-contents) subhs))))
    (dolist (col colnames)
      (if-let ((val (org-entry-get nil col inherit)))
	  (push
	   (csvorg--transform-field val transforms)
	   outcols)
	(push
	 (csvorg--transform-field (cdr (assoc col subhs)) transforms)
	 outcols)))
    (nreverse outcols)))


(defun csvorg--entry-get-item-and-contents ()
  "Get item property (heading) and contents of current entry."
  (cons
   (org-entry-get nil "ITEM")
   (let* ((element (org-element-at-point))
	  (beg (org-element-property :contents-begin element))
	  (end (org-element-property :contents-end element)))
     (and beg end
	  (string-trim (buffer-substring-no-properties beg end))))))


(provide 'csvorg)

;;; csvorg.el ends here
