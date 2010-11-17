;;; qda-indexing.el --- qda indexing stuff

;; Copyright (C) 2007 by Jim Ottaway <jeho@jeho.org>

;; This file is part of QDA: qualitative data analysis in emacs.
;; It is not part of GNU Emacs.

;; QDA is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; QDA is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with QDA; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;
;;
;;; History:
;;
;;
;;; Todo:
;;
;;  Use range code in `gnus-range.el' Wed Nov 8 16:45:13 2000 Added
;;  the basic functionality, just need to enable ranges as arguments
;;  to functions.
;;  ...and convince myself that these changes are safe before
;;  committing to them!
;;
;;  Sorting text units:  at the moment, there is an assumption that
;;  text units should be sorted, but this might not be desirable if
;;  other ways of organising data are introduced (you might want to
;;  put text units into any arbitrary order), so the sorting should be
;;  optional.  This will make set operations much more difficult
;;  though.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'qda-misc))

;;; indexing data structure

;; this is type list because we don't need to do
;; type checking so might as well save space.
;;; (defstruct (qda-indexing (:type list))
;;;   name
;;;   text-units)

(require 'gnus-range)

;; make it look like I wrote them ;-)
(defalias 'qda-compress-sequence 'gnus-compress-sequence)
(defalias 'qda-uncompress-range 'gnus-uncompress-range)

;; text units are held sparsely in the indexing data structure, with
;; all contiguous sequences concatenated.  Accessing the structure
;; with `qda-indexing-text-units' hides this concatenation.  Setting
;; text-units with `setf' automatically compresses sequences.

(defsubst* make-qda-indexing (&key name text-units)
  (list name (qda-compress-sequence text-units)))

(defsubst qda-indexing-name (i)
  (first i))

(defsubst qda-indexing-text-units (i)
  (qda-uncompress-range (second i)))

(defsetf qda-indexing-name (i) (v)
  (list 'setf (list 'first i) v))

(defsetf qda-indexing-text-units (i) (v)
  (list 'setf (list 'second i)
	(list 'qda-compress-sequence v)))

;;; text-units

(defsubst* make-qda-text-unit (&key number)
  "Return a text unit.
Argument &KEY supplies the number for the text unit."
  (or number nil))

(defsubst qda-text-unit-number (text-unit)
  "Return the number of TEXT-UNIT."
  text-unit)

(defsubst qda-text-unit= (text-unit1 text-unit2)
  "Return t if TEXT-UNIT1 is equal to TEXT-UNIT2, otherwise nil."
  (= text-unit1 text-unit2))

(defsubst qda-text-unit-p (text-unit)
  (integerp text-unit))


;;; an indexing list is a list of qda-indexing structures
;; with a list of qda-text-units in its test-units slot

;; a crude check
(defsubst qda-name-p (thing)
  "Return t if THING is a name, otherwise nil."
  (stringp thing))

(defsubst qda-find-indexing (doc indexing-list)
  "Return indexing for DOC in INDEXING-LIST."
  (when (qda-doc-p doc)
    (setq doc (qda-doc-name doc)))
  (loop
   for this-indexing in indexing-list
   thereis (if (string= doc (qda-indexing-name this-indexing))
	       this-indexing
	     nil)))

(defsubst qda-indexing-stats (indexing-list)
  "Return a list of statistics about INDEXING-LIST.
List looks like this: (NUMBER-OF-DOCS NUMBER-OF-TEXT-UNITS)."
  (let ((count 0))
    (dolist (this-indexing indexing-list)
      (incf count (length (qda-indexing-text-units this-indexing))))
    (list (length indexing-list) count)))

(defsubst qda-indexing-num-docs (stats-list)
  "Return number of docs in STATS-LIST.
(see `qda-indexing-stats'."
  (first stats-list))

(defsubst qda-indexing-num-text-units (stats-list)
  "Return number of text units in STATS-LIST.
(see `qda-indexing-stats')"
  (second stats-list))

(defsubst qda-sort-text-unit-func (text-unit1 text-unit2)
  "Return t if TEXT-UNIT1 is less than TEXT-UNIT2, otherwise nil."
  (if (< (qda-text-unit-number text-unit1)
	 (qda-text-unit-number text-unit2))
      t
    nil))

(defsubst qda-indexing-doc= (i1 i2)
  "Return t if indexing I1 has the same doc as indexing I2, otherwise nil."
  (string= (qda-indexing-name i1)
	   (qda-indexing-name i2)))

(defsubst qda-indexing-text-unit= (tu1 tu2)
  "Return t if text unit TU1 is equal to TU2, otherwise nil."
  (= (qda-text-unit-number tu1)
     (qda-text-unit-number tu2)))

(defsubst qda-all-indexing (doc)
  "Return a list of all the text units in DOC."
  (cons 1 (qda-doc-text-unit-count doc)))

(defsubst qda-make-uncompressed-text-unit-list (text-units)
  (cond ((eq text-units 'all)
	 (qda-all-indexing doc))
	((atom text-units)
	 (list text-units))
	(t (qda-uncompress-range text-units))))

(defun qda-validate-text-units (text-units doc)
  "Check that members of list TEXT-UNITS are valid as text units for DOC.
Signal an error if any text unit is bad."
  ;; we assume that DOC is ok.
  (dolist (this-text-unit text-units)
    (unless (numberp this-text-unit)
      (qda-error "Text unit must be a number"))
    (when (> this-text-unit (qda-doc-text-unit-count doc))
      (qda-error "Text unit %d out of range; %s has %d text units"
		 this-text-unit (qda-doc-name doc)
		 (qda-doc-text-unit-count doc)))
    (when (= 0 this-text-unit)
      (qda-error "Text units start at 1, not 0"))))


;;;   (loop
;;;    ;; I'm sure there is a ready made function for doing this already (?)
;;;    for x from 1 to (qda-doc-text-unit-count doc)
;;;    collect x))

;;; adding, deleting, etc.

;; todo: I think these functions should be able to deal with
;; a whole indexing-list.  Adding and deleting such a list
;; are do-able with set functions.

;; todo: Should I rewrite this using union?
(defun qda-add-indexing (doc indexing-list text-units)
  "Add TEXT-UNITS in DOC to INDEXING-LIST.
TEXT-UNITS can be a single text unit number or a list of text unit numbers.
If TEXT-UNITS is 'all, all of the text units in DOC are
added to INDEXING-LIST. Returns the new indexing list or nil."
  (unless (setq doc (qda-find-doc doc))
    (qda-error "Can't find doc %s" doc))
  (let (this-indexing)
    (setq text-units (qda-make-uncompressed-text-unit-list text-units))
    (qda-validate-text-units text-units doc)
    (setq this-indexing (qda-find-indexing doc indexing-list))
    (if (null this-indexing)
	(cons (make-qda-indexing
	       :name (qda-doc-name doc)
	       :text-units (delete-duplicates
			    (sort text-units 'qda-sort-text-unit-func)))
	      indexing-list)
      ;; we use destructive functions, assuming that this is ok...
      ;; is it?
      (nsubstitute
       (make-qda-indexing
	:name (qda-doc-name doc)
	:text-units (sort
		     (delete-duplicates
		      (nunion text-units
			      (qda-indexing-text-units this-indexing)))
		     'qda-sort-text-unit-func))
       this-indexing
       indexing-list))))

(defun qda-delete-indexing (doc indexing-list text-units)
  "Remove TEXT-UNITS in DOC from INDEXING-LIST.
TEXT-UNITS can be a list, a single text-unit or 'all.
Return the new indexing-list."
  (unless (setq doc (qda-find-doc doc))
    (qda-error "Can't find doc %s" doc))
  (let (this-indexing
	new-indexing)
    (setq text-units (qda-make-uncompressed-text-unit-list text-units))
    (qda-validate-text-units text-units doc)
    (setq this-indexing (qda-find-indexing doc indexing-list))
    (if (null this-indexing)
	indexing-list
      ;; destructive, like qda-add-indexing...  but delete is not
      ;; consistently destructive (isn't if deletee is car of
      ;; deleted).
      (setq new-indexing
	    (make-qda-indexing
	     :name (qda-doc-name doc)
	     :text-units
	     (sort (nset-difference
		    (qda-indexing-text-units this-indexing)
		    text-units)
		   'qda-sort-text-unit-func)))
      ;; kill the doc if there are no units left
      (if (null (qda-indexing-text-units new-indexing))
	  (delete this-indexing indexing-list)
	(nsubstitute new-indexing this-indexing indexing-list)))))

(defun qda-indexing-spread (indexing text-units spread)
  "Expand INDEXING TEXT-UNITS by SPREAD either side of the text unit."
  (let ((limit (qda-doc-text-unit-count (qda-indexing-name indexing)))
	tu ret-list)
    (loop
     for i in text-units
     do (loop for j from  (- i spread) to (+ i spread)
	      while (<= j limit)
	      unless (or (<= j 0)
			 (member* (setq tu (make-qda-text-unit
					    :number j))
				  ret-list :test 'qda-indexing-text-unit=))
	      do (push tu ret-list))
     finally return (nreverse ret-list))))

(defun qda-spread-indexing (doc indexing-list text-units spread)
  "Add SPREAD text units either side of TEXT-UNITS from doc in INDEXING-list."
  (unless (setq doc (qda-find-doc doc))
    (qda-error "Can't find doc %s" doc))
  (let (this-indexing
	new-indexing)
    (setq text-units (qda-make-uncompressed-text-unit-list text-units))
    (qda-validate-text-units text-units doc)
    (if (null (setq
	       this-indexing
	       (qda-find-indexing doc indexing-list)))
	indexing-list
      (nsubstitute
       (make-qda-indexing
	:name (qda-doc-name doc)
	:text-units (sort
		     (delete-duplicates
		      (nunion
		       (qda-indexing-spread this-indexing text-units spread)
		       (qda-indexing-text-units this-indexing)))
		     'qda-sort-text-unit-func))
       this-indexing
       indexing-list))))

;;; set functions

;;  These are substantially borrowed from cl-seq.el
;;  todo: add some of the more useful set operations
;;  from NUD.IST such as within and ovelap.
(defun qda-indexing-union (i1 i2)
  "Return indexing `union' of I1 and I2."
  (cond ((null i1) i2) ((null i2) i1)
	((equal i1 i2) i1)
	(t (let ((doc nil)
		 (res '()))
	     (or (>= (length i1) (length i2))
		 (setq i1 (prog1 i2 (setq i2 i1))))
	     (while i2
	       (when  (setq doc (car (member* (car i2) i1 :test 'qda-indexing-doc=)))
		 (setq i1 (remove* doc i1 :test 'qda-indexing-doc=)))
	       (push
		(make-qda-indexing
		 :name (qda-indexing-name (car i2))
		 :text-units (if doc
				 (sort
				  (union (qda-indexing-text-units doc)
					 (qda-indexing-text-units (car i2))
					 :test 'qda-indexing-text-unit=)
				  'qda-sort-text-unit-func)
			       (qda-indexing-text-units (car i2))))
		res)
	       (pop i2))
	     ;; this should preserve the order of i1 and i2
	     ;; roughly...
	     (append (nreverse res) i1)))))

(defun qda-indexing-set-difference (i1 i2)
  "Return indexing `set-difference' of I1 and I2."
  (if (or (null i1) (null i2)) i1
    (let ((tu '())
	  (doc nil)
	  (res '()))
      (while i1
	(setq doc (car (member* (car i1) i2 :test 'qda-indexing-doc=)))
	(setq tu (set-difference (qda-indexing-text-units (car i1))
				 (qda-indexing-text-units doc)
				 :test 'qda-indexing-text-unit=))
	(cond ((and (null doc) tu)
	       (push (car i1) res))
	      (tu (push (make-qda-indexing
			 :name (qda-indexing-name (car i1))
			 :text-units (sort tu 'qda-sort-text-unit-func))
			res)))
	(pop i1))
      (nreverse res))))

(defun qda-indexing-intersection (i1 i2)
  "Return indexing `intersection' of I1 and I2."
  (and i1 i2
       (if (equal i1 i2) i1
	 (let ((doc nil)
	       (tu '())
	       (res '()))
	   (or (>= (length i1) (length i2))
	       (setq i1 (prog1 i2 (setq i2 i1))))
	   (while i2
	     (and (setq doc (car (member* (car i2) i1 :test 'qda-indexing-doc=)))
		  (setq tu (intersection (qda-indexing-text-units (car i2))
					 (qda-indexing-text-units doc)
					 :test 'qda-indexing-text-unit=))
		  (push (make-qda-indexing
			 :name (qda-indexing-name doc)
			 :text-units
			 (sort tu 'qda-sort-text-unit-func))
			res))
	     (pop i2))
	   (nreverse res)))))

(defun qda-indexing-set-exclusive-or (i1 i2)
  "Return indexing `set-exclusive-or' of I1 and I2."
  (cond ((null i1) i2) ((null i2) i1)
	((equal i1 i2) nil)
	(t (qda-indexing-union (qda-indexing-set-difference i1 i2)
			       (qda-indexing-set-difference i2 i1)))))

  
;;; interactive functions

(defvar qda-view-indexing-format-string
  "\n[%s: %d]\n\n"
  "Format for inserting doc and text unit number in qda-view-indexing mode.
Should have specifier for the name and the number in that order.")

(defvar qda-view-indexing-regexp "^\\[\\(.+\\): \\([0-9]+\\)\\]"
  "Regexp for indexing file and text unit number information.
Should complement `qda-view-indexing-format-string' and allow the extraction
of doc name and text unit number in that order")

(defvar qda-view-doc-name)		;for the compiler

(defun qda-get-this-text-unit-number-and-doc ()
  "Return text unit number and name of text at point as a list or nil if not found."
  (save-excursion
    (let (num name)
      (cond ((eq major-mode 'qda-view-doc-mode)
	     (if (re-search-backward qda-view-doc-regexp nil t)
		 (list (string-to-int
			(buffer-substring (match-beginning 1) (match-end 1)))
		       qda-view-doc-name) ; buffer local
	       (list nil qda-view-doc-name)))
	    ((eq major-mode 'qda-view-indexing-mode)
	     (when (re-search-backward qda-view-indexing-regexp nil t)
	       (setq num (string-to-int
			  (buffer-substring-no-properties
			   (match-beginning 2) (match-end 2))))
	       (setq name
		     (buffer-substring-no-properties
		      (match-beginning 1) (match-end 1)))
	       (if (or (not num) (not name))
		   nil
		 (list num name))))))))

(defun qda-add-this-indexing (indexing-list)
  "Add indexing at point to INDEXING-LIST."
  (let ((n-and-d (qda-get-this-text-unit-number-and-doc)))
    (unless n-and-d
      (qda-error "I see no text unit number here"))
    (qda-add-indexing
     (second n-and-d)
     indexing-list
     (first n-and-d))))

(defun qda-delete-this-indexing (indexing-list)
  "Delete indexing at point from INDEXING-LIST."
  (let ((n-and-d (qda-get-this-text-unit-number-and-doc)))
    (unless n-and-d
      (qda-error "I see no text unit number here"))
    (qda-delete-indexing
     (second n-and-d)
     indexing-list
     (first n-and-d))))

;;; Viewing

;; These are just the general functions that handle indexing
;; Functions specific to a data type (doc node ...) are
;; in the appropriate files.

(defun qda-insert-indexing (indexing-list)
  "Insert indexing information from INDEXING-LIST into the current buffer.
Uses `qda-view-indexing-format-string' for formatting of the text unit
markers."
  (let (this-doc n)
    (dolist (this-indexing indexing-list)
      (setq this-doc (qda-find-doc (qda-indexing-name this-indexing)))
      (dolist (this-text-unit (qda-indexing-text-units this-indexing))
	;; n is the next text-unit
	(setq n (qda-nth-text-unit
		 (+ 1 (qda-text-unit-number this-text-unit)) this-doc))
	(insert (format qda-view-indexing-format-string
			(qda-doc-name this-doc)
			(qda-text-unit-number this-text-unit)))
	(insert-file-contents
	 (qda-doc-file this-doc) nil
	 (position-bytes (qda-nth-text-unit
			  (qda-text-unit-number this-text-unit) this-doc))
	 (if n (position-bytes n) nil))
	(goto-char (point-max))))))

;;; (defun qda-insert-indexing (indexing-list)
;;;   "Insert indexing information from INDEXING-LIST into the current buffer.
;;; Uses `qda-view-indexing-format-string' for formatting of the text unit
;;; markers."
;;;   (let ((tmp-buf (generate-new-buffer qda-temp-buffer))
;;; 	(view-buf (current-buffer))
;;; 	this-doc n doc-end)
;;;     (dolist (this-indexing indexing-list)
;;;       (setq this-doc (qda-find-doc (qda-indexing-name this-indexing)))
;;;       (set-buffer tmp-buf)
;;;       ;; why do we have to delete the last stuff?; I thought
;;;       ;; insert-file-contents did this
;;;       (erase-buffer)
;;;       (insert-file-contents (qda-doc-file this-doc) nil nil nil t)
;;;       (goto-char (point-max))
;;;       (skip-chars-backward qda-ignored-chars)
;;;       (setq doc-end (point))
;;;       (set-buffer view-buf)
;;;       (dolist (this-text-unit (qda-indexing-text-units this-indexing))
;;; 	;; n is the next text-unit
;;; 	(setq n (qda-nth-text-unit
;;; 		 (+ 1 (qda-text-unit-number this-text-unit)) this-doc))
;;; 	(insert (format qda-view-indexing-format-string
;;; 			(qda-doc-name this-doc)
;;; 			(qda-text-unit-number this-text-unit)))
;;; 	(insert-buffer-substring
;;; 	 tmp-buf
;;; 	 (qda-nth-text-unit (qda-text-unit-number this-text-unit) this-doc)
;;; 	 (if n (- n 1) doc-end))))
;;;     (kill-buffer tmp-buf)))

(defun qda-view-this-doc ()
  "View document from which indexing at point came.
Jumps to the text-unit at point in the doc"
  (interactive)
  (unless (eq major-mode 'qda-view-indexing-mode) ;this is the only valid mode
    (qda-error "I see no indexing here"))
  (let ((n-and-d (qda-get-this-text-unit-number-and-doc)))
    (unless n-and-d
      (qda-error "I see no indexing here"))
    (qda-view-doc (second n-and-d))
    (goto-char (point-min))		;todo: should write qda-goto-text-unit
    (qda-forward-text-unit (first n-and-d))))

;; these two are not quite right: they fail
;; if point is just before the text unit marker
(defun qda-forward-text-unit (n)
  "Go forward N text units.  N can be negative.
When called interactively, N can be supplied by a prefix argument.
Returns number of text units actually advanced."
  (interactive "p")
  (let ((count (abs n))
	(dir (if (> n 0) 1 -1))
	(func (if (> n 0)
		  're-search-forward
		're-search-backward)))
    (while (and
	    (> count 0)
	    (funcall func
		     (cond ((eq major-mode 'qda-view-indexing-mode)
			    qda-view-indexing-regexp)
			   ((eq major-mode 'qda-view-doc-mode)
			    qda-view-doc-regexp)
			   (t (return nil)))
		     nil t))
      (decf count)
      (goto-char (if (= 1 dir)
		     (match-end 0)
		   (match-beginning 0))))
    (when (interactive-p)
      (recenter 0))
    (- (abs n) count)))

;; todo: should go to (point-min) if there are no more text units?
(defun qda-backward-text-unit (n)
  "Go backward N text units.  N can be negative.
When called interactively, N can be supplied by a prefix argument.
Returns number of text units actually advanced."
  (interactive "p")
  (prog1
      (qda-forward-text-unit (- 0 n))
    (when (interactive-p)
      (recenter 0))))

;;; marking text-units

;; in the following two, matches are set in the calling
;; function. match-beginning is used as an assoc key to
;; the overlay

(defvar text-unit-overlays)

(defun qda-highlight-text-unit ()
  "Apply highlight to text unit."
  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
    (overlay-put ov 'face (cons 'foreground-color qda-highlight-mark-text-unit-colour))
    (push (cons (match-beginning 0) ov) text-unit-overlays)))

(defun qda-remove-highlight-text-unit ()
  "Remove highlight from text unit."
  (let ((ov (assoc (match-beginning 0) text-unit-overlays)))
    (when ov
      (delete-overlay (cdr ov))
      (setq text-unit-overlays (delete ov text-unit-overlays)))))

(defvar marked-indexing)

(defun qda-mark-text-unit ()
  "Mark the text unit at point for future operations."
  (interactive)
  (let ((n-and-d (qda-get-this-text-unit-number-and-doc)))
    (unless n-and-d
      (qda-error "I see no text unit number here"))
    (setq marked-indexing
	  (qda-add-indexing
	   (second n-and-d)
	   marked-indexing
	   (first n-and-d)))
    (qda-highlight-text-unit)))

(defun qda-unmark-text-unit ()
  "Remove mark from text unit at point."
  (interactive)
  (let ((n-and-d (qda-get-this-text-unit-number-and-doc)))
    (unless n-and-d
      (qda-error "I see no text unit number here"))
    (setq marked-indexing
	  (qda-delete-indexing
	   (second n-and-d)
	   marked-indexing
	   (first n-and-d)))
    (qda-remove-highlight-text-unit)))

(defun qda-mark-all-text-units ()
  "Mark all text units in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
	(= 1 (qda-forward-text-unit 1))
      (qda-mark-text-unit))))

(defun qda-unmark-all-text-units ()
  "Remove marks from all text units in buffer."
  (interactive)
  (mapc #'(lambda (ov)
	   (delete-overlay (cdr ov)))
	text-unit-overlays)
  (setq text-unit-overlays '())
  (setq marked-indexing '()))

(defun qda-view-search-mark-regexp (regexp)
  "Mark text units in the current buffer that match REGEXP."
  (interactive "sRegexp: ")
  (message "Searching...")
  (let ((n 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
	(incf n)
	(qda-mark-text-unit)
	(qda-forward-text-unit 1))
      (qda-message "...%d %s marked"
		   n (sing-or-plural-noun "text unit" "text units"
					  n)))))

(provide 'qda-indexing)

;;; qda-indexing.el ends here
