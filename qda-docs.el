;;; qda-docs.el --- qda doc stuff

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


;;; History:
;; 

;;; Todo:
;;
;; Think about changing the way that text units are handled.  at the
;; moment positions in the file are recorded.  This may have an
;; advantage in speed of access, but it means that all of the
;; positions have to be kept in the doc structure and that text units
;; can't be altered once they are in the project (like with NUD.IST)

(eval-when-compile
  (require 'cl)
  (require 'qda-misc)
  (require 'qda-tree)		;for the qda-dotree macro
  (defvar qda-root)
  (defvar indexing-list)
  (defvar qda-view-doc-name)
  (defvar indexing)
  (defvar qda-view-doc-node-indexing-doc-name))


;;; Code:

(defgroup qda-doc nil "Documents in QDA" :group 'qda)

(defcustom qda-text-unit-delimiter "^$"
  "*The default regexp delimiting text-units."
  :group 'qda-doc
  :type 'string)

(defvar qda-doc-list '()
  "The database of files introduced to the project.
This is a list of document-record structures")

(defstruct qda-doc
  name
  file
  text-units
  (slots '()))

(defun qda-doc-name-p (name)
  "Return t if NAME is the name of an existing doc, else nil."
  (if (qda-find-doc name)
      t
    nil))

(defun qda-doc-init ()
  "Initialise the global doc list."
  (setq qda-doc-list '()))

(defun qda-nth-text-unit (n doc)
  "Return the file position of the Nth text-unit in DOC.
N starts from 1, not 0."
  (nth (- n 1) (qda-doc-text-units doc)))

(defvar qda-ignored-chars " \t\n"
  "*Characters ignored at the beginning and the end of docs.
By default it is whitespace.")

;; adapted from how-many in replace.el
;; todo: if there is already a doc that uses this file
;; use its value to save time.  Although...if the file has changed
;; we might want to change the old values instead
(defun qda-parse-text-units (file)
  "Return a list of the positions of text units in FILE."
  (let ((text-unit-list '())
	(buf (set-buffer (generate-new-buffer qda-temp-buffer)))
	opoint end-of-text)
    (insert-file-contents file nil nil nil t)
    (goto-char (point-max))
    (skip-chars-backward qda-ignored-chars)
    (setq end-of-text (point))
    (goto-char (point-min))
    (skip-chars-forward qda-ignored-chars)
    (push (point) text-unit-list)
    (while (and (not (eobp))
		(progn
		  (setq opoint (point))
		  (re-search-forward qda-text-unit-delimiter end-of-text t)))
      ;; I am not convinced that this
      ;; does what I mean it to do, but it does !...
      (if (= opoint (point))
	  (forward-char 1)
	;; this is arbitrary, but if there are adjacent
	;; duplicates of the delimiter, it is the last
	;; one that counts.
	(push (point) text-unit-list)))
    (kill-buffer buf)
    (nreverse text-unit-list)))
	       
(defun qda-doc-text-unit-count (doc)
  "Return the number of text units in DOC."
  (length (qda-doc-text-units (qda-find-doc doc))))

;; Fri Nov 24 16:12:38 2000
;;
;; todo: this isn't consistent with the indexing structure (change
;; this and its use below (I don't think it's used elsewhere)).  Not
;; really that important, but it took a while to realize that this was
;; what was going on when I was fiddling with this.

(defun qda-doc-node-indexing (doc &optional node)
  (when (qda-doc-p doc) (setq doc (qda-doc-name doc)))
  (let ((indexing-list '()) (indexing '()))
    (qda-dotree (n (or node qda-root))
      (when (setq indexing (qda-find-indexing doc (qda-node-indexing n)))
 	(setq indexing-list
 	      (cons
 	       (cons
 		(qda-node-numerical-address n)
 		(qda-indexing-text-units indexing))
 	       indexing-list))))
    (nreverse indexing-list)))

(defun qda-find-doc (doc)
  "Find DOC in the global doc list.
DOC may be either a name or a qda-doc structure.
Return the record or nil."
  (cond ((qda-doc-p doc)
	 (if (member* doc qda-doc-list :test 'equal) doc nil))
	((not (stringp doc)) nil)
	(t (loop
	    for this-doc in qda-doc-list
	    thereis (if (string= (qda-doc-name this-doc) doc)
			this-doc
		      nil)))))

(defun qda-find-doc-by-file-name (file-name)
  "Find a doc in the global doc list that has the file FILE-NAME.
Returns the record or nil."
  (cond ((null file-name) nil)
	(t (setq file-name (expand-file-name file-name))
	   (loop
	    for this-doc in qda-doc-list
	    thereis (if (string= (qda-doc-file this-doc) file-name)
			this-doc
		      nil)))))

(defun qda-doc-readable-p (doc)
  "Return t if file associated with DOC is readable, or nil."
  (if (file-readable-p (qda-doc-file doc)) t nil))

(defun qda-completing-read-doc (prompt &optional default)
  "Complete using names in the global doc list.
The string PROMPT is used as a prompt.
When DEFAULT is nil, supply a value derived from the current context.
When DEFAULT is non-nil, it is the default value."
  (condition-case err
      (progn
	(unless default
	  (setq default (second (qda-get-this-text-unit-number-and-doc))))
	(when default
	  (and (setq default (qda-find-doc default))
	       (setq default (qda-doc-name default))))
	(let ((doc
	       (completing-read
		prompt
		(loop
		 for this-doc in qda-doc-list
		 collect (list (qda-doc-name this-doc)))
		nil t default nil)))
	  (when (string= doc "")
	    (error err))
	  doc))
    (error (progn
	     (message "Please enter a document")
	     (sit-for 1)
	     (qda-completing-read-doc prompt)))))

(defun qda-introduce-doc (file name)
  "Introduce FILE with name NAME to the doc list."
  (interactive "fFile: \nsName: ")
  (unless (stringp name)
    (qda-error "Name must be a string"))
  (when (qda-find-doc name)
    (qda-error  "The name %s is already in use" name))
  (when (qda-find-doc-by-file-name file)
    (qda-error "File %s is already in the project as %s"
	       file
	       (qda-find-doc-by-file-name file)))
  (unless (file-exists-p file)
    (qda-error "The file %s does not exist" file))
  (unless (file-readable-p file)
    (qda-error "You do not have permission to read the file %s" file))
  (push (make-qda-doc
	 :file (expand-file-name file)
	 :name name
	 :text-units (qda-parse-text-units file))
	qda-doc-list)
  (qda-message "File %s introduced as %s." (expand-file-name file) name))

(defun qda-introduce-this-doc (name)
  "Add the file in the current buffer to the doc list with name NAME."
  (interactive "sName for this doc:  ")
  (let ((this-file (buffer-file-name)))
    (unless this-file
      (qda-error "There is no file attached to this buffer"))
    (qda-introduce-doc this-file name)))

;; todo: this doesn't do enough work! it leaves
;; references to docs in indexing lists
(defun qda-delete-doc (name &optional no-confirm)
  "Delete doc with name NAME from the doc list.
When called non-interactively, non-nil optional argument NO-CONFIRM
means don't ask for confirmation."
  (interactive
   (list
    (qda-completing-read-doc "Document to remove: ")))
  (let ((doc (qda-find-doc name)))
    (unless doc (qda-error "Can't find %s to remove" name))
    (if (or no-confirm
	    (yes-or-no-p (format "Really remove %s? " name)))
	(progn
	  (setq qda-doc-list (delete doc qda-doc-list))))))


;;; viewing

;; todo: if there is a memo for the doc, put it in the header
;;       also, perhaps indexing info.
(defun qda-view-doc-insert-header (doc)
  "Insert a header into the doc display buffer.
DOC is the doc displayed in the buffer."
    (goto-char (point-min))
    (skip-chars-forward qda-ignored-chars)
    (delete-region (point-min) (point))
    (insert ""))			;nothing at the moment

(defun qda-view-doc-insert-footer (doc)
  "Insert a footer into the doc display buffer.
DOC is the doc displayed in the buffer."
  (goto-char (point-max))
  (skip-chars-backward qda-ignored-chars)
  (delete-region (point) (point-max))
  (insert ""))

(defvar qda-view-doc-format-string "\n[%s]\n"
  "String format for inserting text unit number in a buffer in `qda-view-doc' mode.")

(defvar qda-view-doc-regexp "\\[\\([0-9]+\\)\\]"
  "Regexp for extracting the text unit number from `qda-view-doc-format-string'.")

(defun qda-insert-text-unit-numbers (doc)
  "Add text unit numbers to display of DOC."
  (let ((count 0)
	(tot-xtra 0)
	tunstr)
    (dolist (this-text-unit (qda-doc-text-units doc))
      (incf count)
      ;; we need to compensate for the added length
      ;; of the text-unit numbering!!
      (setq tunstr (format qda-view-doc-format-string count))
      (goto-char (+ this-text-unit tot-xtra))
      ;; this doesn't work quite right:
      ;; the first text-unit is (potentially) different
      ;; from all of the rest because it is not preceded
      ;; by a newline...I would just insert a newline, but
      ;; that makes assumptions about the delimiter.
      ;;
      ;; todo: sort this out.
      (insert tunstr)
      (incf tot-xtra (length tunstr)))
    (goto-char (point-min))))

(defun qda-view-doc (name)
  "Load file associated with doc called NAME into a buffer, add text unit numbers."
  (interactive
   (list (completing-read
	  "Document: "
	  (loop
	   for this-doc in qda-doc-list
	   collect (list (qda-doc-name this-doc)))
	  nil t)))
   (let ((doc (qda-find-doc name))
	 buf)
     (unless doc
       (qda-error "Can't find doc %s" name))
     ;; just check that the file is still there
     (unless (qda-doc-readable-p doc)
       (qda-error "Can't read file %s" (qda-doc-file doc)))
     (if (get-buffer name)
	   (pop-to-buffer (get-buffer name))
       (setq buf (get-buffer-create name))
       (set-buffer buf)
       (insert-file-contents (qda-doc-file doc))
       (qda-insert-text-unit-numbers doc)
       (qda-view-doc-insert-footer doc)
       (qda-view-doc-insert-header doc)
       (qda-view-doc-mode)
       ;; set buffer local variable that tells functions
       ;; the name of this doc
       (setq qda-view-doc-name (qda-doc-name doc))
       (pop-to-buffer buf))))


(defvar qda-doc-indexing-buffer "*QDA-doc-indexing*")

;; todo: make this visually consistent with viewing
;; node info.
(defun qda-view-doc-node-indexing (doc)
  "Show all the indexing of DOC in the tree."
  (interactive
   (list (qda-completing-read-doc "Document: ")))
  (let ((buf (set-buffer (get-buffer-create qda-doc-indexing-buffer)))
	(indexing (setq indexing (qda-doc-node-indexing doc))))
    (cond ((null indexing)
	   (qda-message "Document %s is indexed nowhere." doc)
	   (sit-for 1))
	  (t (setq buffer-read-only nil)
	     (delete-region (point-min) (point-max))
	     ;; todo: make the formatting much nicer.
	     (insert (format "Document %s\n%s\n\n"
			     doc qda-line-string))
	     (dolist (this-indexing indexing)
	       (insert (format "%s\n  %s\n  text units: %s\n"
			       (car this-indexing)
			       (qda-node-path-name (qda-find-node (car this-indexing)))
			       (if (= (length (cdr this-indexing))
				      (qda-doc-text-unit-count doc))
				   'all
				 (length (cdr this-indexing))))))
	     (insert (format "\n%s" qda-line-string))
	     (goto-char (point-min))
	     (qda-view-doc-node-indexing-mode)
	     (setq qda-view-doc-node-indexing-doc-name doc)
	     ;; should we switch to this buffer?
	     (display-buffer buf)))))

(defun qda-view-this-doc-node-indexing ()
  "Show where doc is indexed in the tree."
  (interactive)
  (unless (eq major-mode 'qda-view-doc-mode)
    (qda-error "I see no document here"))
  (qda-view-doc-node-indexing qda-view-doc-name))

(defvar qda-view-doc-indexing-node-regexp "^([0-9 ]+)")

(defun qda-view-doc-indexing-find-node-indexing ()
  (interactive)
  (let ((point (point)))
    (end-of-line)
    (if (re-search-backward qda-view-doc-indexing-node-regexp nil t)
	(progn
	  (goto-char point)
	  (let ((doc qda-view-doc-node-indexing-doc-name))
	    (qda-view-node-indexing (read (match-string 0)))
	    ;; this is a crude search, but anything else would get
	    ;; fiddly (checking that the indexing still exists, etc.)
	    ;; it should be fine usually.
	    (search-forward doc nil t)
	    (end-of-line)))
      (goto-char point))))

(defun qda-doc-node-indexing-by-text-unit (doc &optional node)
  "Return an array of all the nodes which index text units in DOC.

Optional argument NODE specifies the root node of the tree to start
from.

The zeroth element of the array is a list of nodes that have all of
the text units in DOC indexed to them.  The remaining elements
correspond to the text unit numbers of DOC and contain a list of all
nodes in the tree that index this text unit.

Note: the node lists in the returned array are in reverse order.  This
function is already quite slow, and making the lists come out in
ascending order would make it still slower.  It is better to share the
cost of this function with that of any function which might call it."
  (when (qda-doc-p doc) (setq doc (qda-doc-name doc)))
  (setq node (or node qda-root))
  (let ((text-units (make-vector (1+ (qda-doc-text-unit-count doc)) nil))
	this-indexing)
    (qda-dotree (n node)
      (setq this-indexing
	    (qda-indexing-text-units
	     (qda-find-indexing doc (qda-node-indexing n))))
      ;; if all text units in the doc are indexed to this node, add it
      ;; to the zeroth element.
      (if (= (qda-doc-text-unit-count doc) (length this-indexing))
	  (push n (aref text-units 0))
	(dolist (tu this-indexing)
	  (push n (aref text-units tu)))))
    text-units))

(defvar qda-doc-text-unit-node-all-info-buffer "*QDA-doc-info*")


(defcustom qda-all-indexing-face-foreground "Thistle"
  "*The colour for nodes indexing *all* of a document's text units.

This face is used in `qda-doc-show-where-text-unit-is-indexed'."
  :group 'qda-doc
  :type 'string)

(make-empty-face 'qda-all-indexing-face)
(set-face-foreground 'qda-all-indexing-face
		     qda-all-indexing-face-foreground)

(defun qda-doc-text-unit-index-insert-node (node)
  (let ((node-num-format-string
	 (qda-node-number-with-padding-format-string qda-root 3)))
    (insert (concat
	     (format node-num-format-string
		     (qda-node-numerical-address node))
	     (qda-node-name node)
	     "\n"))))

(defun qda-doc-show-where-text-unit-is-indexed-insert (node-index-array tu)
  (dolist (n (reverse (aref node-index-array 0)))
    (qda-doc-text-unit-index-insert-node n))
  (unless (= (point) (point-min))
    (put-text-property (point-min) (point) 'face 'qda-all-indexing-face))
  (save-excursion
    (dolist (n (reverse (aref node-index-array tu)))
      (qda-doc-text-unit-index-insert-node n))
  ;; lazy way of getting rid of final newline
    (delete-backward-char 1)))

(defun qda-doc-show-where-text-unit-is-indexed (doc text-unit)
  (interactive
   (list
    (qda-completing-read-doc "Document: ")
    (qda-read-number "Text unit: ")))
  (let* ((d (or (qda-find-doc doc) (qda-error "Can't find doc %s" doc)))
	 (tu (if (<= text-unit (qda-doc-text-unit-count d))
		 text-unit
	       (qda-error "Text unit %d out of range for doc %s" text-unit
			  (qda-doc-name d))))
	 (pop-up-frames nil))
    (set-buffer (get-buffer-create qda-doc-text-unit-node-all-info-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (qda-doc-show-where-text-unit-is-indexed-insert
     (if (boundp 'qda-doc-node-indexing-by-text-unit-list)
	 ;; todo: someday add caching to doc buffers.
	 qda-doc-node-indexing-by-text-unit-list
       (qda-doc-node-indexing-by-text-unit d)) tu)
    (qda-view-doc-node-indexing-mode)
    (setq qda-view-doc-node-indexing-doc-name (qda-doc-name d))
    (shrink-window-if-larger-than-buffer
	 (display-buffer qda-doc-text-unit-node-all-info-buffer))))


(defun qda-doc-show-where-this-text-unit-is-indexed ()
  (interactive)
  (let ((doc-text-unit (qda-get-this-text-unit-number-and-doc)))
    (unless (and (cadr doc-text-unit) (car doc-text-unit))
      (qda-error "I see no text unit here"))
    (qda-doc-show-where-text-unit-is-indexed
     (cadr doc-text-unit) (car doc-text-unit))))


;;; searching

;; Fri Nov 24 16:22:43 2000 is this actually used anywhere ?
;; Presumably it's intended as a part of `qda-search' (catching errors
;; because of missing docs, while doing all valid searches)?

(defun qda-doc-sublist (name-list)
  "Return a list of all the docs in NAME-LIST with a match in the
global doc-list.

Return a list with two members, the matching docs and names with no
docs that match: \((DOC1 DOC2 ...) (NAME1 NAME2 ...))."
    (loop
     with doc
     for name in name-list
     if (setq doc (qda-find-doc name))
        collect doc into found
     else
        collect name into not-found
     finally return (list found not-found)))


(defvar qda-last-search '())

(defconst qda-search-options-alist
  '(("regexp" . :regexp)
    ("case-sensitive" . :case-sensitive)
    ("whole-doc" . :whole-doc)
    ("whole-word" . :whole-word)))


(defun qda-search (search-terms)
  "Return a list of lists of finds of SEARCH-TERMS in the doc list.
Each element of the list has the search term as car and an indexing list
of finds as cdr.

SEARCH-TERMS is a list of search-terms. Each search term
is a list with the string as car and options as cdr:
               ((\"pies\") (\"^[a-zA-Z]+$\" :regexp)...).

A single search term may be supplied like this:
               (qda-search (list \"pies\" :whole-word))

i.e., as a list of atoms, rather than a list with just one element.

The global variable qda-last-search is set to the result.
This makes it available for adding to an indexing list.

The options are:

  :regexp Search term is a regular-expression.
  :case-sensitive Do case sensitive search.
  :whole-word Search for whole words.
  :whole-doc Index the whole doc if there are any matches.

All of these are off by default.

NOTE: it is quicker to do lots of searches in one go than to call the
function several times because the searches are done in parallel."
  (interactive (list (qda-search-terms-minibuffer)))
  (when (atom (car search-terms))
    (setq search-terms (list search-terms)))
  (let ((buf (set-buffer (generate-new-buffer qda-temp-buffer)))
	(finds-list (make-list (length search-terms) '()))
	(t-units '())
	(this-search-term nil)
	(next-tu nil)
        search-func)
    ;; doc loop
    (dolist (this-doc qda-doc-list)
      (insert-file-contents (qda-doc-file this-doc) nil nil nil t)
      ;; search-term loop
      (dotimes (m (length search-terms))
	(goto-char (point-min))
	(setq t-units '())
	(setq this-search-term (nth m search-terms))
	(if (memq :case-sensitive this-search-term)
	    ;; Sat Sep  9 12:24:42 2000: is this right?
	    (setq case-fold-search nil)
	  (setq case-fold-search t))
	(when (memq :whole-word this-search-term)
	  (setq this-search-term
		(list (format "\\W%s\\W" (car this-search-term))
		      :regexp
		      (cdr this-search-term))))
	(setq search-func (if (memq :regexp this-search-term)
			      're-search-forward
			    'search-forward))
	;; text-units in doc loop
	;; with whole-doc, we short-circuit if there is a find
	;; and just make a list of all text-units.
	(catch 'qda-whole-doc-found
	  (dotimes (n (qda-doc-text-unit-count this-doc))
	    (setq next-tu (nth (+ 1 n) (qda-doc-text-units this-doc)))
	    (when (funcall search-func (car this-search-term) next-tu t)
 	      (when (memq :whole-doc this-search-term)
		(throw 'qda-whole-doc-found
 		       (setq t-units
			     ;; Thu Nov 23 18:13:46 2000
			     ;; todo: this could be changed to `qda-all-indexing'
			     ;; now that I have added ranges: but this
			     ;; involves other changes down below.
			     (loop
			      for i from (qda-doc-text-unit-count this-doc) downto 1
			      collect (make-qda-text-unit :number i)))))
	      (push (make-qda-text-unit :number (+ n 1)) t-units))
	    (goto-char (or next-tu 1))))
	(when t-units
          ;; here is where code for restriction nodes would be.
	  (push (make-qda-indexing
		 :name (qda-doc-name this-doc)
		 :text-units (reverse t-units))
		(nth m finds-list))))
      (erase-buffer))
    (kill-buffer buf)
    (setq qda-last-search
	  (mapcar* #'(lambda (s f)
		      (setq f (nreverse f))
		      (push s f))
		   search-terms
		   finds-list))))


(defun qda-read-options (search-term)
  "Read search options for SEARCH-TERM from the minibuffer."
  (let ((options '())
	this-opt)
    (while (not (null-string-p
		 (setq this-opt
		       (completing-read
			(format
			 "Search options for \"%s\" (or blank line when finished): "
			 search-term)
			;; these have to be kept in sync with any changes
			;; made to qda-search.  Any way of automating this?
			qda-search-options-alist
			nil t nil nil))))
      (push (cdr (assoc this-opt qda-search-options-alist)) options))
    options))

(defun qda-search-terms-minibuffer ()
 "Read search terms from the minibuffer."
 (let (term
       options
       (term-list))
   (while (not (string-match
		"^[ \t]*$"
		(setq term (read-from-minibuffer
			    "Search term (or blank line when finished): "))))
     (setq options (qda-read-options term))
     (push (cons term options) term-list))
   (nreverse term-list)))


(defun qda-search-term-minibuffer ()
 "Read a search term from the minibuffer.
This is the same as `qda-search-terms-minibuffer', except that it asks for
just the one search term"
   (let ((term
	  (read-from-minibuffer "Search term: ")))
     (cons term
	   (qda-read-options term))))
    

;;; saving and loading
(defun qda-save-doc-list (file)
  "Save the global doc list in FILE."
  (interactive "FDoc file: ")
  (qda-save-object qda-doc-list file))

(defun qda-load-doc-list (file)
"Retrieve the global doc list from FILE."
  (interactive "fDoc File: ")
  ;; check that docs are all valid
  ;; just check that all members of the list
  ;; are qda-doc
  (setq qda-doc-list (qda-get-object file))
  (dolist (this-doc qda-doc-list)
    (unless (qda-doc-p this-doc)
      (qda-error "Invalid Doc list"))))



;;; memos

(defstruct (qda-doc-slot (:type list))
  type)

(defun qda-find-doc-slot-type (type doc)
  (let ((slots (qda-doc-slots doc)))
    (loop
     for slot in slots
     thereis (if (eq (qda-doc-slot-type slot) type)
		 slot
	       nil))))

(defstruct (qda-doc-slot-memo
	    (:include qda-doc-slot
		      (type 'memo))
	    (:type list))
  text)

(defun qda-doc-memo (doc)
  (qda-doc-slot-memo-text
   (qda-find-doc-slot-type 'memo doc)))
  
(defun make-qda-doc-memo (doc text)
  (car (push (make-qda-doc-slot-memo :text text)
	     (qda-doc-slots doc))))
       
(defun delete-qda-doc-memo (doc)
    (setf (qda-doc-slots doc)
	  (delete* (qda-find-doc-slot-type 'memo doc)
		   (qda-doc-slots doc) :test 'equal)))
	
(defun qda-doc-make-memo-identifier (doc)
  (qda-doc-name doc))

(defun qda-insert-doc-memo-header (doc)
  (insert (format "%s\n%s\n\n" (qda-doc-name doc) qda-line-string)))

(defun qda-doc-edit-memo (doc)
  (interactive
   (list
    (qda-completing-read-doc "Document: ")))
  (let ((d (qda-find-doc doc)))
    (if d
	(qda-memo-edit d)
      (qda-error "Can't find doc %s" doc))))

(defun qda-doc-memo-list-all ()
  (let (memos)
    (loop for doc in qda-doc-list
	  do (when (qda-doc-memo doc)
	       (setq memos
		     (concat
		      (format "\n%s\n%s\n%s\n"
			      (qda-doc-make-memo-identifier doc)
			      qda-line-string
			      (qda-doc-memo doc))
		      memos))))
    (when memos
      (insert (format "\nDocument memos\n%s\n%s\n\n"
		      qda-line-string
		      memos)))))
(provide 'qda-docs)

;;; qda-docs.el ends here
