;;; qda-tree.el --- qda tree functions

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
;;  Sat Aug  7 00:18:24 1999
;;  Added the updatable slot and a special updatable search slot.
;;  This looks very useful, especially for indexing base data.
;;
;;  Also wrote some basic stuff for dealing with slots in general.
;;
;;  Removed memo slot: it's obsolete now.
;;
;;  Sun Aug  8 23:58:51 1999
;;  Added memo handling functions for nodes and node-text-units
;;
;;  Tue Nov 28 15:09:11 2000
;;  replaced most of the recursive tree searching functions with
;;  iterative routines...much faster!
;;
;;  Sat Sep 22 15:53:51 2001
;;  Added unique-num slot to qda-node.
;;  Changed save/load routines to save and restore qda-node-counter.
;;

;;; Thoughts:
;;
;;  Saving the tree depends upon the printability and readability of
;;  the qda-node structure.  We shouldn't rely on this being true in
;;  the future.  (Also `qda-make-fresh-copy').

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'qda-misc)
  (require 'qda-indexing)
  (defvar marked-indexing)
  (defvar memo-slot)
  (defvar node)
  (defvar node-list)
  (defvar qda-ret-list)
  (defvar qda-set-func)
  (defvar qda-set-func-string))


(defmacro qda-defun-tree-mod (name args &rest forms)
   "Define a function NAME with ARGS, with a call to
`qda-tree-modified-hook' appended.  Use for functions that modify the tree."
  `(qda-defun-real ,name ,args nil 'qda-tree-modified-hook nil ,@forms))

(defmacro qda-defun*-tree-mod (name args &rest forms)
     "Define a function NAME with ARGS, with a call to
`qda-tree-modified-hook' appended.  Use for functions that modify the
tree.
This is the starred form of `qda-defun-tree-mod' for functions
requiring extended argument processing (keywords)."
  `(qda-defun-real ,name ,args nil 'qda-tree-modified-hook t ,@forms))

(defvar qda-view-tree-update-flag nil)
(defvar qda-node-mod-count 0)

;; keeps track of number of nodes created in project (for
;; qda-node-unique-num)
(defvar qda-node-counter 0)
					
(defun qda-tree-modified-hook ()
  "Do things recording changes to the tree."
  (setq qda-view-tree-update-flag t)
  (incf qda-node-mod-count))


(defstruct qda-node
   name
   ;; We have to have a record of the number of the node otherwise,
   ;; when new nodes are added or deleted the node number will shift
   ;; and it will be hard to keep track of the names of nodes.
   number
   ;; unique-num stays the same even if the node is moved.  It is
   ;; the nth node created in the project
   unique-num
   ;; Parent is pointed to by its node-address list.
   ;; todo: think about changing this
   (parent '())
   (children '())
   (indexing '())
   (description nil)
   ;; slots slot allows extension of the structure.  see the defstruct
   ;; for qda-node-slot for details, and the  code for updatable nodes
   ;; below for an example.
   (slots '()))

(defvar qda-root '())

(defun qda-make-root ()
  (setq qda-root (make-qda-node
		  :name "Root"
		  :number 0)))

;; these are all early in the file because they are in-line

(defsubst qda-nth-kid (n node)
  "Return the kid with number N under NODE."
  (dolist (this-kid (qda-node-children node))
	(when (= (qda-node-number this-kid) n)
	  (return this-kid))
 	nil))

(defsubst qda-last-index (node-address)
 "Return the number that is last in the list NODE-ADDRESS."
  (car (last node-address)))

(defsubst qda-last-kid (node)
  "Return the last kid of NODE."
  (car (last (qda-node-children node))))

(defsubst qda-node-parent-address (node-address)
  "Return the node address of node NODE-ADDRESS."
  (butlast node-address))

(defsubst qda-node-numerical-address (node)
  "Return numerical address of NODE."
  (cond ((= (qda-node-number node) 0) '(0))
	((= 0 (car (qda-node-parent node)))
	 (list (qda-node-number node)))
	(t (append (qda-node-parent node)
		(list (qda-node-number node))))))

(defsubst qda-node-level (node-address)
  (length node-address))

(defmacro qda-dotree (specs &rest body)
    "(qda-dotree (VAR TREE [RESULT]) BODY...) Walk through a tree.
Depth first.  Evaluate BODY with VAR bound to each node from TREE, in
turn.  Then evaluate RESULT to get return value, default nil."
  (let ((node-stack (gensym "node-stack"))
	(tree-tmp (gensym "tree-tmp"))
	(n (gensym "n"))
	(k (gensym "k")))
  `(let ((,node-stack '())
	 (,tree-tmp ,(nth 1 specs))
	 ,(car specs)
	 ,n ,k)
    (push (list ,tree-tmp) ,node-stack)
    (block nil
      (while ,node-stack
	(setq ,n (pop ,node-stack))
	(when (cdr ,n)
	  (push (cdr ,n) ,node-stack))
	(setq ,n (car ,n))
	(setq ,k (qda-node-children ,n))
	;; bind VAR to current node
	;; todo: should I make sure this is only evaluated once?
	;; `dolist' doesn't seem to worry about this.
	(setq ,(car specs) ,n)
	,@body
	(when ,k (push ,k ,node-stack)))
      ;; ...likewise (see above comment)
      ,(if (caddr specs)
	   `(progn
	     (setq ,(car specs) nil)
	     ,(caddr specs))
	 nil)))))

(put 'qda-dotree 'lisp-indent-function 1)

(defun qda-tree-depth (node)
  "Find depth of tree under NODE."
  (let ((depth 0))
    (qda-dotree (n node)
      ;; uses the node parent because this is already in numerical
      ;; form, so is quicker.
      (when (> (qda-node-level (qda-node-parent n)) depth)
	(setq depth (qda-node-level (qda-node-parent n)))))
    (1+ depth)))

;; The key accessing function for nodes by their numerical addresses.

;; Parent is indexed by a numerical address. The indexing will
;; start from 1 as this seems more intuitive than starting from
;; 0. An address of 0 means root. It is an error to have 0 as an
;; element of a list other than '(0).

(defsubst qda-find-node (node)
  "Find the NODE in the global node list.
NODE may be a numerical address or a qda-node structure.
Return nil if NODE is not found.
Nodes start from 1.
The address '(0) means the root node."
  (cond ((qda-node-p node) node)
	((equal node '(0)) qda-root)
	((or (null node)
	     (memq 0 node)) nil)
	(t (let ((n1 qda-root))
	     (if (loop
		  for n-num in node
		  always (setq n1 (qda-nth-kid n-num n1)))
		 n1
	       nil)))))

(defsubst qda-find-node-strict (node)
  "Find the NODE in the global node list.
NODE may be a numerical address or a qda-node structure.
Return nil if NODE is not found.
Nodes start from 1.
The address '(0) means the root node.
Like `qda-find-node' but with stricter type checking.
Unless you need the extra checking, use `qda-find-node', it's faster."
  (cond ((qda-node-p node)
	 (if (equal node
		    (qda-find-node (qda-node-numerical-address node)))
	     node
	   nil))
	((null node) nil)
	((not (listp node)) nil)
	((not (loop
	       for n in node
	       always (integerp n))) nil)
	((memq 0 (cdr node)) nil)
	((equal node '(0)) qda-root)
	(t (let ((n1 qda-root))
	     (if (loop
		  for n-num in node
		  always (setq n1 (qda-nth-kid n-num n1)))
		 n1
	       nil)))))

;; two other ways of finding a node

(defun qda-find-node-by-kid-name (parent name)
  "Find child of node PARENT with name NAME."
  (loop
   for kid in (qda-node-children parent)
   thereis (if (string= name (qda-node-name kid))
	       kid
	     nil)))

(defun qda-find-node-by-pathname (pathname)
  (if (equal pathname "//")
      qda-root
    (let ((n1 qda-root))
      (if (loop
	   for n-name in (cdr (split-string pathname "/"))
	   always (setq n1 (car (member* n-name (qda-node-children n1) :test
					 '(lambda (name n)
					    (equal name
						   (qda-node-name n)))))))
	  n1
	nil))))

(defsubst qda-node-extant-ancestor (node-address)
  "Return the closest ancestor of node whose address is NODE-ADDRESS.
Thus, if NODE-ADDRESS does not exist, but its parent does, return the parent.
If NODE-ADDRESS references an extant node, return the node."
  (let ((n-addr node-address)
	(n nil))
    (while (and n-addr
		(not (setq n (qda-find-node n-addr))))
      (setq n-addr (qda-node-parent-address n-addr)))
    n))

(defsubst qda-last-sib-p (node)
  (and (eq node qda-root) nil)
  (eq node (qda-last-kid (qda-find-node (qda-node-parent node)))))

(defsubst qda-kid-names (node)
  (loop
   for this-node in (qda-node-children node)
   collect (qda-node-name this-node)))

(defsubst qda-kid-numbers (node)
  (loop
   for this-node in (qda-node-children node)
   collect (qda-node-number this-node)))

(defsubst qda-next-kid-number (node)
  "Return + 1 the number of the last kid of NODE."
  (let ((last-node (qda-last-kid node)))
	(if last-node
		(1+ (qda-node-number last-node))
 	  1)))

(defsubst qda-next-sib (node)
  "Return the next sibling of NODE."
  (cadr (member* node (qda-node-children
		       (qda-find-node (qda-node-parent node)))
		 :test 'equal)))

(defun qda-map-nodes (func node)
  "Do depth first traversal of tree from NODE, evaluate FUNC on each node.
Returns a list of returned values from the function call."
  (let ((ret '()))
   (nreverse (qda-dotree (n node ret)
	       (push (funcall func n) ret)))))

(defun qda-node-last-descendant (node)
  "Return last descendant of NODE."
  (if (qda-node-children node)
      (qda-node-last-descendant (qda-last-kid node))
    node))

;; use when you need to find the node before getting kids

(defun qda-node-kids (node)
  (setq node (qda-find-node node))
  (if node
      (qda-node-children node)
    nil))

(defun qda-sort-node-numbers (n1 n2)
  (if (< (qda-node-number n1)
	 (qda-node-number n2))
      t
    nil))

;; This function could be extended to do more rigorous
;; checking.

(defun qda-verify-tree (&optional node)
  "Make sure NODE and its subtree is valid.
If node is nil, start at the root node."
  (qda-map-nodes
   #'(lambda (n)
       (unless (qda-node-p n)
	 (qda-error "invalid node: %s" n)))
   qda-root))

(defun qda-node-list ()
  (cdr (qda-map-nodes 'qda-node-numerical-address qda-root)))

(defun qda-nth-in-node-list (node)
  "Return the position of NODE in the list of node addresses."
  (position (qda-node-numerical-address node) (qda-node-list) :test 'equal))

(defun qda-node-completion-list ()
  "Return an alist for `completing-read' like this: ((\"()\")
\(\"(1)\") (\"(1 1)\") (\"(1 2)\") (\"(2)\")."
  (cons
   (list "()")
   (cdr (qda-map-nodes
	 #'(lambda (n)
	     (list
	      (format "%s" (qda-node-numerical-address n))))
	 qda-root))))

(defun qda-node-path-name (node)
  "Return path name of NODE."
  (let ((n (qda-find-node node))
	(path ""))
   (when
       (loop
	for n-a in (nreverse (qda-node-numerical-address node))
	always n
	do (setq path (concat "/" (qda-node-name n) path))
	   (setq n (qda-find-node (qda-node-parent n))))
     path)))



;;; Main functions: adding, deleting, copying, moving, etc.

;; todo: perhaps we could allow the creation of non-existing parents

(qda-defun*-tree-mod qda-add-node (name parent &key description)
  "Add a node with name NAME to PARENT. Return the new node.
Signals an error if PARENT doesn't exist or if there is a name clash.
If no parent is given, the root node is the default.
Argument &KEY DESCRIPTION gives the new node a description string."
  (interactive
   (list (qda-read-string "Name for new node: ")
	 (qda-completing-read-node "Parent: ")
	 :description (qda-read-string "Description: " nil t)))
  (let ((p (if parent (qda-find-node parent) qda-root)))
    (cond ((null p) (qda-error "Can't find node %s" parent))
	  ((member name (qda-kid-names p))
	   (qda-error "The name %s is already in use" name))
	  (t (let ((new-node (make-qda-node
			      :name name
			      :number (qda-next-kid-number p)
			      :unique-num (incf qda-node-counter)
			      :parent (or parent '(0))
			      :description description)))
	       (setf (qda-node-children p)
		     (nconc (qda-node-children p)
			    (list new-node)))
	       (qda-message "Node %s added to %s as %d."
			    (qda-node-name new-node)
			    (if (eq p qda-root)
				"()"	; another arbitrary representation
					; of root
			      (qda-node-parent new-node))
			    (qda-node-number new-node))
	       new-node)))))

(qda-defun-tree-mod qda-delete-node (node &optional no-confirm)
  "Delete NODE.  Return the revised parent or nil.
When called non-interactively non-nil argument to NO-CONFIRM means
don't ask for confirmation."
  (interactive
   (list (qda-completing-read-node "Node to delete: ")))
  (let* ((this-node (qda-find-node node))
	 (parent (and this-node (qda-find-node (qda-node-parent this-node)))))
    (when (null this-node)
      (qda-error "Can't find node %s to delete" node))
    (if (or no-confirm
	    (yes-or-no-p (format "Really delete node %s? " node)))
	(progn
	  (setf (qda-node-children parent)
		(delete this-node (qda-node-children parent)))
	  (qda-message "Node %s deleted."
		       (qda-node-numerical-address this-node)))
      nil)))

;; We have to do this because copy-qda-node returns a structure that
;; is eq to the original.  The function uses vconcat, exploiting
;; Elisp's simulation of structures using arrays (i.e. this is
;; something of a hack, and may need changing if cl.el is ever
;; changed.)
;;
;; Thu Nov 23 17:57:55 2000
;; It doesn't seem to be true that copy-foo returns a structure eq to
;; the original.  I don't know how I decided this...think about
;; changing this function
;;
;; Sat Dec 2 12:19:07 2000 Changed this in the light of above, but
;; left comments in case something goes wrong.  If this works, we
;; don't really need to make it a separate function.

(defun qda-make-fresh-copy (node &optional parent-address number new-name)
  ;;  (let ((new-node (vconcat node)))
  (let ((new-node (copy-qda-node node)))
    (setf (qda-node-children new-node) '())
    (when parent-address
      (setf (qda-node-parent new-node) parent-address))
    (when number
      (setf (qda-node-number new-node) number))
    (when new-name
      (setf (qda-node-name new-node) new-name))
  new-node))

;; Schlep through the node and its babies, making fresh copies,
;; renumbering/naming as we go.

(defun qda-recursive-copy-node (node &optional parent-address number new-name)
  (let* ((new-node
	  (qda-make-fresh-copy node parent-address number new-name))
	 (p (qda-node-parent new-node)))
    (dolist (this-kid (qda-node-children node) new-node)
      (setf (qda-node-children new-node)
	    (nconc (qda-node-children new-node)
		   (list (qda-recursive-copy-node
			  this-kid
			  (append (if (equal p '(0)) nil ; miss out root number
				    (qda-node-parent new-node))
				  (list (qda-node-number new-node))))))))))

(qda-defun*-tree-mod qda-copy-node (node new-parent &key new-name)
  "Copy NODE to NEW-PARENT.
&KEY argument NEW-NAME changes the new node's name.
Signals an error if there is a name clash."
  (interactive
   (list
    (qda-completing-read-node "Node to copy: ")
    (qda-completing-read-node "Copy to: ")
    :new-name nil))
  (unless new-parent
    (setq new-parent '(0)))
  (let ((this-node (qda-find-node node))
	(this-node-new-parent (qda-find-node new-parent)))
    (unless this-node-new-parent
      (qda-error "Can't find new parent node: %s" new-parent))
    (unless this-node
      (qda-error "Can't find node to copy: %s" node))
    (when (interactive-p)
      (setq
       new-name
       (progn (while
		  (member
		   (setq new-name
			 (qda-read-string "New name: "
					  (qda-node-name this-node)))
		   (qda-kid-names this-node-new-parent))
		(message "The name %s is already in use" new-name)
		(sit-for 1))
	      (if (string= new-name (qda-node-name this-node)) nil new-name))))
    (let* ((t-n-n-p-last-kid (qda-last-kid this-node-new-parent))
	   (new-num (if t-n-n-p-last-kid
			(1+ (qda-node-number t-n-n-p-last-kid))
		      1))
	   (new-node (qda-recursive-copy-node
		      this-node new-parent new-num new-name)))
      (setf (qda-node-children this-node-new-parent)
	    (nconc (qda-node-children this-node-new-parent)
		   (list new-node)))
      (qda-message "Node %s copied to %s as %d%s."
		   (qda-node-numerical-address this-node)
		   (if (equal (qda-node-parent new-node) '(0))
		       "()"
		     (qda-node-parent new-node))
		   (qda-node-number new-node)
		   (if new-name (format " and renamed %s" new-name) "")))))

(qda-defun*-tree-mod qda-move-node (node new-parent &key new-name)
  "Move NODE to NEW-PARENT.
&KEY argument NEW-NAME changes the new node's name.
Signals an error if there is a name clash."
  (interactive
   (list
    (qda-completing-read-node
     "Node to move: ")
    (qda-completing-read-node "Move to: ")
    :new-name nil))
  (unless new-parent
    (setq new-parent '(0)))
  (let ((this-node (qda-find-node node))
	(this-node-new-parent (qda-find-node new-parent)))
    (unless this-node-new-parent
      (qda-error "Can't find new parent node: %s" new-parent))
    (unless this-node
      (qda-error "Can't find node to move: %s" node))
    (when (interactive-p)
      (setq new-name
	    (progn
	      (while
		  (member
		   (setq new-name
			 (qda-read-string
			  "New name: "
			  (qda-node-name this-node)))
		   (qda-kid-names this-node-new-parent))
		(message "The name %s is already in use" new-name)
		(sit-for 1))
	      (if (string= new-name (qda-node-name this-node)) nil new-name))))
    (let* ((this-node-parent (qda-find-node (qda-node-parent this-node)))
	   (t-n-n-p-last-kid (qda-last-kid this-node-new-parent))
	   (new-num (if t-n-n-p-last-kid
			(1+ (qda-node-number t-n-n-p-last-kid)) 1))
	   (this-new-node (qda-recursive-copy-node
			   this-node new-parent new-num new-name)))
      (setf (qda-node-children this-node-new-parent)
	    (nconc (qda-node-children this-node-new-parent)
		   (list this-new-node)))
      (setf (qda-node-children this-node-parent)
	    (delete this-node (qda-node-children this-node-parent)))
      (qda-message "Node %s moved to %s as %d%s."
		   (qda-node-numerical-address this-node)
		   (if (equal (qda-node-parent this-new-node) '(0)) "()"
		     (qda-node-parent this-new-node))
		   (qda-node-number this-new-node)
		   (if new-name (format " and renamed %s" new-name) "")))))

(qda-defun-tree-mod qda-rename-node (node new-name)
  "Rename NODE to NEW-NAME.
Signals an error if there is a name clash."
  (interactive
   (list
    (qda-completing-read-node
     "Node to rename: " )
    (qda-read-string "New name: ")))
  (when (or (null new-name)
	    (string= new-name ""))
    (qda-error "You can't call %s nothing" node))
  (let ((this-node (qda-find-node node)))
    (cond ((null this-node) (qda-error "Can't find node %s to rename" node))
	  ((member new-name (qda-kid-names
			     (qda-find-node (qda-node-parent this-node))))
	   (qda-error "The name %s is already in use" new-name))
	  (t (setf (qda-node-name this-node) new-name)
	     (qda-message "Node %s renamed to %s"
			  (qda-node-numerical-address this-node)
			  (qda-node-name this-node))))))

(defun qda-renum-babies (node)
  "This just goes through all of the offspring of NODE making all of
the parent addresses consistent."
  (dolist (this-kid (qda-node-children node))
   (setf (qda-node-parent this-kid)
	  (qda-node-numerical-address node))
   (qda-renum-babies this-kid)))

(qda-defun-tree-mod qda-renumber-node (node new-number)
  "Renumber NODE to NEW-NUMBER.
Sorts the node after changing so that the new number is in the
right sequence.  Signals an error if there is a number clash."
  (interactive
   (list
    (qda-completing-read-node "Node to renumber: ")
    (qda-read-number "New number: ")))
  (let* ((this-node (or (qda-find-node node)
			(qda-error "Can't find node %s" node)))
	 (this-node-parent (qda-find-node (qda-node-parent this-node)))
	 (old-num (qda-node-numerical-address this-node)))
    (cond ((null this-node) nil)
	  ((member new-number (qda-kid-numbers this-node-parent))
	   (qda-error "The number %d is already in use" new-number))
	  (t (setf (qda-node-number this-node) new-number)
	     (setf (qda-node-children this-node-parent)
		   (sort (qda-node-children this-node-parent)
			 'qda-sort-node-numbers))
	     (qda-renum-babies this-node)
	     (qda-message "Node %s renumbered to %d."
			  old-num (qda-node-number this-node))))))

(qda-defun-tree-mod qda-redescribe-node (node description)
  "Change description of NODE to DESCRIPTION.
If DESCRIPTION is nil it is set to nil."
  (interactive
   (list
    (qda-completing-read-node "Node to redescribe: ")
    (qda-read-string "New description: " nil t)))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (setf (qda-node-description node) description)
  (qda-message "Node %s description changed to: %s."
	       (qda-node-numerical-address node)
	       (or description "")))


;;; indexing

(defun qda-node-add-indexing (name node text-units)
  "Add TEXT-UNITS in doc called NAME to NODE."
  (interactive
   (list
    (qda-completing-read-doc "Document: ")
    (qda-completing-read-node "Index to node: ")
    (qda-read-number-or-range-list "Text units: ")))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (setf (qda-node-indexing node)
	(qda-add-indexing name (qda-node-indexing node) text-units))
  (qda-message "Text %s %s in %s added to %s"
	       (sing-or-plural-noun "unit" "units" (length text-units))
	       text-units
	       name
	       (qda-node-numerical-address node)))

;; (defun qda-node-add-all-text-units (name node text-units)
(defun qda-node-add-all-text-units (name node)
  "Add all text-units in doc called NAME to NODE."
  (interactive
   (list
    (qda-completing-read-doc "Document: ")
    (qda-completing-read-node "Index to node: ")))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (setf (qda-node-indexing node)
	(qda-add-indexing name (qda-node-indexing node) 'all))
  (qda-message "All text units in %s added to %s"
	       name
	       (qda-node-numerical-address node)))

(defun qda-node-add-indexing-maybe-marked ()
  (interactive)
  (if (and (boundp 'marked-indexing)
	   marked-indexing
	   (y-or-n-p "Add marked indexing to a node? "))
      (progn
	(let ((node (qda-find-node
		     (qda-completing-read-node "Index to node: "))))
	  (setf (qda-node-indexing node)
		(qda-indexing-union (qda-node-indexing node) marked-indexing))
	  ;; not a very informative message !?
	  (qda-message "Marked indexing added to %s."
		       (qda-node-numerical-address node))))
    (call-interactively 'qda-node-add-indexing)))



(defun* qda-node-delete-indexing (name node &key text-units)
  "Delete TEXT-UNITS in doc called NAME from NODE."
  (interactive
   (list
    (qda-completing-read-doc "Document: ")
    (qda-completing-read-node "Delete from node: ")
    :text-units (qda-read-number-or-range-list "Text units: ")))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (setf (qda-node-indexing node)
	(qda-delete-indexing name (qda-node-indexing node) text-units))
  (qda-message "Text %s %s in %s deleted from %s."
	       (sing-or-plural-noun "unit" "units" (length text-units))
	       text-units
	       name
	       (qda-node-numerical-address node)))

(defun qda-node-delete-all-indexing (node &optional no-confirm)
  (interactive
   (list (qda-completing-read-node  "Delete all indexing from: ")))
  (let ((n (or (qda-find-node node) (qda-error "Can't find node %s" node))))
    (when (or no-confirm
	      (y-or-n-p (format "Really delete all indexing from %s? " node)))
      (setf (qda-node-indexing n) '())
      (qda-message "All indexing deleted from %s." node))))

(defun qda-node-delete-children (node &optional no-confirm)
  (interactive
   (list (qda-completing-read-node  "Delete children of node: ")))
  (let ((n (or (qda-find-node node) (qda-error "Can't find node %s" node))))
    (when (or no-confirm
	      (y-or-n-p (format "Really delete all children %s? " node)))
      (setf (qda-node-children n) '())
      (qda-message "All children of %s deleted." node))))

(defun qda-node-delete-indexing-maybe-marked ()
  (interactive)
  (if (and (boundp 'marked-indexing)
	   marked-indexing
	   (y-or-n-p "Delete marked indexing from a node? "))
      (progn
	(let ((node (qda-find-node
		     (qda-completing-read-node
		      "Delete indexing from node: "))))
	  (setf (qda-node-indexing node)
		(qda-indexing-set-difference
		 (qda-node-indexing node) marked-indexing))
	  ;; not a very informative message !?
	  (qda-message "Marked indexing deleted from %s."
		       (qda-node-numerical-address node))))
    (call-interactively 'qda-node-delete-indexing)))

;; I can't imagine the next two will be used much

(defun qda-node-add-this-indexing (node)
  "Add text unit at point to NODE."
  (interactive
   (list
    (qda-completing-read-node "Node for this indexing: ")))
  (let ((this-node (qda-find-node node))
	n-and-d)
    (unless this-node
      (qda-error "The node %s doesn't exist" node))
    (setf (qda-node-indexing this-node)
	  (qda-add-this-indexing (qda-node-indexing this-node)))
    (setq n-and-d (qda-get-this-text-unit-number-and-doc))
    (qda-message "Text unit %d from %s added to %s."
		 (first n-and-d)
		 (second n-and-d)
		 (qda-node-numerical-address this-node))))

(defun qda-node-delete-this-indexing (node)
  "Delete text unit at point from NODE."
  (interactive
   (list
    ;; todo: make this cleverer: offer only nodes that
    ;; actually have this indexed
    (qda-completing-read-node "Node to delete this indexing from: ")))
  (let ((this-node (qda-find-node node))
	n-and-d)
    (unless this-node
      (qda-error "The node %s doesn't exist" node))
    (setf (qda-node-indexing this-node)
	  (qda-delete-this-indexing (qda-node-indexing this-node)))
    (setq n-and-d  (qda-get-this-text-unit-number-and-doc))
    (qda-message "Text unit %d from %s deleted from %s."
		(first n-and-d)
		(second n-and-d)
		(qda-node-numerical-address this-node))))

(defun qda-node-delete-this-indexing-this-node ()
  "Delete text unit at point from the node associated with the current buffer."
  (interactive)
    (unless qda-view-indexing-node
      (qda-error "I see no node indexing here %s" node))
    (let ((this-node (qda-find-node qda-view-indexing-node))
	  n-and-d)
      (setf (qda-node-indexing this-node)
	    (qda-delete-this-indexing (qda-node-indexing this-node)))
      (setq n-and-d  (qda-get-this-text-unit-number-and-doc))
      (qda-message "Text unit %d from %s deleted from %s."
		   (first n-and-d)
		   (second n-and-d)
 		   (qda-node-numerical-address this-node))))

(defun qda-node-spread-indexing (doc node text-units spread)
  (interactive
   (list
    (qda-completing-read-doc "Document: ")
    (qda-completing-read-node "Node: ")
    (qda-read-number-or-range-list "Text units: ")
    (qda-read-number "Spread: ")))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (setf (qda-node-indexing node)
	(qda-spread-indexing doc (qda-node-indexing node) text-units spread))
  (qda-message "Text %s %s in %s indexed at %s spread by %d."
	       (sing-or-plural-noun "unit" "units" (length text-units))
	       text-units
	       doc
	       (qda-node-numerical-address node)
	       spread))

(defun qda-node-spread-indexing-maybe-marked ()
  (interactive)
  (if (and (boundp 'marked-indexing)
 	   marked-indexing
 	   (y-or-n-p "Spread marked indexing? "))
      (progn
 	(let ((node (qda-find-node
 		     (qda-completing-read-node "Node: ")))
	      (spread (qda-read-number "Spread: ")))
	  (dolist (indexing marked-indexing)
	    (setf (qda-node-indexing node)
		  (qda-spread-indexing (qda-indexing-name indexing)
				       (qda-node-indexing node)
				       (qda-indexing-text-units indexing)
				       spread))
 	  ;; not a very informative message !?
 	  (qda-message "Marked indexing spread by %d." spread))))
     (call-interactively 'qda-node-spread-indexing)))

(defun qda-node-copy-indexing (n1 n2)
  (interactive
   (list
    (qda-completing-read-node "Node to copy indexing from: ")
    (qda-completing-read-node "Node to copy indexing to: ")))
  (setq n1 (or (qda-find-node n1)
	       (qda-error "Can't find node %s" n1)))
  (setq n2 (or (qda-find-node n2)
	       (qda-error "Can't find node %s" n2)))
  (setf (qda-node-indexing n2)
	(qda-indexing-union (qda-node-indexing n1)
			    (qda-node-indexing n2)))
  (qda-message "Indexing from %s copied to %s."
	       (qda-node-numerical-address n1)
	       (qda-node-numerical-address n2)))


;;; search functions

(defun qda-node-make-unique-kid-name (node base)
  "Make a name that is not used by one of NODE's kids based on BASE."
  (let ((kid-names (qda-kid-names node)) (n 1) name)
    (if  (not (member base kid-names))
	base
      (while (member (setq name (format "%s<%d>" base n)) kid-names)
	(incf n))
      name)))

;; perhaps this should be an option to qda-search

(defun qda-search-index-to-nodes (search-terms node)
  "Search for SEARCH-TERMS and index them under NODE.

See `qda-search' for information about search terms.  Each search term
has a new node added under NODE, with a unique name and the search
term in its description."
  (interactive
   (list (qda-search-terms-minibuffer)
	 (qda-completing-read-node "Index finds under node: ")))
  (when search-terms
    (let ((new-node nil)
	  (finds '()))
      (unless (or (not node)		;i.e. root
		  (qda-find-node node))
	(qda-error "Can't find node %s " node))
      (setq finds (qda-search search-terms))
      (dolist (find finds)
	(setq new-node
	      (qda-add-node
	       (qda-node-make-unique-kid-name
		(qda-find-node node)
		"Search Finds")
	       node
	       :description (format "Search for %s %s"
				    (caar find)
				    (or (cdar find) ""))))
	(setf (qda-node-indexing new-node) (cdr find))))
  (qda-message "Searches indexed under %s" (or node "()"))))



;;; node set functions
;; todo: perhaps finds could optionally replace the former indexing.

(defun qda-node-set-comparison (n1 n2 n3)
  "General set comparison function for nodes.
Compare nodes N1 and N2 and save result in N3.

Relies on qda-set-func and qda-set-func-string being defined in the
calling function.  This command is not intended to be used directly."
  (interactive
   (list
    (qda-completing-read-node "Node 1: ")
    (qda-completing-read-node "Node 2: ")
    (qda-completing-read-node "Add results to: ")))
  (setq n1 (or (qda-find-node n1)
	       (qda-error "Can't find node %s" n1)))
  (setq n2 (or (qda-find-node n2)
	       (qda-error "Can't find node %s" n2)))
  (setq n3 (or (qda-find-node n3)
	       (qda-error "Can't find node %s" n3)))
  (let ((finds (funcall qda-set-func ; qda-set-func defined in calling function
			(qda-node-indexing n1) (qda-node-indexing n2)))
	stats)
    (if finds
	(progn
	  (setf (qda-node-indexing n3)
		(qda-indexing-union (qda-node-indexing n3) finds))
	  (setq stats (qda-indexing-stats finds))
	  (qda-message "%s of %s and %s: %d finds in %s %s added to %s."
		       qda-set-func-string	;in calling function
		       (qda-node-numerical-address n1)
		       (qda-node-numerical-address n2)
		       (qda-indexing-num-text-units stats)
		       (qda-indexing-num-docs stats)
		       (sing-or-plural-noun
			"doc" "docs" (qda-indexing-num-docs stats))
		       (qda-node-numerical-address n3)))
      (qda-message "%s of %s and %s: no finds."
		   qda-set-func-string
		   (qda-node-numerical-address n1)
		   (qda-node-numerical-address n2)))))

(defun qda-node-union (n1 n2 n3)
  "Store set union of indexing in N1 and N2 in N3.
The resulting indexing is merged with any that may already be in N3."
  (interactive (list nil nil nil))
  (let ((qda-set-func 'qda-indexing-union)
	(qda-set-func-string "Union"))
    (if (interactive-p)
	(call-interactively 'qda-node-set-comparison)
      (qda-node-set-comparison n1 n2 n3))))

(defun qda-node-set-difference (n1 n2 n3)
    "Store set difference of indexing in N1 and N2 in N3.
The resulting indexing is merged with any that may already be in N3."
    (interactive (list nil nil nil))
  (let ((qda-set-func 'qda-indexing-set-difference)
	(qda-set-func-string "Set difference"))
    (if (interactive-p)
	(call-interactively 'qda-node-set-comparison)
      (qda-node-set-comparison n1 n2 n3))))

(defun qda-node-intersection (n1 n2 n3)
    "Store set intersection  of indexing in N1 and N2 in N3.
The resulting indexing is merged with any that may already be in N3."
    (interactive (list nil nil nil))
  (let ((qda-set-func 'qda-indexing-intersection)
	(qda-set-func-string "Intersection"))
    (if (interactive-p)
	(call-interactively 'qda-node-set-comparison)
      (qda-node-set-comparison n1 n2 n3))))

(defun qda-node-exclusive-or (n1 n2 n3)
    "Store set exclusive-or of indexing in N1 and N2 in N3.
The resulting indexing is merged with any that may already be in N3."
    (interactive (list nil nil nil))
  (let ((qda-set-func 'qda-indexing-exclusive-or)
	(qda-set-func-string "Exclusive or"))
    (if (interactive-p)
	(call-interactively 'qda-node-set-comparison)
      (qda-node-set-comparison n1 n2 n3))))

(defun qda-node-collect (node)
  "Collect all indexing from the subtree of NODE into NODE.
Result of collect is merged with indexing at NODE."
  (interactive
   (list
    (qda-completing-read-node "Node to collect: ")))
  (let ((n (or (qda-find-node node) (qda-error "Can't find node %s" node))))
     (loop
      for indexing in (cdr (qda-map-nodes
			     #'(lambda (nd)
			       (qda-node-indexing nd))
			    n))
      do (when indexing
	   (setf (qda-node-indexing n) (qda-indexing-union
					(qda-node-indexing n) indexing))))))

;; this should be made more general in qda-indexing

(qda-defun-tree-mod qda-node-vector (n1 n2)
  "Cross tabulate N1 by the children of N2.
Results are added as a new subtree under N1."
  (interactive
   (list (qda-completing-read-node "Base node: ")
	 (qda-completing-read-node "Vector node: ")))
  (setq n1 (or (qda-find-node n1)
	       (error "Can't find node %s" n1)))
  (setq n2 (or (qda-find-node n2)
	       (error "Can't find node %s" n2)))
  (let* ((vector-parent
	 (qda-add-node
	  (qda-node-make-unique-kid-name
	   n1
	   (format "by %s" (qda-node-name n2)))
	  (qda-node-numerical-address n1)
	  :description (format "Vector %s by %s"
			       (qda-node-numerical-address n1)
			       (qda-node-numerical-address n2))))
	 (v-p-num-addr (qda-node-numerical-address vector-parent)))
    (loop
     for n in (qda-node-children n2)
     do (qda-node-intersection n1 n
			       (qda-add-node
				(format "%s:%s"
					(qda-node-name n1)
					(qda-node-name n))
				v-p-num-addr
				:description
				(format "Intersection of %s and %s"
					(qda-node-numerical-address n1)
					(qda-node-numerical-address n)))))
    (qda-message "Vector %s by %s added as %s"
		 (qda-node-numerical-address n1)
		 (qda-node-numerical-address n2)
		 (qda-node-name vector-parent))
    vector-parent))

;; should be made more general also
;; should we delete the indexing at the first level?

(qda-defun-tree-mod qda-node-matrix (n1 n2 n3)
  (interactive
   (list (qda-completing-read-node "Base node: ")
	 (qda-completing-read-node "First matrix node: ")
	 (qda-completing-read-node "Second matrix node: ")))
  (setq n1 (or (qda-find-node n1)
	       (error "Can't find node %s" n1)))
  (setq n2 (or (qda-find-node n2)
	       (error "Can't find node %s" n2)))
  (setq n3 (or (qda-find-node n3)
	       (error "Can't find node %s" n3)))
  (let ((first-vec (qda-node-vector n1 n2)))
    (mapc #'(lambda (vn)
	     (qda-node-vector vn n3))
	  (qda-node-children first-vec))
    (qda-message "Matrix %s by %s by %s indexed as %s"
		 (qda-node-numerical-address n1)
		 (qda-node-numerical-address n2)
		 (qda-node-numerical-address n3)
		 (qda-node-name first-vec))
    first-vec))


;;; saving and loading

(defun qda-save-tree (file)
  (qda-save-object (list qda-node-counter qda-root) file))

(defun qda-load-tree (file)
  (let ((tree-stuff (qda-get-object file))) 
    (setq qda-root (cadr tree-stuff))
    (setq qda-node-counter (car tree-stuff))
    (qda-verify-tree)))


;;; slots

;;  This is just a hook onto which to hang other structures.

(defstruct (qda-node-slot (:type list))
  type)

(defun qda-find-node-slot-type (type node)
  (let ((slots (qda-node-slots node)))
    (loop
     for slot in slots
     thereis (if (eq (qda-node-slot-type slot) type)
		 slot
	       nil))))

;; a means of showing slot attributes of a node
;; todo: do something better with this; also, it really should be
;; integrated with `qda-view-tree-memo-mark' et al.

(defvar qda-node-slot-display-strings-alist
  '((memo . "Memo")
    (update . "Update")))

(defun qda-node-slot-display-strings (node)
  (let (d)
  (mapconcat #'(lambda (s)
		(if (setq d (assoc (qda-node-slot-type s)
				   qda-node-slot-display-strings-alist))
		    (cdr d)
		  ""))
	     (qda-node-slots node) " ")))

;;; updatable node

;;  The idea is that you can specify some function to update a node on
;;  demand.  Mostly this will be useful with search-functions (for base data
;;  for example).
;;

(defstruct (qda-node-slot-updatable
	    (:include qda-node-slot
		      (type 'update))
	    (:type list))
  update-func)

(defun qda-node-update (node)
  (let ((upd-slot (qda-find-node-slot-type 'update node)))
    (if upd-slot
	(qda-node-slot-updatable-update-func upd-slot)
;;;	(cadr (qda-find-node-slot-type 'update node))
      nil)))

(defun qda-node-add-updatable-slot (node func)
  (let ((upd-sl (qda-find-node-slot-type 'update node)))
    (if upd-sl
	(setf (qda-node-slot-updatable-update-func upd-sl) func)
      (push
       (make-qda-node-slot-updatable
	:update-func func)
       (qda-node-slots node)))
    node))

(defun qda-node-remove-updatable-slot (node)
  (interactive
   (list
    (qda-completing-read-node "Node to remove updating from: ")))
   (let ((n (qda-find-node node))
	upd-sl)
    (unless n (qda-error "Can't find node %s" node))
    (setq upd-sl (qda-find-node-slot-type 'update n))
    (when upd-sl
      (setf (qda-node-slots n)
	    (delete* upd-sl (qda-node-slots n) :test 'equal)))
    (qda-message "Updating removed from %s." (qda-node-numerical-address n))))

(defun qda-node-do-update (node)
  (interactive
   (list
    (qda-completing-read-node "Node to update: ")))
  (let ((n (qda-find-node node))
	upd-sl)
    (unless n (qda-error "Can't find node %s" node))
    (setq upd-sl (qda-find-node-slot-type 'update n))
    (and
     upd-sl
     (qda-node-slot-updatable-update-func upd-sl)
     (condition-case err
	 (funcall (qda-node-slot-updatable-update-func upd-sl) n)
       (error (qda-error "Update error in %s: %s"
			 (qda-node-numerical-address n)
			 (error-message-string err))))
;;;      (funcall (qda-node-slot-updatable-update-func upd-sl) n)
     (qda-message "Node %s updated" (qda-node-numerical-address n)))))

;; todo: some indication of progress (this can take a while to finish,
;; and the user may want reassuring that it hasn't crashed).
;; Sat Nov 25 18:04:38 2000
;; What happens if you add an updatable slot that refers to a node
;; that is itself updatable?  If the first node is earlier in the
;; tree, it won't be in sync with the second node.  In some cases,
;; they will never be in sync...(Oh dear).

(defun qda-node-do-all-updates (&optional node)
  (interactive)
  (qda-message "Updating all nodes...")
  (qda-map-nodes #'(lambda (n)
		     (qda-message "Updating all nodes %s"
				  (qda-node-numerical-address node))
		     (qda-node-do-update n))
		 (or node qda-root))
  (qda-message "All nodes updated."))

;; updatable search
;;  todo: it would save space if there were a function whose symbol
;;  replaced the lambda expression in the update slot.

(defun qda-search-finds (search-results)
  "Return indexing from SEARCH-RESULTS.
SEARCH-RESULTS is the indexing resulting from a single search."
  (cdar search-results))

(defun qda-node-add-search-update (node search-term)
    (interactive
     (list
      (qda-completing-read-node "Node to add updatable search to: ")
      (qda-search-term-minibuffer)))
    (let ((n (qda-find-node node)))
      (unless n (qda-error "Can't find node %s" node))
      ;; Fri Nov 24 20:12:13 2000 now we clobber any old indexing.
      ;; This may be undesirable in most circumstances, but is usually
      ;; what we want.
      (qda-node-add-updatable-slot n
       (list 'lambda '(n)
;; 		   (list 'qda-indexing-union
;; 			 '(qda-node-indexing n)
;; 			 (list 'qda-search-finds
;; 			       (list 'qda-search  (list 'quote search-term)))))))))
	     (list 'setf '(qda-node-indexing n)
		   (list 'qda-search-finds
			 (list 'qda-search  (list 'quote search-term))))))))

;;; editing updatable searches

(require 'pp)

(define-derived-mode qda-update-mode emacs-lisp-mode "*QDA-update*"
  "Major mode for editing update slots.
When you're finished with the editing, \\[qda-updatable-end-edit]
saves the changes.

Special commands:
\\{qda-update-mode-map}"
  (make-local-variable 'qda-node))

(define-key qda-update-mode-map "\C-c\C-c" 'qda-updatable-end-edit)

(defvar qda-updatable-pending-save '())

(defun qda-updatable-buffer-name (n)
  (format "Update for node: %s" n))

(defun qda-updatable-slot-edit (node)
  (interactive
   (list
    (qda-completing-read-node "Node: ")))
  (setq node (or (qda-find-node node)
		 (qda-error "Can't find node %s" node)))
  (let ((update-slot (cadr (qda-find-node-slot-type 'update node)))
	(buf (qda-updatable-buffer-name (qda-node-numerical-address node))))
;;    (if (not update-slot)
;;	(qda-error "No update slot for node %s" node)
      (unless (get-buffer buf)
	(setq buf (set-buffer
		   (get-buffer-create buf)))
	(qda-update-mode)
	(setq qda-node (qda-node-numerical-address node))
	(erase-buffer)
	(if update-slot
	    (insert (pp-to-string update-slot)))
	(set-buffer-modified-p nil)
	(pushnew qda-node qda-updatable-pending-save))
    (pop-to-buffer buf)))

(defun qda-updatable-end-edit ()
  (interactive)
  (when (eq major-mode 'qda-update-mode)
    (let ((form (buffer-substring-no-properties (point-min) (point-max)))
	  (node (qda-find-node qda-node))
	  win)
      (unless node
	(qda-error "Node %s no longer exists" qda-node))
      (if (string-match form "[ \t\n]+")
	  ;; nothing left?  remove the slot
	  (qda-node-remove-updatable-slot node)
	;; just reading the buffer is dangerous, but I can't think of
	;; anything better at the moment.
	(qda-node-add-updatable-slot node (read form))
	;; reset indexing for the node (I think it's ok to do this, if
	;; not, then the whole thing becomes a bit pointless).
	(setf (qda-node-indexing node) '())
	(setq qda-updatable-pending-save
		(delete* qda-node
			 qda-updatable-pending-save :test 'equal)))
      (set-buffer-modified-p nil)
      (kill-buffer nil)
      (when (and qda-view-tree-centric-p
		 (get-buffer-window qda-view-tree-buffer)
		 (setq win (get-buffer-window (current-buffer)))
		 (not (eq win (get-buffer-window qda-view-tree-buffer))))
	(delete-window)))))

(defun qda-update-edit-offer-save ()
  (let (buf)
    (dolist (update qda-updatable-pending-save t)
      (when (and (setq buf (get-buffer
			    (qda-updatable-buffer-name update)))
		 (y-or-n-p (format "Save updatable slot for %s? " update)))
	(set-buffer buf)
	(qda-updatable-end-edit)))))

(or (memq 'qda-update-edit-offer-save kill-emacs-query-functions)
    (add-hook 'kill-emacs-query-functions 'qda-update-edit-offer-save))

;; A silly macro for putting comments into updatable slots.

(defmacro qda-comment (&rest args)
  "Return nil, without evaluating ARGS.
Useful for putting comments into updatable slots."
  nil)


;;; memos

;; I have included a facility for adding memos to text units, but this
;; is fiddly & I don't think it is going to be very useful, so it
;; isn't implemented at user level e.g., text unit memos aren't
;; displayed when viewing indexing

;; todo: text unit memos might be useful for the doc structure
;; though...but I can't get excited about implementing them right now.

(defstruct (qda-node-slot-memo
	    (:include qda-node-slot
		      (type 'memo))
	    (:type list))
  (memo-list '()))

(defun qda-node-memo (node)
  (let ((memos (qda-node-slot-memo-memo-list
		(qda-find-node-slot-type 'memo node))))
    (if memos
	(loop
	 for memo in memos
	     ;; the node memo is an atom, text-unit memos are lists
	     thereis (if (atom memo)
			 memo
		       nil))
      nil)))

(defun make-qda-node-memo (node text)
  (let ((memo-slot (qda-find-node-slot-type 'memo node)))
    (when text
      (if memo-slot
	  (push text (qda-node-slot-memo-memo-list memo-slot))
	(qda-node-slot-memo-memo-list
	 (car (push (make-qda-node-slot-memo :memo-list (list text))
	       (qda-node-slots node))))))))

(defun delete-qda-node-memo (node)
  (let ((memo-slot (qda-find-node-slot-type 'memo node))
	 (memo (qda-node-memo node))
	 memos new-memos)
     (when (and
	    memo-slot
	    memo
	    (setq memos (qda-node-slot-memo-memo-list memo-slot)))
       (setq new-memos (delete* memo memos :test 'equal))
       (setf
	(qda-node-slots node)
	(if new-memos
	    (nsubstitute
	     (make-qda-node-slot-memo :memo-list new-memos)
	     memo-slot
	     (qda-node-slots node))
	  (delete* memo-slot (qda-node-slots node) :test 'equal))))))

(defun qda-node-make-memo-identifier (node)
  "Return memo identifier for NODE."
  (when node
    (qda-node-numerical-address node)))

(defun qda-node-memo-header-string (node)
  (format "%s\n%s\n\n"
	  (qda-node-info-string node)
	  qda-line-string))

(defun qda-insert-node-memo-header (node)
  (insert (qda-node-memo-header-string node)))

;;; node-text-unit memos

(defun qda-node-text-unit-p (node-text-unit)
  (and (listp node-text-unit)
       (= 3 (length node-text-unit))
       (qda-node-p (first node-text-unit))
       (qda-doc-p (second node-text-unit))
       (qda-text-unit-p (third node-text-unit))))

(defun qda-node-text-unit-node (node-text-unit)
  (first node-text-unit))

(defun qda-node-text-unit-text-unit (node-text-unit)
  (cons (qda-doc-name (second node-text-unit))
	(qda-text-unit-number (third node-text-unit))))

(defun qda-find-node-text-unit (node-text-unit-identifier)
  (when (listp node-text-unit-identifier)
    (let ((node (qda-find-node-strict (first node-text-unit-identifier)))
	  (doc (qda-find-doc (second node-text-unit-identifier)))
	  (text-unit (third node-text-unit-identifier)))
      (when (and node
		 doc
		 (member* (qda-text-unit-number text-unit)
			  (qda-indexing-text-units
			   (qda-find-indexing doc (qda-node-indexing node)))
			  :test 'qda-text-unit=))
	(list node doc text-unit)))))

(defun qda-node-text-unit-memo (node-text-unit)
  (let* ((node (qda-node-text-unit-node node-text-unit))
	 (memos (cadr (qda-find-node-slot-type 'memo node)))
	 (text-unit (qda-node-text-unit-text-unit node-text-unit)))
    (if memos
	(loop
	 for memo in memos
	     thereis (if (and (listp memo) (equal text-unit (car memo)))
			 (cdr memo)
		       nil))
      nil)))

(defun make-qda-node-text-unit-memo (node-text-unit text)
  (let ((node (qda-node-text-unit-node node-text-unit))
	(text-unit (qda-node-text-unit-text-unit node-text-unit))
	memo-list)
    (when (and node text)
      (if (setq memo-slot (qda-find-node-slot-type 'memo node))
	  (push (cons text-unit text)
		(qda-node-slot-memo-memo-list memo-slot))
	(qda-node-slot-memo-memo-list
	 (car (push (make-qda-node-slot-memo :memo-list
					     (list (cons text-unit text)))
		    (qda-node-slots node))))))))

(defun delete-qda-node-text-unit-memo (node-text-unit)
  (let* ((node (qda-node-text-unit-node node-text-unit))
	 (memo-slot (qda-find-node-slot-type 'memo node))
	 (memo (qda-node-text-unit-memo node-text-unit))
	 memos new-memos)
     (when (and
	    memo-slot
	    memo
	    (setq memos (qda-node-slot-memo-memo-list memo-slot)))
       (setq new-memos
	     (delete* (cons (qda-node-text-unit-text-unit node-text-unit) memo)
		      memos :test 'equal))
       (setf (qda-node-slots node)
	     (if new-memos
		 (nsubstitute
		  (make-qda-node-slot-memo :memo-list new-memos)
		  memo-slot
		  (qda-node-slots node)
		  :test 'equal)
	       (delete* memo-slot (qda-node-slots node) :test 'equal))))))

(defun qda-node-text-unit-make-memo-identifier (node-text-unit)
  (when node-text-unit
    (list
     (qda-node-numerical-address (first node-text-unit))
     (qda-doc-name (second node-text-unit))
     (qda-text-unit-number (third node-text-unit)))))

(defun qda-node-text-unit-memo-header-string (node-text-unit)
    (concat (qda-node-info-string (qda-node-text-unit-node node-text-unit))
	    (format "\ntext unit: %s\n%s\n\n"
		    (qda-node-text-unit-text-unit node-text-unit)
		    qda-line-string)))

(defun qda-insert-node-text-unit-memo-header (node-text-unit)
  (insert (qda-node-text-unit-memo-header-string node-text-unit)))

(defun qda-node-edit-memo (node)
  (interactive
   (list (qda-completing-read-node "Edit memo for node: ")))
  (unless (setq node (qda-find-node node))
    (qda-error "Can't find node %s" node))
  (qda-memo-edit node))

(defun qda-node-text-unit-edit-memo (node-text-unit)
  (interactive
   (list (list (qda-completing-read-node "Node: ")
	       (qda-completing-read-doc "Document: ")
	       (qda-read-number
		"Text unit: "
		(first (qda-get-this-text-unit-number-and-doc))))))
  (let ((ntu (or (qda-find-node-text-unit node-text-unit)
		 (qda-error "Can't find node text unit %s" node-text-unit))))
    (qda-memo-edit ntu)))

;; todo: add node-text-unit memos in here somewhere

(defun qda-node-memo-list-all ()
  (let (memos)
    (qda-map-nodes #'(lambda (n)
		      (when (qda-node-memo n)
			(setq memos
			      (concat
			       memos
			       (format "\n%s%s\n"
				       (qda-node-memo-header-string n)
				       (qda-node-memo n))))))
		   qda-root)
    (when memos
      (insert (format "\nNode memos\n%s\n%s\n\n"
		      qda-line-string
		      memos)))))



;;; node listing

;; todo: add choosing start node to commands

(defvar qda-node-list-buffer "*QDA-node-list*")

;; todo: move this somewhere more appropriate also, should take into
;; account the (unlikely) case that the node name is longer than the
;; length specified

(defun qda-node-abbreviated-path-name (node length)
  "Return the path-name to NODE abbreviated to LENGTH."
  (let ((path (qda-node-path-name node))
	sublen)
    (if (<= (length path) length)
	path
      (setq sublen (- length
		      (+ 3 (length (qda-node-name node)))))
      (concat (substring path 0 sublen)
			 ".../" (qda-node-name node)))))

(defun qda-node-number-with-padding-format-string (node extra-space)
  "Returns a formatting string padded to allow for the deepest node under NODE.
EXTRA-SPACE is the number of spaces to add after the node."
  (format "%%-%ds" (+ extra-space (* 2 (qda-tree-depth node)))))

(defun* qda-node-list-all (&key node
				&key (node-num nil node-num-set-p)
				&key (node-path nil node-path-set-p)
				&key (node-memo nil node-memo-set-p)
				&key (node-update nil
						  node-update-set-p)
				&key (truncate-path nil trunc-set-p))
  (let ((buf (set-buffer (get-buffer-create qda-node-list-buffer)))
	node-number-format-string)
    (unless node (setq node qda-root))
    ;; calculate padding needed
    (setq node-number-format-string
	      (if (and node-num-set-p node-path-set-p)
		  (concat "\n"
			  (qda-node-number-with-padding-format-string node 3))
		"\n%s"))
    (erase-buffer)
    (insert (format "%s\nNode list for project %s\n%s\nNode: %s\n%s\n"
		    qda-line-string
		    qda-project-name
		    (current-time-string)
		    (if (eq node qda-root) "()"
		      (qda-node-numerical-address node))
		    qda-line-string))
    (qda-dotree (n node)
      (unless (eq n qda-root)
	(when node-num-set-p
	  (insert (format node-number-format-string
			  (qda-node-numerical-address n))))
	(when node-path-set-p
	  (insert (concat (if node-num-set-p "" "\n")
			  (if trunc-set-p
			      (qda-node-abbreviated-path-name n truncate-path)
			    (qda-node-path-name n)))))
	(when (and node-memo-set-p (qda-node-memo n))
	  (insert (format "\n%s\nMemo for %s\n%s\n%s"
			  qda-line-string
			  (qda-node-numerical-address n)
			  (qda-node-memo n)
			  qda-line-string)))
	(when node-update-set-p
	  (let ((update-slot (qda-node-update n)))
	    (when update-slot
	      (insert (format "\n%s\nUpdate slot for %s\n%s%s"
			      qda-line-string
			      (qda-node-numerical-address n)
			      (pp-to-string update-slot)
			      qda-line-string)))))))

    (goto-char (point-min))
    (pop-to-buffer buf)))

(defun qda-node-list-just-nums (node)
  (interactive (list (qda-completing-read-node "Node: " "()")))
  (let ((n (qda-find-node node)))
    (unless n (qda-error "Can't find node %s" node))
    (qda-node-list-all :node n :node-num)))

(defun qda-node-list-just-path (node)
  (interactive (list (qda-completing-read-node "Node: " "()")))
    (let ((n (qda-find-node node)))
      (unless n (qda-error "Can't find node %s" node))
      (qda-node-list-all :node n :node-path)))

(defun qda-node-list-short (node)
  (interactive (list (qda-completing-read-node "Node: " "()")))
    (let ((n (qda-find-node node)))
      (unless n (qda-error "Can't find node %s" node))
      (qda-node-list-all :node n :node-num  :node-path)))

(defun qda-node-list-long (node)
  (interactive (list (qda-completing-read-node "Node: " "()")))
    (let ((n (qda-find-node node)))
      (unless n (qda-error "Can't find node %s" node))
      (qda-node-list-all :node n :node-num :node-path :node-memo)))

(defun qda-node-list-full (node)
  (interactive (list (qda-completing-read-node "Node: " "()")))
    (let ((n (qda-find-node node)))
      (unless n (qda-error "Can't find node %s" node))
      (qda-node-list-all
       :node n :node-num :node-path :node-memo :node-update)))


;; todo: there should be some error handling here: what do we do if
;; add-node fails?

(defun qda-build-tree (tree parent)
  "Build TREE under PARENT.
TREE is a tree of new nodes, each node is a list with its name as car
and its childrens' names as cdr."
  (let ((n nil))
    (dolist (node tree)
      (setq n (qda-add-node (car node) parent))
      (when (cdr node)
	(qda-build-tree (cdr node)
			(qda-node-numerical-address n))))))

(defun qda-node-make-build-tree-aux (node)
  (let ((tree '()))
    (dolist (k (qda-node-children node))
      (push (qda-node-make-build-tree-aux k) tree))
    (cons (qda-node-name node) (nreverse tree))))

(defvar qda-build-tree-buffer "*QDA-build-tree*")

(defun qda-node-make-build-tree (node)
  (interactive
   (list (qda-completing-read-node "Make build tree script for node: ")))
  (let ((buf (set-buffer (get-buffer-create qda-build-tree-buffer)))
	(n (or (qda-find-node node) (qda-error "Can't find node %s" node))))
    (set-buffer-modified-p nil)
    (erase-buffer)
    (emacs-lisp-mode)
    (insert
     (pp-to-string
      `(qda-build-tree
	(quote ,(list (qda-node-make-build-tree-aux n)))
	(quote ,(if (eq n qda-root)
		    (list 0)
		  (qda-node-parent n))))))
    (goto-char (point-min))
    (display-buffer buf)))

(provide 'qda-tree)

;;; qda-tree.el ends here
