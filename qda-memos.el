;;; qda-memos.el --- qda memo handling functions

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
;; Mon Aug 9 01:39:54 1999 Reorganised memos: got rid of the global
;; list and set up a generic interface to memo functions.  It seems to
;; work!

;;; Code:

;(require 'qda-modes)
;; structure for a type of memo.
;; memos can be attached to any object, as long
;; as it is possible to make some sort of predicate
;; function that identifies its type.

;; We have to define all sorts of slots to make this
;; work...And make sure we have the functions defined
;; that are mentioned by the slots

;; The big probs are:
;;   * there is no memo slot in a structure by fiat; this
;;     is to save having loads of unused nil slots.
;;   * some object types aren't created by defstruct; they
;;     are composites (q.v. qda-node-text-unit).

(eval-when-compile
  (require 'cl)
  (defvar qda-view-tree-centric-p)
  (defvar qda-view-tree-buffer)
  (defvar qda-project-name)
  (defvar qda-memo-list))
  
;; todo: change identifier-func slot to something more informative,
;; maybe pname-func

(defstruct qda-memo-obj
  type-p				; predicate function for this type
  obj-accessor				; accessor for the object
  memo-accessor			        ; accessor for the memo for this type
  memo-creator 
  memo-destroyer
  identifier-func			; function that makes identifier for this type
  insert-header-func			; insert memo header function
  list-all-func)			; function that shows all memos of this type


;; global list of all memos that are being edited.  This is
;; used in the query save function when exiting Emacs so that
;; all memos get saved.

(defvar qda-memos-pending-save '()
  "Memos that have not been saved yet (still being edited).")

(defconst qda-memo-obj-list
  (list (make-qda-memo-obj
	 :type-p 'qda-node-p
	 :obj-accessor 'qda-find-node-strict
	 :memo-accessor 'qda-node-memo
	 :memo-creator 'make-qda-node-memo
	 :memo-destroyer 'delete-qda-node-memo
	 :identifier-func 'qda-node-make-memo-identifier
	 :insert-header-func 'qda-insert-node-memo-header
	 :list-all-func 'qda-node-memo-list-all)
	(make-qda-memo-obj
	 :type-p 'qda-node-text-unit-p
	 :obj-accessor 'qda-find-node-text-unit
	 :memo-accessor 'qda-node-text-unit-memo
	 :memo-creator 'make-qda-node-text-unit-memo
	 :memo-destroyer 'delete-qda-node-text-unit-memo
	 :identifier-func 'qda-node-text-unit-make-memo-identifier
	 :insert-header-func 'qda-insert-node-text-unit-memo-header
	 :list-all-func nil)
	(make-qda-memo-obj
	 :type-p 'qda-doc-p
	 :obj-accessor 'qda-find-doc
	 :memo-accessor 'qda-doc-memo
	 :memo-creator 'make-qda-doc-memo
	 :memo-destroyer 'delete-qda-doc-memo
	 :identifier-func 'qda-doc-make-memo-identifier
	 :insert-header-func 'qda-insert-doc-memo-header
	 :list-all-func 'qda-doc-memo-list-all))
  "List of memo object types.")

(defun qda-find-memo-obj (obj)
  "Return the qda-memo-obj structure for OBJ."
  (loop
   for obj-stuff in qda-memo-obj-list
   thereis (if (funcall (qda-memo-obj-type-p obj-stuff) obj)
	       obj-stuff
	     nil)))

(defun qda-find-memo (obj)
  (let ((obj-stuff (qda-find-memo-obj obj)))
     (when obj-stuff
       (funcall (qda-memo-obj-memo-accessor obj-stuff) obj))))

(defun qda-make-memo (obj text)
  (let ((obj-stuff (qda-find-memo-obj obj)))
    (when obj-stuff
      (funcall (qda-memo-obj-memo-creator obj-stuff) obj text))))

(defun qda-delete-memo (obj)
  (let ((obj-stuff (qda-find-memo-obj obj)))
    (when obj-stuff
      (funcall (qda-memo-obj-memo-destroyer obj-stuff) obj))))

(defun qda-memo-identifier (obj)
  (let ((obj-stuff (qda-find-memo-obj obj)))
    (when obj-stuff
      (funcall (qda-memo-obj-identifier-func obj-stuff) obj))))

(defun qda-memo-insert-header (obj)
  (let ((obj-stuff (qda-find-memo-obj obj)))
    (when obj-stuff
      (funcall (qda-memo-obj-insert-header-func obj-stuff) obj))))

(defun qda-memo-obj-find (obj)
    (loop
     for obj-stuff in qda-memo-obj-list
     thereis (funcall (qda-memo-obj-obj-accessor obj-stuff) obj)))
    
;;;  (let ((obj-stuff (qda-find-memo-obj obj)))
;;;    (when obj-stuff
;;;      (funcall (qda-memo-obj-obj-accessor obj-stuff) obj))))


(defvar qda-edit-memo-header-string (format "Memo for %%s.\n%s\n\n" qda-line-string)
  "String inserted as header when editing memos.")

(defvar qda-memo-buffer-format-string "Memo: %s"
  "Format for the names of memo buffers.
Should have one `%s' format specifier for the identifier of the memo.")

;; these are for the benefit of the compiler
(defvar qda-memo-text-start)
(defvar qda-memo)

(defun qda-memo-end-edit ()
  "Save changes made to memo.
If there is nothing left of the memo, it is deleted."
  (interactive)
  (when (eq major-mode 'qda-memo-mode)
    (unless (boundp 'qda-memo-text-start)
      (error "I see no memo here"))
	(let ((text (buffer-substring-no-properties qda-memo-text-start (point-max)))
	      obj win)
	  ;; qda-memo is a  buffer local variable defined
	  ;; in qda-memo-mode	  
	  (when (setq obj (qda-memo-obj-find qda-memo))
	    ;; if obj still exists, register changes, otherwise kill the buffer
	    (string-match text "[ \t\n]+")
	    ;; if there is no text left, delete the memo
	    ;; in fact, to change the memo, delete it and add
	    ;; new memo unless text is "", this is the simplest
	    ;; way of registering the changes.
	    (qda-delete-memo obj)
	    (unless (= (length text)
		       (- (match-end 0) (match-beginning 0)))	  
	      (qda-make-memo obj text)))
	  (setq qda-memos-pending-save
		(delete* qda-memo
			 qda-memos-pending-save :test 'equal))
	  (when (or obj
		    (y-or-n-p "This object no longer exists, kill the buffer? "))
	    (set-buffer-modified-p nil)
	    (kill-buffer nil)
	    (if qda-view-tree-centric-p
		(if qda-view-tree-window-config
		    (set-window-configuration qda-view-tree-window-config)
		  (when (and (get-buffer-window qda-view-tree-buffer)
			     (setq win (get-buffer-window (current-buffer)))
			     (not (eq win (get-buffer-window qda-view-tree-buffer))))
		    (delete-window))))))))
;;; 	    (when (and qda-view-tree-centric-p
;;; 		       (get-buffer-window qda-view-tree-buffer)
;;; 		       (setq win (get-buffer-window (current-buffer)))
;;; 		       (not (eq win (get-buffer-window qda-view-tree-buffer))))
;;;	      (delete-window))))))
		       
		       

;; for the benefit of the compiler
(defvar qda-memo-text-start) 

(defun qda-memo-insert-memo (obj)
  "Insert text from MEMO into the current buffer."
  (qda-memo-insert-header obj)
  ;; remember the start of the memo text if we are editing it
  (when (boundp 'qda-memo-text-start)
    (setq qda-memo-text-start (point-marker)))
  (insert (or (qda-find-memo obj) "")))


(defun qda-memo-edit (obj)
  (let ((memo (qda-find-memo obj))
	(buf (format qda-memo-buffer-format-string (qda-memo-identifier obj))))
    (unless (get-buffer buf)
      (setq buf (get-buffer-create (format qda-memo-buffer-format-string 
					   (qda-memo-identifier obj))))
      (set-buffer buf)
      (erase-buffer)
      (qda-memo-mode)
      (buffer-disable-undo)
      (qda-memo-insert-memo obj)
      (goto-char qda-memo-text-start)
      (add-text-properties (point-min) qda-memo-text-start
			   '(rear-nonsticky t front-sticky t intangible t read-only t))
      (buffer-enable-undo)
      (setq qda-memo (qda-memo-identifier obj))
      (pushnew qda-memo qda-memos-pending-save :test 'equal))
    (pop-to-buffer buf)
    (message (substitute-command-keys
	      "Press \\[qda-memo-end-edit] to save changes."))))
  
(defun qda-memo-offer-save ()
  "Ask if memos that haven't been saved should be saved."
  (let (buf)
    (dolist (memo qda-memos-pending-save t)
      (when (and (setq buf (get-buffer	
			    (format qda-memo-buffer-format-string memo)))
		 (y-or-n-p (format "Save memo %s? " memo)))
	(set-buffer buf)
	(qda-memo-end-edit)))))

(defun qda-save-memos (file)
  "Save memos in FILE."
  (qda-save-object qda-memo-list file))

(defun qda-load-memos (file)
  "Load memos from FILE."
  (setq qda-memo-list (qda-get-object file)))

;;; listing all memos

(defvar qda-memo-list-buffer "*QDA-memos*")

(defun qda-memo-list-all ()
  (interactive)
  (let ((buf (set-buffer (get-buffer-create qda-memo-list-buffer)))
	point)
    (erase-buffer)
    (insert (format "%s\nMemos in %s project\n%s\n\n"
		    qda-line-string
		    qda-project-name
		    qda-line-string))
    (setq point (point))
    (loop
     for obj-stuff in qda-memo-obj-list
     do (when (qda-memo-obj-list-all-func obj-stuff)
	  (funcall (qda-memo-obj-list-all-func obj-stuff))))
    (goto-char point)
    (pop-to-buffer buf)))
  
(provide 'qda-memos)

;;; qda-memos.el ends here
