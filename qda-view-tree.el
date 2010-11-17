;;; qda-view-tree.el --- qda tree viewing functions

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

;;;; Tree viewing functions.

;;; Commentary:
;;

;;; History:
;;
;;  Sat Aug  7 00:13:41 1999
;;  Added deletion of overlays to `qda-view-tree-show-children' and
;;  qda-view-tree-show-all.
;;
;;  Tue Aug 17 13:08:43 1999
;;
;;  Added `qda-view-tree-hide-all-children'
;;
;;  Made viewing tree faster by changing from recursive functions
;;  to iterative ones and eliding some functions.  This was initiated
;;  because when lots of children are hidden (say by
;;  `qda-tree-hide-all-children') viewing the tree gets slowed down a lot.
;;
;;  Made `qda-node-list' a buffer local variable, so that we don't
;;  have to keep recalculating it pointlessly.
;;
;;  Mon Sep 11 19:50:07 2000
;;
;;  Changed the whole hiding nodes method to make it faster
;;  
;;
;; Todo:
;;
;; It's worth spending time making this whole module as fast as
;; possible; viewing the tree makes for most of the ``feel'' of the
;; program, and it should be as snappy as we can make it.
;;
;;
;; Add mouse facilities for hiding/showing.
;;
;; Fri Nov 24 23:00:44 2000
;; What about marking nodes for operations?
;;
;; Sun Nov 26 17:35:32 2000
;; Rewrote `qda-view-tree-aux' to make it faster

(eval-when-compile
  (require 'cl)
  (require 'qda-misc)
  (require 'qda-indexing)
  (require 'qda-tree)
  (defvar qda-node-list)
  (defvar qda-hidden-kids)
  (defvar tree-highlight-overlay)
  (defvar qda-hidden-kids)
  (defvar qda-view-indexing-node)
  (defvar prompt)
  (defvar qda-select-node)
  (defvar qda-view-tree-mode-map)
  (defvar node))


; (require 'electric)
; (require 'qda-tree)
; (require 'qda-indexing)
; (require 'qda-memos)

;;; Code:

(defgroup qda-tree nil "QDA tree" :group 'qda)

(defvar qda-view-tree-buffer "*QDA-tree*")
(defvar qda-node-info-buffer "*QDA-node-info*")

(defvar qda-tree-last-branch  " `-")
(defvar qda-tree-branch       " |-")
(defvar qda-tree-connector    " | ")
(defvar qda-tree-no-connector "   ")

(defvar qda-view-tree-highlight-regexp
   "^\\([`| ()-]+\\)?\\(([0-9 ]*)\\)\\(.*\\)"
  "Regexp that matches the whole of a tree branch line.")

;; these find the matching groups in the regexp
(defvar qda-v-t-h-regexp-normal-highlight 3)
(defvar qda-v-t-h-regexp-marked-highlight 2)

(defvar qda-view-tree-highlight-face-foreground "IndianRed"
  "Foreground colour used when highlighting current node in view tree mode.")

;; highlighting slows down scrolling on my old computer, make it optional.

(defcustom qda-view-tree-highlight-current-node-p t
  "*Non-nil means highlight node at point in view tree buffer."
  :group 'qda-tree
  :type 'boolean)

;; marks after the node name to show its status
;; these should really be put in `qda-node-slot-display-strings-alist'

(defvar qda-view-tree-indexing-mark " *")
(defvar qda-view-tree-update-mark   " &")
(defvar qda-view-tree-memo-mark     " %") ; like scrolls in nethack :-)

;; mark to put at the end of every line of the tree (mainly for things
;; like latexifying the tree.

(defvar qda-view-tree-eol-mark "")

;;; View tree functions

(defsubst qda-view-tree-get-node-at-point ()
  ;; assume we are in an appropriate buffer
;;;  (when (eq major-mode 'qda-view-tree-mode)
    (save-excursion
      (beginning-of-line)
      (let ((l (count-lines 1 (point))))
	(if (= l 0)
	    '(0)
	  (nth (1- l) qda-node-list)))))

;; this returns a string representation of node
;; so that we can represent root as "()"
(defun qda-view-tree-get-node-at-point-prompt ()
  (let ((n (qda-view-tree-get-node-at-point)))
    (cond ((null n) nil)
	  ((equal n '(0)) "()")
	  (t (format "%s" n)))))

(defsubst goto-node-line (n)
  ;; assume we are in an appropriate buffer
  (when n
    (goto-line
     (if (equal n qda-root)
	 1
       (+ 2 (position (qda-node-numerical-address n)
			  qda-node-list
		      :test 'equal))))))

(defvar qda-node-list)

(defun qda-node-path-name-symbol (node)
  (intern (format "%s" (qda-node-numerical-address node))))

;; todo: it would be good if we could print parts of the tree instead
;; of the whole thing.  This should be easily do-able.

; (defun qda-view-tree-aux ()
;   (interactive)
;   (setq buffer-invisibility-spec nil)
;      (let (branches n inv st)
;        (insert "()\n")
;        (loop for node in qda-node-list
; 	 do
; 	 (setq inv '())
; 	 (setq node (qda-find-node node)
; 	       branches ""
; 	       n node)
; 	 (loop
; 	     until (eq (setq n (qda-find-node (qda-node-parent n)))
; 		       qda-root)
; 	     do
; 	     ;; add an invisibility property for each higher level
; 	     ;; so that we can selectively hide the node using
; 	     ;; buffer-invisibility-spec.  Each node inherits all the
; 	     ;; invisibility specs of its ancestors
; 	     (push (qda-node-path-name-symbol n) inv)
; 	     (setq branches (concat (if (qda-last-sib-p n)
; 					qda-tree-no-connector
; 				      qda-tree-connector)
; 				    branches)))
; 	 (setq st (- (point) 1))
; 	 (insert (concat 
; 		  branches
; 		  (if (qda-last-sib-p node)
; 		      qda-tree-last-branch
; 		    qda-tree-branch)
; 		  "("
; 		  (qda-node-number node)
; 		  ") "
; 		  (qda-node-name node)
; 		  (if (qda-node-indexing node) qda-view-tree-indexing-mark "")
; 		  (if (qda-find-node-slot-type 'memo node) qda-view-tree-memo-mark "")
; 		  (if (qda-find-node-slot-type 'update node)
; 		      qda-view-tree-update-mark "")
; 		  qda-view-tree-eol-mark
; 		  "\n"))
; 	 (put-text-property st (point) 'invisible inv)
; 	 )))


(defun qda-view-tree-aux ()
  (setq buffer-invisibility-spec nil)
  (insert "()\n")
  (let* ((nodes qda-node-list)
	 ;; node-predecessors records useful info about all
	 ;; predecessors (whether each is the last sibling and each node
	 ;; address) for formatting and invisibility specs.
	 (node-predecessors '())
	 (cur-node (car nodes))
	 (next-node (cadr nodes))
	 cur-l next-l last-sib-p n
	 branches inv st)
    (while nodes
      (setq n (qda-find-node cur-node))
      (setq cur-l (qda-node-level cur-node)
	    next-l (qda-node-level next-node))
      (setq last-sib-p (qda-last-sib-p n))
      ;; insert the current node
      (setq branches "")
      (dolist (b node-predecessors)
	(setq branches
	      (concat
	       (if (car b)		 
		   qda-tree-no-connector 
		 qda-tree-connector)
	       branches)))
      ;; add the current node, with info
      (setq branches
	    (concat
	     branches
	     (if last-sib-p
		 qda-tree-last-branch
	       qda-tree-branch)
	     "("
	     (number-to-string (qda-node-number n))
	     ") "
	     (qda-node-name n)
	     (if (qda-node-indexing n) qda-view-tree-indexing-mark "")
	     (if (qda-find-node-slot-type 'memo n) qda-view-tree-memo-mark "")
	     (if (qda-find-node-slot-type 'update n)
		 qda-view-tree-update-mark "")
	     qda-view-tree-eol-mark
	     "\n"))
      (setq st (- (point) 1))
      (insert branches)
      ;; add invisibility specs for all predecessors of the node
      (put-text-property st (point) 'invisible
			 (mapcar #'(lambda (b)
				     (cdr b))
				 node-predecessors))
      ;; update predecessor list
      (cond ((< cur-l next-l)
	     (setq node-predecessors
		   (cons (cons last-sib-p (qda-node-path-name-symbol n))
			 node-predecessors)))
	    ((> cur-l next-l)
	     ;; forget about all levels below parent of next node
	     (setq node-predecessors
		   (nthcdr (- (length node-predecessors) (1- next-l))
			   node-predecessors))))
      (setq nodes (cdr nodes))
      (setq cur-node (car nodes)
	    next-node (if (cdr nodes) (cadr nodes) nil)))))

;; todo: go to nearest line thing doesn't work
(defun qda-view-tree ()
  "View the tree.  Nodes that have indexing have a '*' appended to their name."
  (interactive)
  (if (and
       (not qda-view-tree-update-flag)
       (get-buffer qda-view-tree-buffer))
	(pop-to-buffer qda-view-tree-buffer)
    (let ((buf (set-buffer (get-buffer-create qda-view-tree-buffer)))
	  (old-buf-inv-spec (if (boundp 'buffer-invisibility-spec)
				buffer-invisibility-spec
			      nil))
	  (cur-n (if (boundp 'qda-node-list)
		     (qda-view-tree-get-node-at-point)
		   qda-root)))
;;;	(pop-up-frames t))
      (qda-view-tree-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (qda-view-tree-aux)
      (setq buffer-read-only t)
      (goto-char (point-min))
      ;; hide all the old hidden bits
      (setq buffer-invisibility-spec old-buf-inv-spec)
      ;; go to line nearest to the old one
      (goto-node-line (qda-node-extant-ancestor cur-n))
      (qda-view-tree-make-overlay)
      (setq qda-view-tree-update-flag nil)
      (pop-to-buffer buf))))


(defun qda-view-tree-make-overlay ()
  (and
   qda-view-tree-highlight-current-node-p
   (looking-at qda-view-tree-highlight-regexp)
   (progn
     (setq tree-highlight-overlay
	   (make-overlay (match-beginning qda-v-t-h-regexp-normal-highlight)
			 (match-end qda-v-t-h-regexp-normal-highlight)))
     (overlay-put tree-highlight-overlay 'face
		  (cons 'foreground-color qda-view-tree-highlight-face-foreground)))))

(defsubst qda-update-node-info ()
  (when (get-buffer-window qda-node-info-buffer)
    (qda-view-node-info (or (qda-view-tree-get-node-at-point)
			    qda-root))))

(defsubst qda-view-tree-move-overlay ()
 (and
  qda-view-tree-highlight-current-node-p
  (looking-at qda-view-tree-highlight-regexp)
  (move-overlay tree-highlight-overlay
		(match-beginning qda-v-t-h-regexp-normal-highlight)
		(match-end qda-v-t-h-regexp-normal-highlight))))

; (defun qda-view-tree-next-line ()
;   (interactive)
;   (next-line 1)
;   (beginning-of-line)
;   (qda-view-tree-move-overlay)
;   (qda-update-node-info))

; (defun qda-view-tree-previous-line ()
;   (interactive)
;   (previous-line 1)
;   (beginning-of-line)
;   (qda-view-tree-move-overlay)
;   (qda-update-node-info))

;; todo: these next two wraparound at the beginning/end of the buffer;
;; I think this is good, but perhaps it should be optional.
(defun qda-view-tree-next-line ()
  (interactive)
  (while
      (and (= 0 (forward-line 1))
	   (loop
	    for n-sym in (get-text-property (point) 'invisible)
	    thereis (assq n-sym buffer-invisibility-spec))))
  (when (eobp)
    (goto-char (point-min)))
  (qda-view-tree-move-overlay)
  (qda-update-node-info))

(defun qda-view-tree-previous-line ()
  (interactive)
  (when (bobp)
    (goto-char (point-max)))
  (while
      (and (= 0 (forward-line -1))
	   (loop
	    for n-sym in (get-text-property (point) 'invisible)
	    thereis (assq n-sym buffer-invisibility-spec))))
  (qda-view-tree-move-overlay)
  (qda-update-node-info))


(defun qda-view-tree-scroll-down ()
  (interactive)
  (scroll-down nil)
  (beginning-of-line)
  (qda-view-tree-move-overlay)
  (qda-update-node-info))

(defun qda-view-tree-scroll-up ()
  (interactive)
  (scroll-up nil)
  (beginning-of-line)
  (qda-view-tree-move-overlay)
  (qda-update-node-info))

;; these all substantially rewritten Mon Sep 11 19:24:12 2000

(defun qda-view-tree-toggle-hide-children ()
  (interactive)
  (let (n n-inv-prop st ov n-path-sym)
    (when (and (setq n (qda-find-node (qda-view-tree-get-node-at-point)))
	       (qda-node-children (qda-find-node n)))
      (if (assoc (setq n-path-sym (qda-node-path-name-symbol n))
		 buffer-invisibility-spec)
	  (remove-from-invisibility-spec (cons n-path-sym t))
	(add-to-invisibility-spec (cons n-path-sym t)))
      ;; this refreshes display without an annoying flash:
      ;; is there a better way to do this?
      (qda-view-tree-move-overlay))))


(defun qda-view-tree-hide-children ()
  (interactive)
  (let (n)
    (when (and (setq n (qda-find-node (qda-view-tree-get-node-at-point)))
	       (qda-node-children n))
      (add-to-invisibility-spec (cons (qda-node-path-name-symbol n) t))))
  (when (interactive-p) (qda-view-tree-move-overlay)))


(defun qda-view-tree-show-children ()
  (interactive)
    (let (n)
      (when (and (setq n (qda-find-node (qda-view-tree-get-node-at-point)))
		 (qda-node-children n))
      (remove-from-invisibility-spec (cons (qda-node-path-name-symbol n) t))))
    (when (interactive-p) (qda-view-tree-move-overlay)))

(defun qda-view-tree-show-all ()
  (interactive)
  (setq buffer-invisibility-spec '())
  (qda-view-tree-move-overlay))

(defun qda-view-tree-hide-all-children ()
  (interactive)
  (goto-char (point-min))
  (while (= (forward-line 1) 0)
    (qda-view-tree-hide-children))
  (goto-char (point-min))
  (qda-view-tree-move-overlay))

;; todo: write updating tree function.

; (defun qda-update-tree (node-list)
;   (let ((buf (get-buffer qda-view-tree-buffer))
; 	(start-point nil)
; 	(opoint nil))
;     (when buf
;       (setq opoint (point))
;       (dolist (this-node node-list)
; ;	(setq this-node (qda-node-parent this-node))
; 	(goto-line (+ 1 (qda-nth-in-node-list this-node)))
; 	(beginning-of-line)
; ; 	(when (qda-last-sib-p this-node)
; ; 	  ;; change branch to last-branch
; ; 	  (forward-line -1)
; ; 	  (setq start-point (point))
; ; 	  (forward-char (length qda-tree-branch))
; ; 	  (delete-region start-point (point))
; ; 	  (insert qda-tree-last-branch)
; ; 	  (forward-line 1))
; 	(setq start-point (point))
; 	(if (qda-last-sib-p this-node)
; 	    (goto-line (+ 1 (qda-nth-in-node-list (qda-next-sib this-node))))
; 	  (goto-char (point-max)))
; 	(beginning-of-line)
; 	(delete-region start-point (point))
; 	(qda-view-tree-aux this-node))
;       (goto-char opoint))))

;; This is intended for saving hidden nodes with the tree so you can
;; restore all the hidings as well.
;;; (defun qda-view-tree-hidden-kids-nodes ()
;;;   "Return a list that may be used to restore hidden kids of the tree."
;;;   (let ((buf (get-buffer qda-view-tree-buffer)))
;;;     (when (and buf
;;; 	       (progn
;;; 		 (set-buffer buf)
;;; 		 qda-hidden-kids))
;;;       (loop for hidden-kid in qda-hidden-kids
;;; 	    collect (list (car hidden-kid))))))

(defun qda-view-tree-remember-window-config ()
  (setq qda-view-tree-window-config (current-window-configuration)))

(defun qda-view-tree-forget-window-config ()
  (setq qda-view-tree-window-config nil))

;;; Node info functions

;; todo: make all of the formatting stuff cleaner

(defun qda-node-info-string (node)
  (let ((node-stats (qda-indexing-stats (qda-node-indexing node))))
    (format "node: %s %s\nname: %s \n%sindexing: %d text %s in %d %s"
	    (qda-node-numerical-address node)
	    (qda-node-slot-display-strings node)
;;;		    (qda-node-name node) 
	    (qda-node-path-name node)
	    (format "description: %s\n"
		    (or (qda-node-description node)
			""))
	    (qda-indexing-num-text-units node-stats)
	    (sing-or-plural-noun
	     "unit" "units" (qda-indexing-num-text-units node-stats))
	    (qda-indexing-num-docs node-stats)
	    (sing-or-plural-noun
	     "doc" "docs" (qda-indexing-num-docs node-stats)))))

(defun qda-insert-node-info (node)
  (insert (qda-node-info-string node)))

(defun qda-view-node-indexing-header (node)
  "Insert a header into a buffer for viewing indexing for NODE."
 (qda-insert-node-info node)
  (insert (format "\n\n%s\n" qda-line-string)))

(defun qda-view-node-indexing-footer (node)
  "Insert a footer into a buffer for viewing indexing for NODE."
  (insert (format "\n\n%s" qda-line-string)))

(defun qda-view-node-info (node)
  "View info about NODE."
  ;; todo: add options for what to view.
  (interactive
   (list
    (qda-completing-read-node "Node: ")))
  (unless (setq node (qda-find-node node))
    (qda-error "Can't find node %s" node))
  (let ((pop-up-frames nil))
    (set-buffer (get-buffer-create qda-node-info-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (qda-insert-node-info node)
    (qda-view-mode)
    (shrink-window-if-larger-than-buffer
     (display-buffer qda-node-info-buffer))
    (qda-view-tree-remember-window-config)))

(defun qda-view-this-node-info ()
  "Give information about node at point."
  (interactive)
  (let ((node (qda-view-tree-get-node-at-point)))
    (when node
      (qda-view-node-info node))))

(defun qda-view-this-node-info-toggle ()
  (interactive)
  (let ((win (get-buffer-window qda-node-info-buffer)))
    (if win
	(progn (delete-window win) (qda-view-tree-forget-window-config))
      (qda-view-this-node-info))))

(defun qda-view-node-indexing (node)
  "View indexing at NODE."
  (interactive
   (list (qda-completing-read-node "Node: ")))
  (let ((this-node (qda-find-node node))
	(buf-name (format "%s" node))
	buf memo)
    (cond
     ((null this-node)
      (qda-error "Can't find node %s." node))
     ((null (qda-node-indexing this-node))
      (qda-error "No indexing at node %s."
		   (qda-node-numerical-address this-node))))
    (when (get-buffer buf-name)
      ;; perhaps we should check whether node has changed since
      ;; last view?  todo: think about adding attributes slot
      ;; to node to mark changes etc.
;;; 	    (progn
;;; 	      (setq buf (set-buffer (get-buffer buf-name)))
;;; 	      (setq buffer-read-only nil)
;;; 	      (erase-buffer))
      ;; Thu Sep 20 18:24:19 2001
      ;; just kill the buffer: sorts out a problem with overlays in
      ;; transcription mode.
      (kill-buffer (get-buffer buf-name)))
    (setq buf (set-buffer (get-buffer-create buf-name)))
    (qda-view-node-indexing-header this-node)
    (setq memo (qda-find-memo this-node))
    (when memo (insert (format "%s\n%s\n" memo qda-line-string)))
    (qda-insert-indexing (qda-node-indexing this-node))
    (qda-view-node-indexing-footer this-node)
    (qda-view-indexing-mode)
    ;; this local variable allows a function to get the node
    ;; address of the indexing in the current buffer
    ;; it is here rather than in the mode definition because
    ;; view-indexing-mode could be used for other data structures
    (make-local-variable 'qda-view-indexing-node)
    (setq qda-view-indexing-node (qda-node-numerical-address this-node))
    (pop-to-buffer buf)
    (goto-char (point-min))))

(defun qda-view-this-node-indexing ()
  "View indexing of node at point."
  (interactive)
  (qda-view-node-indexing (qda-view-tree-get-node-at-point)))

;;; Electric node selection functions

;;; select a node from the tree

(require 'electric)

(defvar qda-select-node-map '())
;; todo: undefine lots of keys so you can't do all
;; sorts of confusing things like visit another
;; buffer while selecting a node.

(unless qda-select-node-map
  (let ((map (copy-keymap qda-view-tree-mode-map)))
    (define-key map "q" 'qda-select-node-exit)
    (define-key map "\C-m" 'qda-select-this-node)
    (setq qda-select-node-map map)))

(defun qda-select-this-node ()
  (interactive)
  (throw 'qda-select-node-quit (qda-view-tree-get-node-at-point)))

(defun qda-select-node-exit ()
  (interactive)
  (throw 'qda-select-node-quit nil))

(defun qda-select-node (&optional prompt)
  (save-window-excursion
    (qda-view-tree)
    (unwind-protect
	(progn
	  (use-local-map qda-select-node-map)
	  (let ((mesg
		 (concat
		  (substitute-command-keys
		   (concat "(\\[qda-select-this-node] selects current,"
			   " \\[qda-select-node-exit] quits) "))
		  (or prompt ""))))
	    (setq node
		  (catch 'qda-select-node-quit
		    (save-window-excursion
		      (save-window-excursion
			(Electric-command-loop
			 'qda-select-node-quit
			 mesg
			 t)))))))
      (use-local-map qda-view-tree-mode-map))))

(defun qda-select-node-from-minibuffer ()
  (interactive)
  (throw 'qda-got-node
	 (qda-select-node prompt)))

;; Not entirely sure about the correctness of this

(defun qda-completing-read-node (prompt &optional default)
  "Ask for a node with completion using PROMPT.
Insists on having a node.
If DEFAULT is nil, a default is derived from the current context.
If DEFAULT is non-nil, it is offered as the default node."
  (condition-case err
      (let ((old-map (copy-keymap minibuffer-local-must-match-map))
	    entry)
	(unless default
	  (setq default
		(cond ((eq major-mode 'qda-view-tree-mode)
		       (let ((n (qda-view-tree-get-node-at-point-prompt)))
			 (if (or (null n)
				 (eq n qda-root)
				 (null (qda-find-node (read n))))
			     "()"
			   n)))
		      ((eq major-mode 'qda-view-indexing-mode)
		       (if (qda-find-node qda-view-indexing-node)
			   qda-view-indexing-node
			 nil))
		      (t nil))))
	(unwind-protect
	    (progn
	      (define-key
		minibuffer-local-must-match-map
		"\C-xt" 'qda-select-node-from-minibuffer)
	      (when default (setq default (format "%s" default)))
	      (or
	       (catch 'qda-got-node
		 (progn
		   (setq entry
			 (completing-read
			  (concat
			   ;; why doesn't this work?
;;;			   (substitute-command-keys
;;;			    "(\\[qda-select-node-from-minibuffer] for the tree) ")
			   "(C-xt for the tree) "
			   prompt)
			  (qda-node-completion-list) nil t
			  default
			  nil))
		   (cond ((null entry) nil)
			 ((equal entry "()") '(0))
			 (t (read entry)))))
	       (error err)))
	  (setq minibuffer-local-must-match-map old-map)))
    (error (progn
	     (message "Please enter a node")
	     (sit-for 1)
	     (qda-completing-read-node prompt)))))

;;; Marking nodes

;; Sat Nov 25 01:48:39 2000
;; not really implemented yet

(defvar qda-view-tree-marked-face-foreground "GoldenRod")

(defvar qda-v-t-marked-node-list '()) ; make a buffer local variable when this works

; (defun qda-view-tree-mark-node ()
;   (interactive)
;   (let (ov n)
;   (when (and
; 	 (looking-at qda-view-tree-highlight-regexp)
; 	 (setq n (qda-view-tree-get-node-at-point))
; 	 (not (assoc n qda-v-t-marked-node-list)))
;     (setq ov
; 	  (make-overlay (match-beginning qda-v-t-h-regexp-marked-highlight)
; 			(match-end qda-v-t-h-regexp-marked-highlight)))
;     (overlay-put ov 'face
; 		 (cons 'foreground-color qda-view-tree-marked-face-foreground))
;     (overlay-put ov 'priority 2)
;     (push (cons (qda-view-tree-get-node-at-point) ov) qda-v-t-marked-node-list))))

; (defun qda-view-tree-unmark-node ()
;   (interactive)
;   (let* ((n (qda-view-tree-get-node-at-point))
; 	 (m (and n (assoc n qda-v-t-marked-node-list))))
;     (when m
;       (delete-overlay (cdr m))
;       (setq qda-v-t-marked-node-list (delete m qda-v-t-marked-node-list)))))

(defun qda-view-tree-toggle-mark-node ()
  (interactive)
  (let (ov n m)
    (and
     (looking-at qda-view-tree-highlight-regexp)
     (setq n (qda-view-tree-get-node-at-point))
     (if (setq m (assoc n qda-v-t-marked-node-list))
	 (progn
	   (delete-overlay (cdr m))
	   (setq qda-v-t-marked-node-list
		 (delete m qda-v-t-marked-node-list)))
       (setq ov (make-overlay
		 (match-beginning qda-v-t-h-regexp-marked-highlight)
		 (match-end qda-v-t-h-regexp-marked-highlight)))
       (overlay-put ov 'face (cons 'foreground-color
				   qda-view-tree-marked-face-foreground))
       (overlay-put ov 'priority 2)
       (push (cons (qda-view-tree-get-node-at-point) ov) qda-v-t-marked-node-list)))))

(provide 'qda-view-tree)

;;; qda-view-tree.el ends here
