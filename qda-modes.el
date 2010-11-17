;;; qda-modes.el --- modes for the qda program

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
;;  This file defines all of the modes used in the program.  It sets
;;  up a general mode, qda-view-mode, from which other modes inherit
;;  properties, then add their own idiosyncracies.
;;
;;  The program is currently organised around the tree display; this
;;  gives a sense of interactive coherence.  Thus, quitting displays
;;  of indexing, documents, node information and the like returns the
;;  focus to the tree if it is displayed in another window.  This
;;  feature can be disabled by changing `qda-view-tree-centric-p' to
;;  nil.
;;
;;; History:
;;
;;; Todo:
;;
;;  Fri Sep  8 20:52:42 2000
;;  Sort out all the keybindings and the menu.  Think about using
;;  multiple keybindings  in qda-view-mode.

(require 'easymenu)
(require 'derived)

;;; Code:

(eval-when-compile
  (defvar text-unit-overlays)
  (defvar marked-indexing)
  (defvar qda-hidden-kids)
  (defvar text-unit-overlays)
  (defvar tree-highlight-overlay)
  (defvar memo-object)
  (defvar qda-memo)
  (defvar qda-node-list)
  (defvar qda-memo-text-start)
  (defvar qda-view-text-units-mode-map)          
  (defvar qda-view-indexing-mode-map)            
  (defvar qda-view-doc-mode-map)                 
  (defvar qda-view-doc-node-indexing-mode-map)   
  (defvar qda-view-tree-mode-map))           

  

;; General mode for viewing.

;; All other viewing modes are derived from this, so any general
;; key-bindings should be added here (for scrolling, quitting,
;; and the like.  More specific bindings should be added in the
;; relevant mode.

(defcustom qda-view-tree-centric-p t
  "*Non-nil, quit view windows will delete the window if the tree is visible."
  :group 'qda
  :type 'boolean)


;;; this variable remembers the view tree window configuration 
;;; so that the node info window state can be restored after viewing
;;; something.
(defvar qda-view-tree-window-config nil)

(defun qda-view-quit ()
  (interactive)
  (if (and qda-view-tree-centric-p
	   (get-buffer-window qda-view-tree-buffer))
      (if qda-view-tree-window-config
	  (set-window-configuration qda-view-tree-window-config)
	(bury-buffer)
	(delete-window))
    (bury-buffer)))

(defvar qda-view-mode-map nil)

(unless qda-view-mode-map
  (setq qda-view-mode-map (make-sparse-keymap))
  (suppress-keymap qda-view-mode-map)
  (define-key qda-view-mode-map " " 'scroll-up)
  (define-key qda-view-mode-map "\C-?" 'scroll-down)
  (define-key qda-view-mode-map "q" 'qda-view-quit)
  (define-key qda-view-mode-map "j"  'next-line)
  (define-key qda-view-mode-map "k" 'previous-line)
  (define-key qda-view-mode-map "t" 'qda-view-tree)
  (define-key qda-view-mode-map "S" 'qda-save-project)

  (define-key qda-view-mode-map [mouse-3]
    '(lambda ()
       (interactive)
       (call-interactively 'mouse-major-mode-menu)))
    
  (define-key qda-view-mode-map "Q" nil)
  (define-key qda-view-mode-map "Qn" nil)
  (define-key qda-view-mode-map "Qna" 'qda-add-node)
  (define-key qda-view-mode-map "Qnd" 'qda-delete-node)
  (define-key qda-view-mode-map "Qnc" 'qda-copy-node)
  (define-key qda-view-mode-map "Qnm" 'qda-move-node)
  (define-key qda-view-mode-map "QnM" 'qda-node-edit-memo)
  (define-key qda-view-mode-map "Qnr" 'qda-rename-node)
  (define-key qda-view-mode-map "QnR" 'qda-redescribe-node)
  (define-key qda-view-mode-map "Qnn" 'qda-renumber-node)
  (define-key qda-view-mode-map "QnS" 'qda-search-index-to-nodes)
  (define-key qda-view-mode-map "Qnt" 'qda-view-tree) ;refresh
  (define-key qda-view-mode-map "Qnu" nil)
  (define-key qda-view-mode-map "Qnuu" 'qda-node-do-update)
  (define-key qda-view-mode-map "QnuU" 'qda-node-do-all-updates)
  (define-key qda-view-mode-map "Qnue" 'qda-updatable-slot-edit)
  (define-key qda-view-mode-map "Qnus" 'qda-node-add-search-update)
  (define-key qda-view-mode-map "Qns" nil)
  (define-key qda-view-mode-map "Qnsu" 'qda-node-union)
  (define-key qda-view-mode-map "Qnsi" 'qda-node-intersection)
  (define-key qda-view-mode-map "Qnsd" 'qda-node-set-difference)
  (define-key qda-view-mode-map "Qnsx" 'qda-node-exclusive-or)
  (define-key qda-view-mode-map "Qnsv" 'qda-node-vector)
  (define-key qda-view-mode-map "Qnsm" 'qda-node-matrix)
  (define-key qda-view-mode-map "Qnsc" 'qda-node-collect))


;; todo selectively disable items in modes
(easy-menu-define
 qda-menu
 qda-view-mode-map
 "QDA menu"
 (list "QDA"
       (list
	"Nodes"
	["View tree" qda-view-tree t]
	["Add node" qda-add-node t]
	["Delete node" qda-delete-node t]
	["Copy node" qda-copy-node t]
	["Move node" qda-move-node t]
	["Rename node" qda-rename-node t]
	["Change node description" qda-redescribe-node t]
	["Change node number" qda-renumber-node t]
	["View node info" qda-view-this-node-info t]
	["Edit node memo" qda-node-edit-memo t]
	["---" nil nil]
	(list
	 "Indexing"
	 ["Add indexing" qda-node-add-indexing-maybe-marked t]
	 ["Add this indexing" qda-node-add-this-indexing t]
	 ["Add all document text units" qda-node-add-all-text-units t]
	 ["Delete indexing" qda-node-delete-indexing-maybe-marked t]
	 ["Delete this indexing" qda-node-delete-this-indexing t]
	 ["Delete all indexing" qda-node-delete-all-indexing t]
	 ["Spread indexing" qda-node-spread-indexing-maybe-marked t]
	 ["View node indexing" qda-view-this-node-indexing t]
	 ["---" nil nil]
	 ["Show where a text unit is indexed" qda-doc-show-where-text-unit-is-indexed t]
	 ["Show where this text unit is indexed" qda-doc-show-where-this-text-unit-is-indexed t])
	(list
	 "Set operations"
	 ["Union" qda-node-union t]
	 ["Set difference" qda-node-set-difference t]
	 ["Intersection" qda-node-intersection t]
	 ["Exclusive or" qda-node-exclusive-or t]
	 ["---" nil nil]
	 ["Vector" qda-node-vector t]
	 ["Collect" qda-node-collect t])
	["---" nil nil]
	["Search" qda-search-index-to-nodes t]
	(list
	 "Updatable nodes"
	 ["Add updatable search" qda-node-add-search-update t]
	 ["Edit updatable slot" qda-updatable-slot-edit t]
	 ["Update node" qda-node-do-update t]
	 ["Update all nodes" qda-node-do-all-updates t])
	["---" nil nil]
	(list
	 "Listings"
	 ["List node numbers" qda-node-list-just-nums t]
	 ["List node pathnames" qda-node-list-just-path t]
	 ["Short node list" qda-node-list-short t]
	 ["Long node list" qda-node-list-long t]
	 ["Full node list" qda-node-list-full t]))
	(list
	 "Docs"
	 ["View doc" qda-view-doc t]
	 ["View this doc" qda-view-this-doc t]
	 ["View doc node index info" qda-view-doc-node-indexing]
	 ["View this doc node index info" qda-view-this-doc-node-indexing t]
	 ["Show where a text unit is indexed" qda-doc-show-where-text-unit-is-indexed t]
	 ["Show where this text unit is indexed" qda-doc-show-where-this-text-unit-is-indexed t])
	(list
	 "Memos"
	 ["Edit node memo" qda-node-edit-memo t]
	 ["Edit doc memo" qda-doc-edit-memo t]
	 ["---" nil nil]
	 ["List all memos" qda-memo-list-all t])
	["---" nil nil]
	["Load project" qda-load-project t]
	["Create project" qda-load-project t]
	["Save project" qda-save-project t]))



(defun qda-view-mode ()
  (kill-all-local-variables)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq major-mode 'qda-view-mode)
  (setq mode-name "QDA-View")
  (use-local-map qda-view-mode-map)
  (run-hooks 'qda-view-mode-hook))

(put 'qda-view-mode 'mode-class 'special)

;; General mode from which qda-view-doc-mode and
;; qda-view-indexing-mode are derived.

;; It inherits the basic setup from qda-view-mode. All
;; keybindings that are shared by these two modes (and any
;; others yet to be created) should be put in here.

(make-empty-face 'qda-text-unit-marker-face)
(set-face-foreground 'qda-text-unit-marker-face "GoldenRod")

(defvar qda-highlight-mark-text-unit-colour "MediumVioletRed"
  "*Foreground colour for marked text units.")

;; todo: this regexp should be generated by the program
(defconst qda-view-text-units-font-lock-keywords
  '(("^\\[.+: [0-9]+\\]\\|\\[[0-9]+\\]" . 'qda-text-unit-marker-face)))

(define-derived-mode qda-view-text-units-mode qda-view-mode "QDA-View"
  ;; an indexing list of marked text-units
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(qda-view-text-units-font-lock-keywords t t))
  (make-local-variable 'text-unit-overlays)
  (setq text-unit-overlays '())
  (make-local-variable 'marked-indexing)
  (setq marked-indexing '()))

(define-key qda-view-text-units-mode-map "a" 'qda-node-add-indexing-maybe-marked)
(define-key qda-view-text-units-mode-map "d" 'qda-node-delete-indexing-maybe-marked)
;(define-key qda-view-text-units-mode-map "s" 'qda-node-spread-indexing-maybe-marked)

(define-key qda-view-text-units-mode-map "b" 'qda-backward-text-unit)
(define-key qda-view-text-units-mode-map "f" 'qda-forward-text-unit)
(define-key qda-view-text-units-mode-map "m" 'qda-mark-text-unit)
(define-key qda-view-text-units-mode-map "u" 'qda-unmark-text-unit)
(define-key qda-view-text-units-mode-map "M" 'qda-mark-all-text-units)
(define-key qda-view-text-units-mode-map "U" 'qda-unmark-all-text-units)
(define-key qda-view-text-units-mode-map  "s" 'qda-view-search-mark-regexp)
(define-key qda-view-text-units-mode-map "n" 'qda-doc-show-where-this-text-unit-is-indexed)

;; Mode for viewing indexing

(define-derived-mode qda-view-indexing-mode
  qda-view-text-units-mode "QDA-View-Indexing")

(define-key qda-view-indexing-mode-map "v" 'qda-view-this-doc)

;; Mode for viewing docs

(define-derived-mode qda-view-doc-mode qda-view-text-units-mode "QDA-View-Doc"
  (make-local-variable 'qda-view-doc-name)
  (make-local-variable 'qda-doc-node-indexing-by-text-unit-list))

(define-key qda-view-doc-mode-map "i" 'qda-view-this-doc-node-indexing)
(define-key qda-view-doc-mode-map "M" 'qda-doc-edit-memo)


(define-derived-mode qda-view-doc-node-indexing-mode qda-view-mode "QDA-View-Doc-Node-Indexing"
  (make-local-variable 'qda-view-doc-node-indexing-doc-name))

(define-key qda-view-doc-node-indexing-mode-map "v"
  'qda-view-doc-indexing-find-node-indexing)

;; Mode for viewing the tree

(define-derived-mode qda-view-tree-mode qda-view-mode "QDA-View-Tree"
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(qda-view-tree . t))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults nil)
  ;; list of numerical addresses of nodes in the tree
  (make-local-variable 'qda-node-list)
  (setq qda-node-list (qda-node-list)))



(define-key qda-view-tree-mode-map [left] 'undefined)
(define-key qda-view-tree-mode-map [right] 'undefined)
(define-key qda-view-tree-mode-map [up] 'qda-view-tree-previous-line)
(define-key qda-view-tree-mode-map [down] 'qda-view-tree-next-line)
(define-key qda-view-tree-mode-map "k" 'qda-view-tree-previous-line)
(define-key qda-view-tree-mode-map "j" 'qda-view-tree-next-line)
(define-key qda-view-tree-mode-map [prior] 'qda-view-tree-scroll-down)
(define-key qda-view-tree-mode-map [next] 'qda-view-tree-scroll-up)
(define-key qda-view-tree-mode-map "\C-p" 'qda-view-tree-previous-line)
(define-key qda-view-tree-mode-map "\C-n" 'qda-view-tree-next-line)
(define-key qda-view-tree-mode-map "\C-v" 'qda-view-tree-scroll-up)
(define-key qda-view-tree-mode-map "\ev" 'qda-view-tree-scroll-down)
(define-key qda-view-tree-mode-map "i" 'qda-view-this-node-info-toggle)
(define-key qda-view-tree-mode-map "v" 'qda-view-this-node-indexing)
(define-key qda-view-tree-mode-map "q" 'bury-buffer)
(define-key qda-view-tree-mode-map "a" 'qda-add-node)
(define-key qda-view-tree-mode-map "d" 'qda-delete-node)
(define-key qda-view-tree-mode-map "c" 'qda-copy-node)
(define-key qda-view-tree-mode-map "m" 'qda-move-node)
(define-key qda-view-tree-mode-map "M" 'qda-node-edit-memo)
(define-key qda-view-tree-mode-map "r" 'qda-rename-node)
(define-key qda-view-tree-mode-map "R" 'qda-redescribe-node)
(define-key qda-view-tree-mode-map "n" 'qda-renumber-node)
(define-key qda-view-tree-mode-map "#" 'qda-view-tree-toggle-mark-node)
(define-key qda-view-tree-mode-map "~" 'qda-view-tree-unmark-all)
(define-key qda-view-tree-mode-map " " 'qda-view-tree-toggle-hide-children)
(define-key qda-view-tree-mode-map "\C-m" 'qda-view-tree-toggle-hide-children)

(define-key qda-view-tree-mode-map "A" 'qda-view-tree-show-all)
(define-key qda-view-tree-mode-map "H" 'qda-view-tree-hide-all-children)

(easy-menu-define
 qda-view-tree-menu
 qda-view-tree-mode-map
 "QDA view tree menu"
 (list "Tree"
       ["Toggle show children" qda-view-tree-toggle-hide-children t]
;;;       ["Show children" qda-view-tree-show-children t]
;;;       ["Hide children" qda-view-tree-hide-children t]
       ["Show all" qda-view-tree-show-all t]
       ["Hide all children" qda-view-tree-hide-all-children]
       ["Update tree" qda-view-tree t]))
;;;    ["Hide all" hide-sublevels t]))

;; memo mode

(defvar qda-memo-mode-map nil)

(unless qda-memo-mode-map
  (setq qda-memo-mode-map (make-sparse-keymap))
  (define-key qda-memo-mode-map "\C-c\C-c" 'qda-memo-end-edit))

(defun qda-memo-mode ()
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'qda-memo-mode)
  (setq mode-name "QDA-Memo")
  (use-local-map qda-memo-mode-map)
  (make-local-variable 'memo-object)
  (setq memo-object nil)
  (make-local-variable 'qda-memo-text-start)
  (setq qda-memo-text-start nil)
  (run-hooks 'qda-memo-mode-hook))

(provide 'qda-modes)

;;; qda-modes.el ends here
