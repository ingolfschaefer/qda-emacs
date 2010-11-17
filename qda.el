;;; qda.el --- qda project setup

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
;; This is the main file for the qda program
;; It pulls in all of the required files
;; and sets up all of the project-specific functions.
;;
;;; History:
;; 



(eval-when  (load)
  (require 'qda-modes)
  (require 'qda-misc)
  (require 'qda-tree)
  (require 'qda-view-tree)
  (require 'qda-docs)
  (require 'qda-indexing)
  (require 'qda-memos))

(eval-when-compile
  (require 'cl))

;;; Code:

(defgroup qda nil
  "Qualitative Data Analysis."
  :group 'applications)

(defcustom qda-project-directory
  "~/qda"
  "*The name of the base directory where all qda projects are kept."
  :group 'qda
  :type 'string)

(defcustom qda-auto-save-file-prefix
  "#"
  "*String to add to auto-saved bits of the project."
  :group 'qda
  :type 'string)

(defcustom qda-auto-save-period
  300
  "*Time in seconds between auto-saves."
  :group 'qda
  :type 'integer)

(defvar qda-project-name nil
  "The name of the current qda-project.")

; (defvar qda-project-directory "~/qda/"
;   "The name of the base directory where all qda projects are kept.")

; (defvar qda-project-name nil
;   "The name of the current qda-project.")

; (defvar qda-auto-save-file-prefix "#"
;   "String to add to auto-saved bits of the project.")

; (defvar qda-auto-save-period 300
;   "Time in seconds between auto-saves.")

(defun qda-project-make-directory-name (name)
  "Return a full pathname for the project directory NAME."
  (expand-file-name (concat qda-project-directory "/" name "/")))

(defun qda-project-make-file-name (name)
  "Return the expanded file name of NAME in the current project directory."
  (concat (qda-project-make-directory-name qda-project-name) name))

;; Structure for all the top level data structures.
;; This is intended to make it easy to add features
;; to the program.

(defstruct qda-project-obj
  name					; basename for files
  init-func				; intialisation (takes 0 args)
  load-func				; load (1 arg)
  save-func				; save (1 arg)
  exit-func				; function to call when exiting emacs (0 arg)
  (necessary-p nil))			; do we need it for the project to run?

;; These are the ones currently defined
(defvar qda-project-objects
  (list
   (make-qda-project-obj
    :name "docs"
    :init-func 'qda-doc-init
    :load-func 'qda-load-doc-list
    :save-func 'qda-save-doc-list
    :exit-func nil
    :necessary-p nil)
   (make-qda-project-obj
    :name "tree"
    :init-func 'qda-make-root
    :load-func 'qda-load-tree
    :save-func 'qda-save-tree
    :exit-func nil
    :necessary-p t)
   (make-qda-project-obj
    :name "memos"
    :init-func nil
    :load-func nil
    :save-func nil
    :exit-func 'qda-memo-offer-save
    :necessary-p nil)))
   
;;; kill-emacs hooks
(defun qda-project-add-exit-hook (func)
  "Add FUNC to `kill-emacs-query-functions'."
  (when (and func
	     (not (memq func kill-emacs-query-functions)))
    (add-hook 'kill-emacs-query-functions func)))

(defun qda-project-add-exit-hooks ()
  "Add exit functions from `qda-project-objects' to `kill-emacs-query-functions'."
  (let (exit-func)
  (dolist (obj qda-project-objects)
    (setq exit-func (qda-project-obj-exit-func obj))
    (qda-project-add-exit-hook exit-func))))

(or (memq 'qda-query-save-on-exit kill-emacs-query-functions)
    (add-hook 'kill-emacs-query-functions 'qda-query-save-on-exit))
(or (memq 'qda-cleanup-auto-save kill-emacs-query-functions)
    (add-hook 'kill-emacs-query-functions 'qda-cleanup-auto-save))

(qda-project-add-exit-hooks)

;;; add a new object to the project

(defun qda-project-add-obj (project-obj)
  "Add PROJECT-OBJ to the list of project objects.
PROJECT-OBJ is a structure of type `qda-project-obj'."
  (unless (qda-project-obj-p project-obj)
    (error "Not a qda-project-obj"))
  (push project-obj qda-project-objects)
  (qda-project-add-exit-hook (qda-project-obj-exit-func project-obj)))

;;; creating, saving, loading...

(defun qda-create-project (name)
  "Create project called NAME."
  (interactive "sName: ")
  (when (and qda-project-name
	     (yes-or-no-p "Save current project? "))
    (qda-save-project)
    (qda-message "Project %s saved." qda-project-name)
    (sit-for 1))
  (cond ((file-directory-p (qda-project-make-directory-name name))
	 (qda-error "The name %s is already in use for a project" name))
	((not (file-writable-p qda-project-directory))
	 (qda-error "You don't have permission to write to %s"
		    qda-project-directory))
	(t (make-directory (qda-project-make-directory-name name) t)
	   (setq qda-project-name name)
	   (dolist (obj qda-project-objects)
	     (when (qda-project-obj-init-func obj)
	       (funcall (qda-project-obj-init-func obj))))
	   (qda-message "Project %s created." name))))

(defun qda-load-project (name)
  "Load project NAME."
  (interactive
   (list 
    (completing-read
     "Project: "
     (loop
      for project in
      (directory-files qda-project-directory t "[^.][^.]")
      if (file-directory-p project)
      collect (list (file-name-nondirectory project)) into projects
      finally return projects))))
  (if (string= qda-project-name name)
      (qda-message "Project %s already loaded" name)     
    (if (not (file-directory-p (qda-project-make-directory-name name)))
	(when (y-or-n-p (format "Create new project with name %s? " name))
	  (qda-create-project name))
      (when (and qda-project-name
		 (yes-or-no-p "Save current project? "))
	(qda-save-project)
	(qda-message "Project %s saved." qda-project-name)
	(sit-for 1))
      (setq qda-project-name name)
      (let (f)
	(dolist (obj qda-project-objects)
	  (setq f (qda-project-make-file-name (qda-project-obj-name obj)))
	  (and (qda-project-obj-necessary-p obj)
	       (not (file-readable-p f))
	       (qda-error "Can't load %s" f))
	  (when (qda-project-obj-load-func obj)
	    (funcall (qda-project-obj-load-func obj) f))))
      (qda-message "Project %s loaded." name)
      (when qda-view-tree-centric-p
	(qda-view-tree)			;this really shouldn't be
					;here.  There should be a hook
					;to start up something like
					;this.
	(qda-view-tree-hide-all-children)))))
  
(defun qda-save-project (&optional query)
  "Save current project.
Non-nil optional QUERY means ask first."
  (interactive)
  (when (or (not query)
	    (and qda-project-name
		 query
		 (yes-or-no-p "Save qda project before exiting? ")))
    (unless qda-project-name
      (qda-error "No project loaded"))
    (let (f)
      (dolist (obj qda-project-objects)
	(setq f (qda-project-make-file-name (qda-project-obj-name obj)))
	(and (qda-project-obj-necessary-p obj)
	     (not (file-writable-p f))
	     (qda-error "Can't save %s" f))
	(when (qda-project-obj-save-func obj)
	  (funcall (qda-project-obj-save-func obj) f))))
    (qda-cleanup-auto-save)
    (qda-message "Project %s saved." qda-project-name))
  t)

(defun qda-query-save-on-exit ()
  "Save project, ask first."
  (qda-save-project t))

;;; auto saving

;;  this is very rough, I should add stuff that
;;  deletes auto save files when a normal save is done
;;  and some check on loading whether any extant
;;  auto save files are newer.
;;  Sun Sep 10 10:16:52 2000
;;  cleanup of autosave files done.

(defun qda-project-make-auto-save-file-name (file)
  "Return auto save file name for FILE using `qda-auto-save-file-prefix'."
  (qda-project-make-file-name
   (concat qda-auto-save-file-prefix file)))

(defun qda-auto-save-project ()
  "Auto save current project."
;;;  (message "Auto saving qda project...")
;;;  (sit-for 1)
 (let (f)
   (dolist (obj qda-project-objects)
     (setq f (qda-project-make-auto-save-file-name
	      (qda-project-obj-name obj)))
     (and (qda-project-obj-necessary-p obj)
	  (not (file-writable-p f))
	  (qda-error "Can't save %s" f))
     (when (qda-project-obj-save-func obj)
       (funcall (qda-project-obj-save-func obj) f))))
 (message nil))
;;; (message "Auto saving qda project...done"))

(defvar qda-auto-save-timer
  (run-at-time t qda-auto-save-period 'qda-auto-save-project)
  "Timer for autosaving, period between saves is supplied by `qda-auto-save-project'.")


;;; Sun Sep 10 10:16:52 2000

(defun qda-cleanup-auto-save ()
  (mapc 'delete-file (directory-files
		      (qda-project-make-directory-name qda-project-name)
		      t
		      (concat qda-auto-save-file-prefix ".*"))) t)
			       
		      
(provide 'qda)

;;; qda.el ends here
