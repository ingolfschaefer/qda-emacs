;;; qda-misc.el --- qda miscellany

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
;;; Code:

(defvar qda-temp-buffer " *qda-temp-buffer*")

(defun null-string-p (str)
  "Return t if STR is the null string, else nil."
  (string= str ""))


(defun sing-or-plural-noun (sing-string plural-string n)
  "If N is 1, return SING-STRING, else PLURAL-STRING."
  (if (= n 1)
      sing-string
    plural-string))

(defun qda-read-number (prompt &optional initial)
  ;; read a number from the minibuffer
  ;; insists on valid input.
  (condition-case err
      (let (num)
	(setq num (read-minibuffer prompt (concat initial)))
	(unless (numberp num) (error err))
	num)
    (error
     (progn
       (message "Must be a number")
       (sit-for 1)
       (qda-read-number prompt initial)))))

(defun qda-read-number-list (prompt &optional initial allow-null)
  ;; read a number list from the minibuffer
  ;; insisting on valid input. Numbers are entered
  ;; like this: 1 2 3 4 (i.e. no brackets) and
  ;; turned into a list. Non-nil ALLOW-NULL lets user
  ;; enter nothing.
  (condition-case err
      (let ((n-list (read-from-minibuffer prompt initial)))
	(cond ((and allow-null
		    (null-string-p n-list)) nil)
	      ((not (string-match "^[0-9 ]+$" n-list))
	       (error err))
	      (t (read (format "(%s)" n-list)))))
    (error
     (progn
       (message "Must be list of numbers")
       (sit-for 1)
       (qda-read-number-list prompt initial)))))
	       
(defun qda-read-number-or-range-list (prompt &optional initial allow-null)
  (condition-case err
      (let ((n-list (read-from-minibuffer prompt initial)))
	(cond ((and allow-null
		    (null-string-p n-list)) nil)
	      ((not (string-match "^[0-9 ().]+$" n-list))
	       (error err))
	      (t (read (format "(%s)" n-list)))))
    (error
     (progn
       (message "Must be list of numbers")
       (sit-for 1)
       (qda-read-number-list prompt initial)))))

(defun qda-read-string (prompt &optional initial allow-null)
  ;; read a string from the minibuffer
  ;; insists on valid input
  ;; if allow null, let user enter nothing and return nil
  (condition-case err
      (let ((string
	     (read-from-minibuffer prompt initial)))
	(if (null-string-p string)
	    (if allow-null
		nil
	      (error err))
	  string))
    (error
     (progn
       (message "Must be a string")
       (sit-for 1)
       (qda-read-string prompt initial)))))

;; macro for messages (a macro so that the test
;; interactive-p works)
;; ...this could be developed into a
;; logging function: put all messages into a log
;; and also into the minibuffer if they are called
;; interactively.

(defmacro qda-message (msg &rest args)
  `(when (interactive-p)
     (message ,msg ,@ args)))

;; ...likewise with qda-error

(defmacro qda-error (msg &rest args)
  `(error ,msg ,@ args))

;; primitive saving and loading functions
;; these could be modified to get/save more than one object
;; if necessary

(defun qda-save-object (object file)
  (unless (file-writable-p file)
    (qda-error "Can't write to file %s" file))
  (let ((buf (set-buffer (generate-new-buffer qda-temp-buffer))))
      (prin1 object buf)
      (write-region (point-min) (point-max) file)
      (kill-buffer buf)))

(defun qda-get-object (file)
  ;; Returns object from file
  (unless (file-readable-p file)
    (qda-error "Cannot read file %s" file))
  (let ((buf (set-buffer (generate-new-buffer qda-temp-buffer))))
    (insert-file-contents file nil nil nil t)
    (prog2
	(goto-char (point-min))
	(read buf)
      (kill-buffer buf))))

(defvar qda-line-string
  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  "The line used in headers, footers, etc.")


;;; method for adding code to defuns for things like undo, marking
;;; changed nodes etc.


;;; todo: add option for evaluating after forms even if function fails
;;; (unwind-protect...)

(defmacro qda-defun-real (name args before-func after-func starred-p &rest forms)
  "Define a function NAME with ARGS.

If non-nil, BEFORE-FUNC will be added to the beginning the defun.

If non-nil, AFTER-FUNC will be added to the end of the defun, preserving the
return value of FORMS.

Non-nil STARRED-P means use `defun*' instead of `defun'."
  ;; shuffle documentation and (interactive ...) so that they come
  ;; before the (prog1 ...)
  (let ((docstring (if (stringp (car forms)) (pop forms) nil))
	(int (if (eq (caar forms) 'interactive) (pop forms) nil))
	(defun-name (if starred-p 'defun* 'defun))
	(ret-form (if after-func
		      `((prog1
			    (progn ,@forms)
			  (funcall ,after-func)))
		    `(,@forms))))
    (when before-func (push `(funcall ,before-func) `,ret-form))
    (when int (push `,int `,ret-form))
    (when docstring (push `,docstring `,ret-form))
    (append (list `,defun-name `,name `,args) `,ret-form)))

(defmacro qda-defun (name args before-func after-func &rest forms)
  "Define a function NAME with ARGS.

If non-nil, BEFORE-FUNC will be added to the beginning the defun.

If non-nil, FUNC will be added to the end of the defun, preserving the
return value of FORMS.

If you need to use keyword arguments, use `qda-defun*'."
  `(qda-defun-real ,name ,args ,before-func ,after-func  nil ,@forms))

(defmacro qda-defun* (name args before-func after-func &rest forms)
    "Define a function NAME with ARGS.

If non-nil, BEFORE-FUNC will be added to the beginning the defun.

If non-nil, AFTER-FUNC will be added to the end of the defun, preserving the
return value of FORMS.

This function uses `defun*' for its extended arguments, use
`qda-defun' if you don't need this."
    `(qda-defun-real ,name ,args ,before-func ,after-func t ,@forms))

(provide 'qda-misc)

;;; qda-misc.el ends here
