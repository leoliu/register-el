;;; register.el --- register commands for Emacs

;; Copyright (C) 1985, 1993-1994, 2001-2011 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package of functions emulates and somewhat extends the venerable
;; TECO's `register' feature, which permits you to save various useful
;; pieces of buffer state to named variables.  The entry points are
;; documented in the Emacs user's manual.

;;; Global key bindings

(define-key ctl-x-r-map "\C-@" 'point-to-register)
(define-key ctl-x-r-map [?\C-\ ] 'point-to-register)
(define-key ctl-x-r-map " " 'point-to-register)
(define-key ctl-x-r-map "j" 'jump-to-register)
(define-key ctl-x-r-map "s" 'copy-to-register)
(define-key ctl-x-r-map "x" 'copy-to-register)
(define-key ctl-x-r-map "i" 'insert-register)
(define-key ctl-x-r-map "g" 'insert-register)
(define-key ctl-x-r-map "r" 'copy-rectangle-to-register)
(define-key ctl-x-r-map "n" 'number-to-register)
(define-key ctl-x-r-map "+" 'increment-register)
(define-key ctl-x-r-map "w" 'window-configuration-to-register)
(define-key ctl-x-r-map "f" 'frame-configuration-to-register)

;;; Code:

(eval-when-compile (require 'cl))

;;; In-memory persistency.
(defvar register-hash-table (make-hash-table))

;;; Immutable register object.
(defstruct
  (register (:constructor nil)
	    (:constructor register--make (name &optional value print-func
					       jump-func insert-func))
	    (:copier nil)
	    (:type list)
	    :named)
  (name	       nil :read-only t)
  (value       nil :read-only t)
  (print-func  nil :read-only t)
  (jump-func   nil :read-only t)
  (insert-func nil :read-only t))

(defun* register-make (name value &key print-func jump-func insert-func)
  "Return a newly created register with NAME and VALUE.
VALUE may be a string, number, marker or list.
A list of strings represents a rectangle.
A list of the form (file . FILE-NAME) represents the file named FILE-NAME.
A list of the form (file-query FILE-NAME POSITION) represents
 position POSITION in the file named FILE-NAME, but query before
 visiting it.
A list of the form (WINDOW-CONFIGURATION POSITION)
 represents a saved window configuration plus a saved value of point.
A list of the form (FRAME-CONFIGURATION POSITION)
 represents a saved frame configuration plus a saved value of point.

VALUE may also be any value.

PRINT-FUNC if provided controls how `list-registers' and
`view-register' print the register.  It should be a function
recieving one argument VALUE and print text that completes
this sentence:
  Register X contains [TEXT PRINTED BY PRINT-FUNC]

JUMP-FUNC if provided, controls how `jump-to-register' jumps to the register.
INSERT-FUNC if provided, controls how `insert-register' insert the register.
They both receive the VALUE of the register as argument."
  (let ((register (register--make name value print-func
				  jump-func insert-func)))
    (puthash name register register-hash-table)
    register))

(defun register-find (name &optional if-does-not-exist)
  "Find the register named NAME and return it.
If IF-DOES-NOT-EXIST is :error, signal an error; otherwise return nil."
  (let ((register (gethash name register-hash-table)))
    (or register (case if-does-not-exist
		   (:error (error "Register named `%s' does not exist"
				  (single-key-description name)))
		   (otherwise nil)))))

(defun register-map (function)
  "Apply FUNCTION to each register for side effects only.
FUNCTION should accept one argument - the register."
  (maphash (lambda (name register)
	     (funcall function register)) register-hash-table))

(define-obsolete-function-alias 'set-register 'register-make "24.1")
(make-obsolete 'get-register "\
use `register-find' and `register-value' instead." "24.1")

(defun get-register (name)
  "Return the value of register named NAME or nil if none."
  (ignore-errors (register-value (register-find name :error))))

(defun point-to-register (name &optional arg)
  "Store current location of point in a register.
With prefix argument, store current frame configuration.
Use \\[jump-to-register] to go to that location or restore that configuration.
Argument is a character, naming the register."
  (interactive "cPoint to register: \nP")
  ;; Turn the marker into a file-ref if the buffer is killed.
  (add-hook 'kill-buffer-hook 'register-swap-out nil t)
  (register-make name
		 (if arg (list (current-frame-configuration) (point-marker))
		   (point-marker))))

(defun window-configuration-to-register (name &optional arg)
  "Store the window configuration of the selected frame in a register.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register."
  (interactive "cWindow configuration to register: \nP")
  ;; current-window-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (register-make name (list (current-window-configuration) (point-marker))))

(defun frame-configuration-to-register (name &optional arg)
  "Store the window configuration of all frames in a register.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register."
  (interactive "cFrame configuration to register: \nP")
  ;; current-frame-configuration does not include the value
  ;; of point in the current buffer, so record that separately.
  (register-make name (list (current-frame-configuration) (point-marker))))

(defalias 'register-to-point 'jump-to-register)
(defun jump-to-register (name &optional delete)
  "Move point to location stored in a register.
If the register contains a file name, find that file.
\(To put a file name in a register, you must use `set-register'.)
If the register contains a window configuration (one frame) or a frame
configuration (all frames), restore that frame or all frames accordingly.
First argument is a character, naming the register.
Optional second arg non-nil (interactively, prefix argument) says to
delete any existing frames that the frame configuration doesn't mention.
\(Otherwise, these frames are iconified.)"
  (interactive "cJump to register: \nP")
  (let* ((register (register-find name :error))
	 (val (register-value register))
	 (jump-func (register-jump-func register)))
    (cond
     (jump-func (funcall jump-func val))
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not delete))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
	  (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
	  (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
	  (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     (t
      (error "Register doesn't contain a buffer position or configuration")))))

(defun register-swap-out ()
  "Turn markers into file-query references when a buffer is killed."
  (and buffer-file-name
       (register-map
	(lambda (register)
	  (let ((val (register-value register)))
	    (and (markerp val)
		 (eq (marker-buffer val) (current-buffer))
		 (register-make (register-name register)
				(list 'file-query
				      buffer-file-name
				      (marker-position val)))))))))

(defun number-to-register (number name)
  "Store a number in a register.
Two args, NUMBER and NAME (a character, naming the register).
If NUMBER is nil, a decimal number is read from the buffer starting
at point, and point moves to the end of that number.
Interactively, NUMBER is the prefix arg (none means nil)."
  (interactive "P\ncNumber to register: ")
  (register-make name
		 (if number
		     (prefix-numeric-value number)
		   (if (looking-at "\\s-*-?[0-9]+")
		       (progn
			 (goto-char (match-end 0))
			 (string-to-number (match-string 0)))
		     0))))

(defun increment-register (number name)
  "Add NUMBER to the value of the register named NAME.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncIncrement register: ")
  (let ((register (register-find name :error)))
    (or (numberp (register-value register))
	(error "Register does not contain a number"))
    (register-make name (+ number (register-value register)))))

(defun view-register (name)
  "Display what is contained in register named NAME."
  (interactive "cView register: ")
  (let* ((register (register-find name :error))
	 (val (register-value register)))
    (with-output-to-temp-buffer "*Output*"
      (describe-register-1 register t))))

(defun list-registers ()
  "Display a list of nonempty registers saying briefly what they contain."
  (interactive)
  (let (names register)
    (register-map (lambda (r) (push (register-name r) names)))
    (setq names (sort names '<))
    (with-output-to-temp-buffer "*Output*"
      (dolist (name names)
	(setq register (register-find name))
	(when (and register (register-value register))
	  (describe-register-1 register)
	  (terpri))))))

(defun describe-register-1 (register &optional verbose)
  (princ "Register ")
  (princ (single-key-description (register-name register)))
  (princ " contains ")
  (let ((val (register-value register))
	(print-func (register-print-func register)))
    (cond
     (print-func (funcall print-func val))

     ((numberp val)
      (princ val))

     ((markerp val)
      (let ((buf (marker-buffer val)))
	(if (null buf)
	    (princ "a marker in no buffer")
	  (princ "a buffer position:\n    buffer ")
	  (princ (buffer-name buf))
	  (princ ", position ")
	  (princ (marker-position val)))))

     ((and (consp val) (window-configuration-p (car val)))
      (princ "a window configuration."))

     ((and (consp val) (frame-configuration-p (car val)))
      (princ "a frame configuration."))

     ((and (consp val) (eq (car val) 'file))
      (princ "the file ")
      (prin1 (cdr val))
      (princ "."))

     ((and (consp val) (eq (car val) 'file-query))
      (princ "a file-query reference:\n    file ")
      (prin1 (car (cdr val)))
      (princ ",\n    position ")
      (princ (car (cdr (cdr val))))
      (princ "."))

     ((consp val)
      (if verbose
	  (progn
	    (princ "the rectangle:\n")
	    (while val
	      (princ "    ")
	      (princ (car val))
	      (terpri)
	      (setq val (cdr val))))
	(princ "a rectangle starting with ")
	(princ (car val))))

     ((stringp val)
      (if (eq yank-excluded-properties t)
	  (set-text-properties 0 (length val) nil val)
	(remove-list-of-text-properties 0 (length val)
					yank-excluded-properties val))
      (if verbose
	  (progn
	    (princ "the text:\n")
	    (princ val))
	(cond
	 ;; Extract first N characters starting with first non-whitespace.
	 ((string-match (format "[^ \t\n].\\{,%d\\}"
				;; Deduct 6 for the spaces inserted below.
				(min 20 (max 0 (- (window-width) 6))))
			val)
	  (princ "text starting with\n    ")
	  (princ (match-string 0 val)))
	 ((string-match "^[ \t\n]+$" val)
	  (princ "whitespace"))
	 (t
	  (princ "the empty string")))))
     (t
      (princ "Garbage:\n")
      (if verbose (prin1 val))))))

(defun insert-register (name &optional arg)
  "Insert the value of the register named NAME.
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is non-nil if prefix arg is supplied."
  (interactive "*cInsert register: \nP")
  (let* ((register (register-find name :error))
	 (val (register-value register))
	 (insert-func (register-insert-func register)))
    (push-mark)
    (cond
     (insert-func (funcall insert-func val))
     ((stringp val)
      (insert-for-yank val))
     ((numberp val)
      (princ val (current-buffer)))
     ((and (markerp val) (marker-position val))
      (princ (marker-position val) (current-buffer)))
     (t
      (error "Register does not contain text"))))
  (if (not arg) (exchange-point-and-mark)))

(defun copy-to-register (name start end &optional delete-flag)
  "Copy region into register named NAME.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to copy."
  (interactive "cCopy to register: \nr\nP")
  (register-make name (filter-buffer-substring start end))
  (if delete-flag (delete-region start end)))

(defun append-to-register (name start end &optional delete-flag)
  "Append region to text in register named NAME.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (let* ((register (register-find name))
	 (val (and register (register-value register)))
	 (text (filter-buffer-substring start end)))
    (assert (string-or-null-p val) nil "Register does not contain text")
    (register-make name (concat val text)))
  (if delete-flag (delete-region start end)))

(defun prepend-to-register (name start end &optional delete-flag)
  "Prepend region to text in register named NAME.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend."
  (interactive "cPrepend to register: \nr\nP")
  (let* ((register (register-find name))
	 (val (and register (register-value register)))
	 (text (filter-buffer-substring start end)))
    (assert (string-or-null-p val) nil "Register does not contain text")
    (register-make name (concat text val)))
  (if delete-flag (delete-region start end)))

(defun copy-rectangle-to-register (name start end &optional delete-flag)
  "Copy rectangular region into register named NAME.
With prefix arg, delete as well.
To insert this register in the buffer, use \\[insert-register].

Called from a program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle."
  (interactive "cCopy rectangle to register: \nr\nP")
  (register-make name
		 (if delete-flag
		     (delete-extract-rectangle start end)
		   (extract-rectangle start end))
		 :insert-func #'insert-rectangle))

(provide 'register)
;;; register.el ends here
