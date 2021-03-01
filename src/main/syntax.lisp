(in-package :cl-user)
(defpackage cl-annot.syntax
  (:nicknames :annot.syntax)
  (:use :cl
        :annot.core)
  (:export :annotation-syntax-reader
           :enable-annot-syntax))
(in-package :annot.syntax)

(defun read-annotation (stream)
  (let ((annot-sym (read stream t nil t)))
    (unless (symbolp annot-sym)
      (warn "unexpected annotation exp ~a" annot-sym))
    (values (find-annot annot-sym)
            annot-sym)))

(defun read-annotation-arguments (annot stream)
  (loop :repeat (annot-arity annot)
        :for x = (read stream t nil nil)
        :do (format t "debug: ~a~%" x)
        :collect x))

(defun annotation-syntax-reader (stream char)
  (declare (ignore char))
  ;; TODO: disallow space between @ and annot-sym
  (multiple-value-bind (annot sym)
      (read-annotation stream)
    (let ((args (read-annotation-arguments annot stream)))
      (expand-annot annot sym args))))

(defun %enable-annot-syntax ()
  (setf *readtable* (copy-readtable))
  (set-macro-character #\@ #'annotation-syntax-reader))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
