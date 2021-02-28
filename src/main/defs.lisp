
(defpackage cl-annot.defs
  (:nicknames :annot.defs)
  (:import-from :annot.core :define-annot)
  (:use :cl :annot.util))
(in-package :annot.defs)

(defun definition-symbol (exp) (cadr exp))
(defun exportable? (exp)
  (and (listp exp)
       (member (car exp) '(defun defmethod defgeneric defvar defparameter
                           defclass defstruct))))

(define-annot export (exp)
  "Export the definition symbol of DEFINITION-FORM."
  (if (exportable? exp)
      `(export ,exp)
      (error "bad export annot: ~a" exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; simple one-or-more variables
;;;;

(defun annot-simple-declare (exp identifier)
  (cond ((listp exp)   `(declare (,identifier ,@exp)))
        ((symbolp exp) `(declare (,identifier ,exp)))
        (t (error "bad ~a annot: ~a" identifier exp))))

(define-annot ignore (vars)
  "Shorthand for (DECLARE (IGNORE ...))."
  (annot-simple-declare vars 'ignore))

(define-annot ignorable (vars)
  "Shorthand for (DECLARE (IGNORABLE ...))."
  (annot-simple-declare vars 'ignorable))

(define-annot dynamic-extent (vars)
  "Shorthand for (DECLARE (DYNAMIC-EXTENT ...))."
  (annot-simple-declare vars 'dynamic-extent))

(define-annot declaration (vars)
  "Shorthand for (DECLARE (DECLARATION ...))."
  (annot-simple-declare vars 'declaration))

(define-annot special (vars)
  "Shorthand for (DECLARE (SPECIAL ...))."
  (annot-simple-declare vars 'special))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; others
;;;;

(defun defun? (exp) (and (listp exp)
                         (member (car exp) '(defun defmethod))))
(defun defun-name (exp) (cadr exp))

(defun list-of-syms? (exp) (and (listp exp) (every #'symbolp exp)))

(defun annot-type-declare (type typespec exp)
  (cond ((symbolp exp)  `(declare (,type ,typespec ,exp)))
        ((list-of-syms? exp) `(declare (type ,typespec ,@exp)))
        ((defun? exp) `(progn
                         (declaim (type ,typespec ',(defun-name exp)))
                         ,exp))
        (t (error "bad type annot: ~a" exp))))

(define-annot type (typespec exp)
  "Shorthand for (DECLARE (TYPE ...))."
  (annot-type-declare 'type typespec exp))

(define-annot ftype (typespec exp)
  "Shorthand for (DECLARE (FTYPE ...))."
  (annot-type-declare 'ftype typespec exp))

(define-annot optimize (spec)
  "Shorthand for (DECLARE (OPTIMIZE ...))."
  `(declare (optimize ,spec)))

;;;

(defun annot-inline-declare (inline exp)
  (cond ((symbol? exp)  `(declare (,inline ,exp)))
        ((list-of-syms? exp) `(declare (,inline ,@exp)))
        ((defun? exp) `(progn
                         (declaim (,inline ',(defun-name exp)))
                         ,exp))
        (t (error "bad ~a annot: ~a" inline exp))))

(define-annot inline (exp)
  "Shorthand for (DECLARE (INLINE ...))."
  (annot-inline-declare 'inline exp))

(define-annot notinline (name)
  "Shorthand for (DECLARE (NOTINLINE ...))."
  (annot-inline-declare 'notinline exp))

