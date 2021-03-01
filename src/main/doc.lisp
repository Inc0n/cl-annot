
(defpackage cl-annot.doc
  (:nicknames :annot.doc)
  (:use :cl :annot.util)
  (:import-from :annot.core :define-annot))
(in-package :annot.doc)

(define-annot doc (docstring definition-form)
  "Add DOCSTRING documentation for DEFINITION-FORM."
  ;; (progn-form-replace-last
  ;;  (lambda (definition-form))
  ;;  definition-form)
  (case (definition-type definition-form)
    ((defvar defparameter defconstant defun defmethod defmacro
			 deftype define-compiler-macro)
     (destructuring-bind (def name arg . body)
         definition-form
       `(,def ,name ,arg ,docstring ,@body)))
	((defstruct)
	 (destructuring-bind (def name-and-options . slots)
         definition-form
       `(,def ,name-and-options ,docstring ,@slots)))
	((defclass define-condition)
     (destructuring-bind (def name supers slots . opts)
         definition-form
	   (pushnew `(:documentation ,docstring)
				opts :key #'car)
       `(,def ,name ,supers ,slots
		  ,@opts)))
	((defgeneric)
     (destructuring-bind (def name args . opts)
         definition-form
	   (pushnew `(:documentation ,docstring)
				opts :key #'car)
	   `(,def ,name ,args
		  ,@opts)))
    (t (error "Documentation not supported: ~a"
              definition-form))))

;; todo:
;; define-compiler-macro 	 define-condition
;; define-method-combination define-modify-macro
;; define-setf-expander 	 define-symbol-macro
