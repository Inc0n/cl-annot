
(defpackage cl-annot.core
  (:nicknames :annot.core)
  (:use :cl)
  (:export :make-annot :annot-name
           :annot-arity :annot-expander
           :find-annot :expand-annot))
(in-package :annot.core)

(defstruct annot
  (name     nil :type symbol :read-only t)
  (arity    nil :type fixnum :read-only t)
  (expander nil :type function :read-only t))

(defun add-annot! (annot)
  (setf (get 'annotations (annot-name annot)) annot))

(defmacro define-annot (name params &body body)
  `(add-annot!
    (make-annot :name ',name
                :arity ,(length params)
                :expander (lambda ,params ,@body))))

(defun find-annot (annot-sym)
  (or (get 'annotations annot-sym)
      (get 'annotations t)
      (error "cannot find annotation named ~a" annot-sym)))

(defun expand-annot (annot annot-sym args)
  (if (eq (annot-name annot) t)
      (funcall (annot-expander annot) annot-sym args)
      (apply (annot-expander annot) args)))

;; default annotation
(add-annot!
 (make-annot :name t
             :arity 1
             :expander (lambda (self exp) `(,self ,@exp))))


;; (defun annotation-form (annot args)
;;   "Make an annotation-form with ANNOT and ARGS."
;;   `(%annotation ,annot ,@args))

;; (defun annotation-form-p (form)
;;   "Return non-nil if FORM is an annotation-form."
;;   (and (consp form)
;;        (consp (cdr form))
;;        (eq (car form) '%annotation)))
