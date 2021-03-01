
(defpackage cl-annot.core
  (:nicknames :annot.core)
  (:use :cl)
  (:export :make-annot :annot-name
           :annot-arity :annot-expander
           :make-annotated :find-annot :expand-annot)
  (:import-from :alexandria :if-let))
(in-package :annot.core)

;;; utils

(defun sym->keyword (sym)
  (and (symbolp sym)
       (intern (string sym) 'keyword)))

;;;

(defstruct annot
  (name     nil :type symbol :read-only t)
  (arity    nil :type fixnum :read-only t)
  (expander nil :type function :read-only t))

(defun add-annot! (annot)
  (setf (get 'annotations (sym->keyword (annot-name annot)))
        annot))

(defvar *annot-default-sym* :t)

(defun find-annot (annot-sym)
  (or (get 'annotations (sym->keyword annot-sym))
      (get 'annotations *annot-default-sym*)
      (error "cannot find annotation named ~a" annot-sym)))

(defmacro define-annot (name params &body body)
  `(add-annot!
    (make-annot :name ',name
                :arity ,(length params)
                :expander (lambda ,params ,@body))))

(add-annot! ;; default annotation
 (make-annot :name *annot-default-sym*
             :arity 1
             :expander (lambda (self exp) `(,self ,@exp))))


;;; annotated -- used to aid in manipulation of original exp between annotations

(defmacro annotated (&body exps)
  `(progn ,@exps))

(defun tagged? (tag exp) (and (listp exp) (eq tag (car exp))))
(defun exp-annotated? (exp) (tagged? 'annotated exp))
(defun make-annotated (annotation exp) `(annotated ,annotation ,exp))
(defun annotated-annotation (exp)
  (if (exp-annotated? exp)
      (second exp)
      nil))
(defun annotated-exp (exp)
  (if (exp-annotated? exp)
      (third exp)
      exp))

;;;

(defun expand-annot (annot annot-sym args)
  (let ((exps (mapcar #'annotated-exp args))
        (annotations
          (mapcar #'annotated-annotation
                  (remove-if-not #'exp-annotated? args))))
    (join-annotations
     (if (eq (annot-name annot) *annot-default-sym*)
         (funcall (annot-expander annot) annot-sym exps)
         (apply (annot-expander annot) exps))
     annotations)))

(defun single? (exp) (and (listp exp) (null (cdr exp))))

(defun progn-exps (exp)
  (if (tagged? 'progn exp1)
      (cdr exp1)
      exp))
(defun progn-join (exp1 exp2)
  `(progn ,@(progn-exps exp) ,@(progn-exps exp2)))

(defun join-annotations (annotated-exp annotations)
  ;;
  (format t "join ~a ~a~%" annotated-exp annotations)
  (if (null annotations)
      annotated-exp
      (make-annotated
       (if (null (annotated-annotation annotated-exp))
           `(progn ,@annotations)
           `(progn-join ,(annotated-annotation annotated-exp) ,@annotations))
       (annotated-exp annotated-exp))))


;; (defun annotation-form (annot args)
;;   "Make an annotation-form with ANNOT and ARGS."
;;   `(%annotation ,annot ,@args))

;; (defun annotation-form-p (form)
;;   "Return non-nil if FORM is an annotation-form."
;;   (and (consp form)
;;        (consp (cdr form))
;;        (eq (car form) '%annotation)))

;; Test
;; '@export @doc "1237" (defun x () 1)