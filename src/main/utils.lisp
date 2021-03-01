(in-package :cl-user)
(defpackage cl-annot.util
  (:nicknames :annot.util)
  (:use :cl)
  (:export ;; General
           :plist-member
           :plist-get-all
           ;; Macros
           :macrop
           :macroexpand-some
           :macroexpand-until-normal-form
           ;; Progns
           :progn-form-last
           :progn-form-replace-last
           ;; Definitions
           :definition-symbol
           :definition-type
           ;; Functions
           :replace-function-body
           ;; Classes
           :slot-specifiers
           :replace-slot-specifiers
           :class-options
           :get-class-option))
(in-package :annot.util)

(defun plist-member (plist prop)
  "Return t if PLIST contains PROP as a property."
  (let ((undef '#:undef))
    (not (eq (getf plist prop undef) undef))))

(defun plist-get-all (plist prop)
  "Return all values in PLIST named PROP."
  (loop :for (name value) :on plist :by #'cddr
        :when (string= prop name)
          :collect value))

(defun macrop (symbol)
  "Return non-nil if SYMBOL is a macro."
  (and (symbolp symbol)
       (macro-function symbol)))

(defun macroexpand-some (form)
  "Expand FORM once. The result form won't be nil."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p) (null new-form))
        (values form nil)
        (values new-form expanded-p))))

(defun macroexpand-until-normal-form (form)
  "Expand FORM until it brecomes normal-form."
  (if (and (consp form)
           (macrop (car form))
           (let ((package (symbol-package (car form))))
             (and package
                  (member package
                          (list (find-package :cl)
                                #+clisp (find-package :clos))))))
      (values form nil)
      (multiple-value-bind (new-form expanded-p)
          (macroexpand-1 form)
        (if (or (not expanded-p) (null new-form))
            (values form nil)
            (values (macroexpand-until-normal-form new-form) t)))))

(defun progn-form-last (progn-form)
  "Return the last form of PROGN-FORM which should be evaluated at
last. If macro forms seen, the macro forms will be expanded using
MACROEXPAND-UNTIL-NORMAL-FORM."
  (let ((progn-form (macroexpand-until-normal-form progn-form)))
    (if (and (consp progn-form)
             (eq (car progn-form) 'progn))
        (progn-form-last (car (last progn-form)))
        progn-form)))

(defun progn-form-replace-last (last progn-form)
  "Replace the last form of PROGN-FORM with LAST. If LAST is a
function, the function will be called with the last form and used for
replacing. If macro forms seen, the macro forms will be expanded using
MACROEXPAND-UNTIL-NORMAL-FORM."
  (if (and (consp progn-form)
           (eq (car progn-form) 'progn))
      `(,@(butlast progn-form)
        ,(progn-form-replace-last last (car (last progn-form))))
      (if (functionp last)
          (funcall last progn-form)
          last)))


(defun definition-symbol (exp)
  "Return the symbol of DEFINITION-FORM."
  (and (listp exp)
       (consp (last exp))
       (cadr exp)))

(defun definition-type (exp)
  (and (listp exp)
       (consp (last exp))
       (car exp)))

(defun replace-function-body (function function-definition-form)
  "Replace the body of FUNCTION-DEFINITION-FORM by calling FUNCTION
with name, lambda-list and the body as arguments."
  (progn-form-replace-last
   (lambda (function-definition-form)
     (destructuring-bind (type name lambda-list . body)
         function-definition-form
       (let (header)
         (when (and (stringp (car body))
                    (cdr body))
           (setf header (list (car body))
                 body (cdr body)))
         `(,type ,name ,lambda-list
                 ,@header
                 ,(funcall function name lambda-list body)))))
   function-definition-form))


(defun replace-slot-specifiers (function class-definition-form)
  "Replace slot-specifiers of CLASS-DEFINITION-FORM with FUNCTION. The
result value will be a class definition form also."
  (progn-form-replace-last
   (lambda (class-definition-form)
     (destructuring-bind (type name supers slots &rest options)
         class-definition-form
       `(,type ,name ,supers ,(mapcar function slots) ,@options)))
   class-definition-form))

(defun class-options (class-definition-form)
  "Return class-options of CLASS-DEFINITION-FORM."
  (cdddr class-definition-form))

(defun get-class-option (name class-definition-form)
  "Return a value of NAME class-option of CLASS-DEFINITION-FORM."
  (cadr (assoc name (class-options class-definition-form))))
