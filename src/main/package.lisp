
(defpackage cl-annot
  (:nicknames :annot)
  (:use :cl)
  ;; (:import-from :annot.helper
  ;;               :defannotation
  ;;               :annotation)
  (:import-from :annot.core
                :find-annot
                :define-annot)
  (:import-from :annot.syntax
                :enable-annot-syntax)
  (:export :define-annot
           :find-annot
           :enable-annot-syntax))
