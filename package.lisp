(defpackage #:cl-tsetlin
  (:use :cl)
  (:import-from
   :closer-mop
   :validate-superclass
   :standard-direct-slot-definition
   :standard-effective-slot-definition
   :direct-slot-definition-class
   :effective-slot-definition-class
   :compute-effective-slot-definition
   :slot-value-using-class))
