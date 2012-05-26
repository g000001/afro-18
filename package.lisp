;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :afro-18
  (:use)
  (:export))

(defpackage :afro-18.internal
  (:use :afro-18 :cl :named-readtables :fiveam))

