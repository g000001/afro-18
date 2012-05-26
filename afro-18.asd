;;;; afro-18.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :afro-18
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "readtable")
               (:file "afro-18")))

(defmethod perform ((o test-op) (c (eql (find-system :afro-18))))
  (load-system :afro-18)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :afro-18.internal :afro-18))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

