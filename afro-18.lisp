;;;; afro-18.lisp

(cl:in-package :afro-18.internal)
;; (in-readtable :afro-18)

(def-suite afro-18)

(in-suite afro-18)

(defmacro with-stack-list ((variable &rest elements) &body body)
  `(let ((,variable (list ,@elements)))
     (declare (dynamic-extent ,variable))
     ,@body))

(defmacro progs (init &body body)
  (reduce (lambda (ans e)
            `(multiple-value-call ,e ,ans))
          body
          :initial-value init))

(defmacro defafro (name (&rest required-args) &body body)
  (let ((args (gensym "args-")))
    `(defun ,name (,@required-args &rest ,args)
       (declare (dynamic-extent ,args))
       (progs (values-list ,args)
         ,@body))))

(defmacro afro (&body body)
  (let ((stack (gensym "stack-")))
   `(lambda (&rest ,stack)
      (declare (dynamic-extent ,stack))
      (progs (values-list ,stack)
        ,@body))))

(defun drop (&rest args)
  (declare (dynamic-extent args))
  (values-list (cdr args)))

(defun nip (&rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       (car args)
                       (values-list (cddr args))))

(defun swap (x y &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       y
                       x
                       (values-list args)))

(defun over (&rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       (second args)
                       (values-list args)))

(defun tuck (x y &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       x
                       y
                       x
                       (values-list args)))

(defun modulo (x y &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       (cl:rem y x)
                       (values-list args)))

(defun dup (&rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       (first args)
                       (values-list args)))

(defun 0= (x &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       (if (zerop x)
                           -1
                           0)
                       (values-list args)))

(defun until (mvfn &rest args)
  (declare (dynamic-extent args))
  (labels ((*until (&rest args)
             (declare (dynamic-extent args))
             (multiple-value-call
               (lambda (true? &rest args)
                 (if (= -1 true?)
                     (values-list args)
                     (multiple-value-call #'*until
                                          (values-list args))))
               (apply mvfn args))))
    (apply #'*until args)))

(defun pu (x)
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (multiple-value-call #'values
                         x
                         (values-list args))))

(defun rot (x y z &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       z x y
                       (values-list args)))
(defafro 2drop ()
  #'drop
  #'drop)

(defun 2nip (a b c d &rest args)
  (declare (dynamic-extent args)
           (ignore c d))
  (multiple-value-call #'values
                       a b
                       (values-list args)))

(defun 2dup (a b &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       a b
                       a b
                       (values-list args)))

(defun 2over (a b c d &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       c d
                       a b
                       c d
                       (values-list args)))

(defun 2swap (a b c d &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       c d
                       a b
                       (values-list args)))

(defun 2rot (a b c d e f &rest args)
  (declare (dynamic-extent args))
  (multiple-value-call #'values
                       e f
                       a b
                       c d
                       (values-list args)))
(defafro 2tuck ()
  #'2swap
  #'2over)

;; : gcd ( u1 u2 )
;;     begin
;;         tuck mod
;;         dup 0=
;;     until
;;     drop ;
(defafro my-gcd ()
  (pu (afro
        #'tuck #'modulo
        #'dup #'0=))
  #'until
  #'drop )

;(my-gcd 1029 1071 8 8 8 8 8)
;=>  21
;    8
;    8
;    8
;    8
;    8

;;; eof
