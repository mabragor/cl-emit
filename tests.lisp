(in-package :cl-user)

(defpackage :cl-emit-tests
  (:use :alexandria :cl :cl-emit :eos :iterate)
  (:export #:run-tests))

(in-package :cl-emit-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite clemit)
(in-suite clemit)

(defun run-tests ()
  (let ((results (run 'clemit)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))

(define-emit-env emit-tests)

(define-rule simple-int x
  ((format nil "~a" x))
  ((integerp x)))

(test simple-int
  (is (equal "123"
	     (emit-tests-emit 'simple-int 123)))
  (signals (error "emitting non-int didn't signal an error.")
    (emit-tests-emit 'simple-int :foobar)))

