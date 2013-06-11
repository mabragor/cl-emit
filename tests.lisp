(in-package :cl-user)

(defpackage :cl-emit-tests
  (:use :alexandria :cl :cl-emit :eos :iterate)
  (:shadowing-import-from :rutils.symbol :eval-always)
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

(define-emit-rule simple-int x
  ((format nil "~a" x))
  ((integerp x)))

(test simple-int
  (is (equal "123"
	     (emit-tests-emit 'simple-int 123)))
  (signals (error "emitting non-int didn't signal an error.")
    (emit-tests-emit 'simple-int :foobar)))

(eval-always
  (register-emit-tests-emit-context test-context in out))
(test contexts
  (is (equal cl-emit::void
	     (let ((test-context :out)) (emit-tests-emit 'out-test-context 123))))
  (signals (error "emitting non-int didn't signal an error.")
    (emit-tests-emit 'out-test-context 123))
  (signals (error "emitting non-int didn't signal an error.")
    (let ((test-context :out)) (emit-tests-emit 'in-test-context 123))))

  
