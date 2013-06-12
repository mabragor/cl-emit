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

(define-emit-rule no-node-rule () ("No node now."))
(test no-node
  (is (equal "No node now."
	     (emit-tests-emit 'no-node-rule))))
  
(define-emit-rule failing-rule () () ((fail-emit "deliberate check")))
(define-emit-rule failing-rule-2 () ((fail-emit "deliberate emit")))
(define-emit-rule failing-rule-3 () () (nil))
(test informative-failures
  (is (equal "Check for rule FAILING-RULE failed: deliberate check."
	     (handler-case (emit-tests-emit 'failing-rule)
	       (cl-emit::emit-error (e) (cl-emit::emit-error-reason e)))))
  (is (equal "deliberate emit"
	     (handler-case (emit-tests-emit 'failing-rule-2)
	       (cl-emit::emit-error (e) (cl-emit::emit-error-reason e)))))
  (is (equal "Check for rule FAILING-RULE-3 failed."
	     (handler-case (emit-tests-emit 'failing-rule-3)
	       (cl-emit::emit-error (e) (cl-emit::emit-error-reason e)))))
  )
