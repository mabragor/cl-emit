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

(defmacro capture-emit-fail-reason (&rest forms)
  `(handler-case (progn ,@forms)
     (cl-emit::emit-error (e) (cl-emit::emit-error-reason e))))
  
(define-emit-rule failing-rule () () ((fail-emit "deliberate check")))
(define-emit-rule failing-rule-2 () ((fail-emit "deliberate emit")))
(define-emit-rule failing-rule-3 () () (nil))
(test informative-failures
  (is (equal "Check for rule FAILING-RULE failed: deliberate check."
	     (capture-emit-fail-reason (emit-tests-emit 'failing-rule))))
  (is (equal "deliberate emit"
	     (capture-emit-fail-reason (emit-tests-emit 'failing-rule-2))))
  (is (equal "Check for rule FAILING-RULE-3 failed."
	     (capture-emit-fail-reason (emit-tests-emit 'failing-rule-3)))))


(define-emit-rule partially-failing-choice ()
  ((|| (descend 'simple-int :foo)
       (descend 'simple-int :bar)
       (descend 'simple-int 123)
       (descend 'simple-int :baz))))
(define-emit-rule fully-failing-choice ()
  ((|| (descend 'simple-int :foo)
       (descend 'simple-int :bar)
       (descend 'simple-int :baz))))

(test ordered-choice
  (is (equal "123" (emit-tests-emit 'partially-failing-choice)))
  (is (equal "Ordered choice failed with reasons:
  - Check for rule SIMPLE-INT failed.
  - Check for rule SIMPLE-INT failed.
  - Check for rule SIMPLE-INT failed.
" (capture-emit-fail-reason (emit-tests-emit 'fully-failing-choice)))))
  
