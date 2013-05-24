;;;; cl-emit.asd

(defpackage :cl-emit-system
  (:use :cl :asdf))

(in-package cl-emit-system)

(asdf:defsystem #:cl-emit
  :serial t
  :version "0.1"
  :description "Context-sensitive serializing made easy."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:defmacro-enhance #:yaclanapht #:rutils)
  :components ((:file "package")
               (:file "cl-emit")
	       (:file "emit-basics")))

(defsystem :cl-emit-tests
  :description "Tests for CL-EMIT."
  :licence "GPL"
  :depends-on (:cl-emit :eos :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-emit))))
  (load-system :cl-emit-tests)
  (funcall (intern "RUN-TESTS" :cl-emit-tests)))


