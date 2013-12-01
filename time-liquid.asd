;;;; time-liquid.asd

(defpackage :time-liquid-system
  (:use :cl :asdf))

(in-package time-liquid-system)

(asdf:defsystem #:time-liquid
  :serial t
  :version "0.3"
  :description "Context-sensitive serializing made easy."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:defmacro-enhance #:yaclanapht #:rutils)
  :components ((:file "package")
               (:file "time-liquid")
	       (:file "emit-basics")))

(defsystem :time-liquid-tests
  :description "Tests for TIME-LIQUID."
  :licence "GPL"
  :depends-on (:time-liquid :eos :cl-interpol :rutils)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :time-liquid))))
  (load-system :time-liquid-tests)
  (funcall (intern "RUN-TESTS" :time-liquid-tests)))


