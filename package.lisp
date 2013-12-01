;;;; package.lisp

(defpackage #:time-liquid
  (:use #:cl #:defmacro-enhance #:rutils.symbol)
  (:export #:define-emit-env #:emit #:defemitrule #:descend #:fail-emit #:emit-error #:emit-error-reason
	   #:|| #:fail-muffled))

