;;;; package.lisp

(defpackage #:cl-emit
  (:use #:cl #:defmacro-enhance #:rutils.symbol)
  (:export #:define-emit-env #:emit #:define-emit-rule))

