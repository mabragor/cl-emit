;;;; cl-emit.asd

(asdf:defsystem #:cl-emit
  :serial t
  :version "0.1"
  :description "Context-sensitive serializing made easy."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:defmacro-enhance #:yaclanapht)
  :components ((:file "package")
               (:file "cl-emit")))

