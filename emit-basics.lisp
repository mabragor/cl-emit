;;;; This file is one of components of CL-EMIT system, licenced under GPL, see COPYING for details

(in-package #:time-liquid)

(defmacro define-emit-env (symbol)
  `(eval-always
     (defvar ,(sb-int:symbolicate symbol "-EMIT-RULES") (make-hash-table))
     (defvar ,(sb-int:symbolicate symbol "-EMIT-CONTEXTS") nil)
     (defmacro ,(sb-int:symbolicate "WITH-" symbol "-EMIT-RULES") (&body body)
       `(let ((*emit-rules* ,',(sb-int:symbolicate symbol "-EMIT-RULES")))
	  ,@body))
     (defmacro ,(sb-int:symbolicate "WITH-" symbol "-EMIT-CONTEXTS") (&body body)
       `(let ((contexts ,',(sb-int:symbolicate symbol "-EMIT-CONTEXTS")))
	  ,@body))
     (defmacro ,(sb-int:symbolicate 'define-emit-rule) (name destructure-vars (&body emit) &optional check)
       `(,',(sb-int:symbolicate "WITH-" symbol "-EMIT-RULES")
	    (,',(sb-int:symbolicate "WITH-" symbol "-EMIT-CONTEXTS")
		(defemitrule ,name ,destructure-vars ,emit ,check))))
     (defmacro ,(sb-int:symbolicate "REGISTER-" symbol "-EMIT-CONTEXT")
	 (context-var &rest plausible-contexts)
       `(progn (defparameter ,context-var ,(sb-int:keywordicate (format nil "~a" (car plausible-contexts))))
	       ,@(mapcar (lambda (context-name)
			   (let ((pred-name (sb-int:symbolicate context-name
								"-"
								context-var
								"-P"))
				 (rule-name (sb-int:symbolicate context-name
								"-"
								context-var)))
			     `(progn
				(defun ,pred-name (x)
				  (declare (ignore x))
				  (equal ,context-var ,(sb-int:keywordicate context-name)))
				(,(sb-int:symbolicate 'define-emit-rule) ,rule-name x
				  ((declare (ignore x))
				   void)
				  ((,pred-name x))))))
			 (mapcar (lambda (x) (format nil "~a" x)) plausible-contexts))
	       (push ',context-var ,',(sb-int:symbolicate symbol "-EMIT-CONTEXTS"))))
     (defmacro ,(sb-int:symbolicate symbol "-EMIT") (symbol &optional node)
       `(,',(sb-int:symbolicate "WITH-" symbol "-EMIT-RULES")
	    (,',(sb-int:symbolicate "WITH-" symbol "-EMIT-CONTEXTS")
		(emit ,symbol ,node))))))

