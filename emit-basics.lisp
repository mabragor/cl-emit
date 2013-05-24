;;;; This file is one of components of CL-EMIT system, licenced under GPL, see COPYING for details

(in-package #:cl-emit)

(defmacro define-emit-env (symbol)
  `(progn (eval-always
	    (defvar ,(sb-int:symbolicate symbol "-RULES") (make-hash-table))
	    (defvar ,(sb-int:symbolicate symbol "-CONTEXTS") nil))
	  (defmacro ,(sb-int:symbolicate "WITH-" symbol "-RULES") (&body body)
	    `(let ((*emit-rules* ,',(sb-int:symbolicate symbol "-RULES")))
	       ,@body))
	  (defmacro ,(sb-int:symbolicate "WITH-" symbol "-CONTEXTS") (&body body)
	    `(let ((contexts ,',(sb-int:symbolicate symbol "-CONTEXTS")))
	       ,@body))
	  (defmacro ,(sb-int:symbolicate 'define-rule) (name destructure-vars (&body emit) &optional check)
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
		 (,',(sb-int:symbolicate "WITH-" symbol "-CONTEXTS")
		     (define-emit-rule ,name ,destructure-vars ,emit ,check))))
	  (defmacro ,(sb-int:symbolicate "REGISTER-" symbol "-CONTEXT")
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
				     (,(sb-int:symbolicate 'define-rule) ,rule-name x
				       ((declare (ignore x))
					(void))))))
			      (mapcar (lambda (x) (format nil "~a" x)) plausible-contexts))
		    (push ',context-var ,',(sb-int:symbolicate symbol "-CONTEXTS"))))
	  (defmacro ,(sb-int:symbolicate symbol "-EMIT") (symbol node)
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
		 (,',(sb-int:symbolicate "WITH-" symbol "-CONTEXTS")
		     (emit ,symbol ,node))))))

