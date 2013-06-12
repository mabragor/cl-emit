;;;; This file is one of components of CL-EMIT system, licenced under GPL, see COPYING for details

(in-package #:cl-emit)

;;; OK, now what needs to be created?
;; - memoization cache
;; - registration of the memoization contexts
;; - FAIL macrolet in the context of DEFEMITRULE macro
;; - tests for all that
;; - || && and other convenient constructs

(defparameter *emit-rules* (make-hash-table))

(defstruct emit-rule-cell
  emit-lambda
  check-lambda
  no-node-p)

(defvar void (gensym) "Variable used to report that nothing should be output.")
(defvar no-node (gensym) "Fake node to memoize the result of emission of no-node rules.")

(defmacro! defemitrule (name destructure-vars (&body emit) &optional check)
  (let ((emit-lambda (if destructure-vars
			 `(lambda (,g!-node)
			    ,(if (atom destructure-vars)
				 `(let ((,destructure-vars ,g!-node))
				    ,@emit)
				 `(destructuring-bind ,destructure-vars ,g!-node
				    ,@emit)))
			 `(lambda () ,@emit)))
	(check-lambda (if destructure-vars
			  `(lambda (,g!-node)
			     ,@(if check
				   `(,(if (atom destructure-vars)
					  `(let ((,destructure-vars ,g!-node))
					     ,@check)
					  `(destructuring-bind ,destructure-vars ,g!-node
					     ,@check)))
				   `((declare (ignore ,g!-node))
				     t)))
			  `(lambda () ,@(or check (list 't))))))
    `(setf (gethash ',name *emit-rules*) (make-emit-rule-cell :emit-lambda ,emit-lambda
							      :check-lambda ,check-lambda
							      :no-node-p ,(if destructure-vars nil t)))))

;;; Memoization cache.
;; Actually, there are 2 caches - one for results of "type checks" - check-lambdas,
;; and the other for results of emission of the given node in the given context.
;; The former is simply an #'EQ hashtable
;; The latter is #'EQ hashtable of #'EQUAL assoc-lists, where EQ is for discriminating nodes, and
;; EQUAL for discriminating contexts.


(defparameter contexts nil)
(defmacro register-context (context-sym)
  `(push ',context-sym contexts))

(defvar *check-cache*)

(defun make-check-cache ()
  (make-hash-table :test #'eq))

(defun get-check-cached (rule node)
  (let ((it (gethash node *check-cache*)))
    (if it
	(gethash rule it)
	(values nil nil))))

(defun (setf get-check-cached) (result rule node)
  (a:acond-got ((gethash node *check-cache*) (setf (gethash rule it) result))
	       (t (setf (gethash rule (setf (gethash node *check-cache*) (make-hash-table :test #'equal)))
			result))))

(defvar *emit-cache*)
(defun make-emit-cache ()
  (make-hash-table :test #'eq))

(defun get-emit-cached (rule node)
  (let ((it (gethash node *emit-cache*)))
    (if it
	(let ((it2 (gethash rule it)))
	  (let ((res (assoc (mapcar #'symbol-value contexts) it2 :test #'equal)))
	    (if res
		(values (cdr res) t)
		(values nil nil))))
	(values nil nil))))

(defun (setf get-emit-cached) (result rule node)
  (let ((cur-contexts (mapcar #'symbol-value contexts)))
    (a:acond-got ((gethash node *emit-cache*)
		  (a:avcond-got that
				((gethash rule it) (let ((it2 (assoc cur-contexts that :test #'equal)))
						     (if it2
							 (setf (cdr it2) result)
							 (setf (gethash rule it) `((,cur-contexts . ,result) ,. that)))))
				(t (setf (gethash rule it) `((,cur-contexts . ,result))))))
		 (t (let ((it (make-hash-table :test #'equal)))
		      (setf (gethash node *emit-cache*) it
			    (gethash rule it) `((,cur-contexts . ,result))))))
    result))
		      
				  
(define-condition emit-error (error) ())

(defmacro fail () (error 'emit-error))

(defun descend (rule &optional node)
  (let* ((rule-cell (or (gethash rule *emit-rules*)
			(error 'emit-error)))
	 (no-node-p (emit-rule-cell-no-node-p rule-cell))
	 (node (if no-node-p
		   no-node
		   node)))
    (a:acond-got ((get-emit-cached rule node) (if (equal it *failed*)
						  (error 'emit-error)
						  it))
		 (t (a:apif (lambda (x) (equal *failed* x))
			    (if (or (get-check-cached rule node)
				    (setf (get-check-cached rule node)
					  (if no-node-p
					      (funcall (emit-rule-cell-check-lambda rule-cell))
					      (funcall (emit-rule-cell-check-lambda rule-cell)
						       node))))
				(setf (get-emit-cached rule node)
				      (handler-case (if no-node-p
							(funcall (emit-rule-cell-emit-lambda (gethash rule *emit-rules*)))
							(funcall (emit-rule-cell-emit-lambda (gethash rule *emit-rules*))
								 node))
					(emit-error () *failed*)))
				(setf (get-emit-cached rule node) *failed*))
			    (error 'emit-error)
			    it)))))
	
(defparameter *failed* (gensym) "Gensym, that denotes failure of emission.")
(defparameter *void* (gensym) "Gensym, that denotes success, but no value at all.")

(defun emit (rule &optional node)
  (let ((*check-cache* (make-check-cache))
	(*emit-cache* (make-emit-cache))
	(*failed* (gensym))
	(*void* (gensym)))
    (let ((res (descend rule node)))
      (cond ((eq res *failed*) (error 'emit-error))
	    ((eq res *void*) nil)
	    (t res)))))

