;;;; This file is one of components of CL-EMIT system, licenced under GPL, see COPYING for details

(in-package #:cl-emit)

;;; OK, now what needs to be created?
;; - memoization cache
;; - registration of the memoization contexts
;; - FAIL macrolet in the context of DEFINE-EMIT-RULE macro
;; - tests for all that
;; - || && and other convenient constructs

(defparameter *emit-rules* (make-hash-table))

(defstruct emit-rule-cell
  emit-lambda
  check-lambda)

(defmacro! define-emit-rule (name destructure-vars (&body emit) &optional check)
  (let ((emit-lambda `(lambda (,g!-node)
			(destructuring-bind ,destructure-vars ,g!-node
			  ,@emit)))
	(check-lambda `(lambda (,g!-node)
			 ,@(if check
			       `((destructuring-bind ,destructure-vars ,g!-node
				 ,@check))
			       `((declare (ignore ,g!-node))
				 t)))))
    `(setf (gethash ,name *emit-rules*) (make-emit-rule-cell :emit-lambda ,emit-lambda
							     :check-lambda ,check-lambda))))

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

(defun descend (rule node)
  (a:acond-got ((get-emit-cached rule node) it)
	       (t (if (or (get-check-cached rule node)
			  (setf (get-check-cached rule node)
				(funcall (emit-rule-cell-check-lambda (gethash rule *emit-rules*)) node)))
		      (setf (get-emit-cached rule node)
			    (handler-case (funcall (emit-rule-cell-emit-lambda (gethash rule *emit-rules*)) node)
			      (emit-error () *failed*)))
		      (setf (get-emit-cached rule node) *failed*)))))
	
(defparameter *failed* (gensym) "Gensym, that denotes failure of emission.")

(defun emit (rule node)
  (let ((*check-cache* (make-check-cache))
	(*emit-cache* (make-emit-cache))
	(*failed* (gensym)))
    (let ((res (descend rule node)))
      (if (eq res *failed*)
	  (error 'emit-error)))))
