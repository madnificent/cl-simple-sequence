;;;; CL simple sequence
(defpackage :seq
  (:use :common-lisp)
  (:export :seq :s+ :s- :s/ :listify :contains :flatten :sequencify))

(in-package :seq)

;;;; -- Common Lisp simple Sequence utility --
;;;;
;;;; The code is distributed under an MIT-style license which can be found in the git repository 
;;;; github.com/madnificent/cl-simple-sequence
;;;;
;;;; This is a simple library for common lisp to work with sequences.
;;;; 
;;;; The following examples cover the methods in the library.
;; 
;; (let ((seq (s/ (s+ (seq -10 10) (seq 100 200)) (s+ (seq 1 5) (seq 195 20000))))) ; creation of a sequence
;;   (listify seq) ; returns a list of all elements in the sequence
;;   (contains seq 3)) ; is 3 an element of this sequence?
;;
;; ;; example of slow sequences
;; (let ((seq (s/ (s+ (seq -10 10) (s- (seq 100 20000000) (seq 200 2000000000))) (s+ (seq 1 5) (seq 195 20000)))))
;;        (time (listify seq)) ; If you'd use this multiple times, you'd better optimise it!
;;        (setf seq (flatten seq))
;;        (time (listify seq))); Much better!
;;
;; ;; example of customized listifying (useable for sequences that are not (yet) readily supported).
;; (listify (s+ (sequencify (loop for x from -20 upto 20 collect (* 3 x)))
;; 	        (seq -15 15) (seq 40 60)))
;;
;;;;
;;;;
;;;; -- Happy lisping - madnificent --

(defmacro with-gensyms ((&rest symbols) &body body)
  `(let ,(map 'list (lambda (sym) `(,sym (gensym))) symbols)
     ,@body))

(defun seq (start end)
  "Creates a new sequence."
  (let ((contains (lambda (x) (<= start x end)))
	(listify  (lambda () (loop for i from start upto end collect i))))
    (lambda (req) (if (eql req 'contains)
		      contains
		    listify))))

(defun s+ (a &rest bs)
  "Unification of <a> with <bs>"
  (unless bs (return-from s+ a))
  (let ((b (first bs)))
    (let ((a-contains (funcall a 'contains))
	  (b-contains (funcall b 'contains))
	  (a-list (funcall a 'listify))
	  (b-list (funcall b 'listify)))
      (let ((contains (lambda (x) (or (funcall a-contains x) (funcall b-contains x))))
	    (listify (lambda () 
			(sort (concatenate 'list 
					   (funcall a-list)
					   (loop for x in (funcall b-list)
						 unless (funcall a-contains x)
						 collect x))
			      '<))))
	(apply 's+ 
	       (lambda (req) (if (eql req 'contains) contains listify))
	       (rest bs))))))

(defun s- (a &rest bs)
  "Substraction of <bs> from <a>."
  (unless bs (return-from s- a))
  (let ((b (first bs)))
    (let ((a-contains (funcall a 'contains))
	  (b-contains (funcall b 'contains))
	  (a-list (funcall a 'listify)))
      (let ((contains (lambda (x) (and (funcall a-contains x) (not (funcall b-contains x)))))
	    (listify (lambda () (loop for x in (funcall a-list)
				      unless (funcall b-contains x)
				      collect x))))
	(apply 's- 
	       (lambda (req) (if (eql req 'contains) contains listify))
	       (rest bs))))))

(defun s/ (a &rest bs)
  "The overlap of all given sequences."
  (unless bs (return-from s/ a))
  (apply 's/ 
	 (s- a (s- a (first bs))) ;; overlap is [A - (A - B)]
	 (rest bs)))

(defun listify (seq)
  "Returns the list of values represented by seq"
  (funcall (funcall seq 'listify)))

(defun contains (seq val)
  "Returns true iff <seq> contains <val>"
  (funcall (funcall seq 'contains) val))

(defun sequencify (list)
  "Creates a sequence from the given list."
  (let ((seqs nil)
	(start (first list))
	(prev (first list)))
    (dolist (x (rest list))
      (unless (= x (1- prev))
	(push (seq start prev) seqs)
	(setf start x))
      (setf prev x))
    (push (seq start prev) seqs)
    (apply 's+ seqs)))
	  
(defun flatten (seq)
  "Flattens the sequence, which may remove redundant data (does good for any standard sequence)."
  (sequencify (listify seq)))