(fiasco:define-test-package :runner-tests
  (:use :runner :fiasco :check-it)
  (:local-nicknames (:a :alexandria)))

(in-package :runner-tests)

(defparameter *elevations* '((0 . 5) (1 . 3) (2 . 15) (3 . 20) (4 . 10)))

(defparameter *paths* '((0 . (1 . 10))
			(0 . (2 . 8))
			(0 . (3 . 15))
			(1 . (3 . 12))
			(2 . (4 . 10))
			(3 . (4 . 5))
			(3 . (0 . 17))
			(4 . (0 . 10))))

(defparameter *elevations-1* '((5 . 1)
			       (4 . 3)
			       (2 . 5)
			       (1 . 4)))

(defparameter *paths-1* '((5 . (4 . 3))
			  (2 . (5 . 1))
			  (4 . (2 . 1))
			  (1 . (4 . 1))
			  (5 . (1 . 1))))

(defun distance (from to paths)
  (cddr (find-if (lambda (e) (and (= from (car e)) (= to (cadr e)))) paths)))

(defun verify-path (prev-place path len up? target elevations paths)
  (if path
      (let* ((place (car path))
	     (dist (distance prev-place place paths))
	     (higher? (< (elevation prev-place elevations) (elevation place elevations))))
	(when (and dist (or (eq up? higher?) up?))
	  (verify-path place (cdr path) (+ len dist) higher? target elevations paths)))
      (when (= prev-place target)
	len)))

(deftest find-path-0-test ()
  (let ((result (cons '(0 2 4 0) 28)))
    (is (equal result (find-path 0 *paths* *elevations*)))
    (is (= 28 (verify-path 0 (cdar result) 0 t 0 *elevations* *paths*)))))

(deftest find-path-1-test ()
  (let ((result (cons '(1 3 4 0 1) 37)))
    (is (equal result (find-path 1 *paths* *elevations*)))
    (is (= 37 (verify-path 1 (cdar result) 0 t 1 *elevations* *paths*)))))

(deftest find-path-2-test ()
  (let ((result (cons '(5 4 2 5) 5)))
    (is (equal result (find-path 5 *paths-1* *elevations-1*)))
    (is (= 5 (verify-path 5 (cdar result) 0 t 5 *elevations-1* *paths-1*)))))

(deftest find-path-null-test ()
  (is (null (find-path 3 *paths* *elevations*)))
  (is (null (find-path 2 *paths* *elevations*)))
  (is (null (find-path 4 *paths* *elevations*))))

(defun ->paths (xs ys ps seen paths)
  (if xs
      (let ((x (car xs))
	    (y (car ys)))
	(if (or (= x y) (member (cons x y) seen :test #'equal))
	    (->paths (cdr xs) (cdr ys) (cdr ps) seen paths)
	    (->paths (cdr xs) (cdr ys) (cdr ps) (cons (cons x y) seen) (cons (cons x (cons y (car ps))) paths))))
      paths))

(defun ->test-data (lists)
  (let* ((elev (car lists))
	 (xs (cadr lists))
	 (ys (caddr lists))
	 (ps (cadddr lists)))
    (cons (pairlis (a:iota (length elev)) elev) (->paths xs ys ps nil nil))))

(defun test-gen ()
  (generator
   (chain ((n (integer 3 50)))
          (generator (tuple
		      (list (integer 1 50) :length n)
		      (list (integer 0 (1- 10)) :length (* 2 n) )
		      (list (integer 0 (1- n)) :length (* 2 n))
		      (list (integer 1 15) :length (* 2 n)))))))

(defun check-result (result target paths elevations)
  (or (null result)
      (and
       (= target (caar result))
       (= (cdr result) (verify-path target (cdar result) 0 t target elevations paths)))))

(deftest find-path-gen-test ()
  (let ((*num-trials* 10000)
	(*list-size* 110))
    (check-it (test-gen)
	      (lambda (e)
		(let* ((data (->test-data e))
		       (elevations (car data))
		       (paths (cdr data))
		       (places (mapcar #'car elevations)))
		  (is (every (lambda (e) (check-result (find-path e paths elevations) e paths elevations)) places)))))))
