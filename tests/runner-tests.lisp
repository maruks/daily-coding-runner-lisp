(fiasco:define-test-package :runner-tests
  (:use :runner :fiasco))

(in-package :runner-tests)

(defparameter *elevations* '((0 . 5) ( 1 . 25) ( 2 . 15) ( 3 . 20) ( 4 . 10)))

(defparameter *paths* '((0 . (1 . 10))
			(0 . (2 . 8))
			(0 . (3 . 15))
			(1 . (3 . 12))
			(2 . (4 . 10))
			(3 . (4 . 5))
			(3 . (0 . 17))
			(4 . (0 . 10))))

(deftest find-path-test ()
  (let ((result (cons '(0 2 4 0) 28)))
    (is (equal result (find-path 0 *paths* *elevations*)))))
