(defpackage :runner
  (:use cl)
  (:export find-path))

(in-package :runner)

(defun places (from paths)
  (mapcar #'cdr (remove-if-not (lambda (e) (= (car e) from)) paths)))

(defun targets (place uphill? paths elevations)
  (let ((places (places place paths))
	(elevation (elevation place elevations)))
    (remove-if-not (lambda (e) (eq uphill? (< elevation (elevation (car e) elevations)))) places)))

(defun elevation (place elevations)
  (cdr (assoc place elevations)))

(defun backtrack (place track)
  (let ((prev-place (cdr (assoc place track))))
    (if (and prev-place (not (= place prev-place)))
	(cons place (backtrack prev-place track))
	(list place))))

(defun bfs (queue target visited track paths elevations)
  (when queue
    (let* ((elem (car queue))
	   (length (car elem))
	   (place (cadr elem))
	   (uphill? (caddr elem))
	   (prev-place (cdddr elem))
	   (uphill-targets (when uphill? (targets place t paths elevations)))
	   (downhill-targets (when (not (= place target)) (targets place nil paths elevations)))
	   (targets (append downhill-targets uphill-targets))
	   (elevation (elevation place elevations))
	   (enqueue (mapcar (lambda (e) (let* ((len (cdr e))
					       (plc (car e))
					       (up? (< elevation (elevation plc elevations))))
					  (cons (+ len length) (cons plc (cons up? place))))) targets))
	   (filtered (remove-if (lambda (e) (let ((plc (cadr e))) (member plc visited))) enqueue))
	   (new-visited (if (= place target) visited (cons place visited)))
	   (new-track (if (null (assoc place track)) (acons place prev-place track) track))
	   (new-queue (sort (append (cdr queue) filtered) (lambda (a b) (< (car a) (car b))))))
      (if (and (not uphill?) (= place target))
	  (cons (reverse (backtrack prev-place track)) length)
	  (bfs new-queue target new-visited new-track paths elevations)))))

(defun find-path (target paths elevations)
  (bfs (list (cons 0 (cons 0 (cons t 0)))) target nil nil paths elevations))
