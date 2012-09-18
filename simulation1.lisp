(ql:quickload :cl-csv)

(in-package :cl-user)

(load "restroom.lisp")

(use-package :com.takeico.r.restroom)

(defparameter *frequency* 3)
(defparameter *facilities-per-restroom* 3)
(defparameter *use-duration* 1)
(defparameter *population-range* '(:from 10 :to 600 :by 10))
(defparameter *file-name* "simulation1.csv")

(defparameter *data* (make-hash-table))

(defun make-person ()
  (make-instance 'person
     :use-duration *use-duration*
     :frequency *frequency*))

(loop for population-size
	from (getf *population-range* :from) to (getf *population-range* :to)
	  by (getf *population-range* :by)
      do
   (format t "population=~a " population-size)
   (setf *person-population*
	 (loop repeat population-size collect (make-person)))
   (let ((restroom (make-instance 'restroom :facilities-per-room *facilities-per-restroom*))
	 (data nil))
     (dotimes (time *duration*)
       (push (length (queue restroom)) data)
       ;; queued person enter unoccupied restroom
       (let ((queue (copy-list (queue restroom))))
	 (setf (slot-value restroom 'queue) nil)
	 (loop while queue
	       do (enter restroom (pop queue))))
       ;; enter restroom if need to go
       (loop for person in *person-population*
	 when (need-to-go-p person)
	   do (enter restroom person))
       (tick restroom))
     (setf data (nreverse data))
     (format t "data=~a~%" data)
     (setf (gethash population-size *data*) data)))

(with-open-file (out *file-name* :direction :output :if-exists :supersede)
  (cl-csv:write-csv-row
   (loop for population-size
	from (getf *population-range* :from) to (getf *population-range* :to)
	  by (getf *population-range* :by)
	collect population-size) :stream out)
   (dotimes (time *duration*)
     (let ((row nil))
       (loop for population-size
	from (getf *population-range* :from) to (getf *population-range* :to)
	  by (getf *population-range* :by)
	     do (push (nth time (gethash population-size *data*)) row)
	     finally (setf row (nreverse row)))
       (cl-csv:write-csv-row row :stream out))))
  