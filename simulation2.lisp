(ql:quickload :cl-csv)

(in-package :cl-user)

(load "./restroom.lisp")

(use-package :com.takeico.r.restroom)

(defparameter *frequecy* 3)
(defparameter *use-duration* 1)
(defparameter *population-size* 1000)
(defparameter *facilities-per-restroom-range* '(:from 1 :to 30))
(defparameter *file-name* "simulation2.csv")

(defparameter *data* (make-hash-table))

(defun make-person ()
  (make-instance 'person
     :use-duration *use-duration*
     :frequency *frequecy*))

(loop for facilities-per-restroom
      from (getf *facilities-per-restroom-range* :from)
      to (getf *facilities-per-restroom-range* :to)
      do
;;   (format t "~a~%" facilities-per-restroom)
      (setf *person-population*
	    (loop repeat *population-size* collect (make-person)))
      (let ((restroom (make-instance 'restroom :facilities-per-room facilities-per-restroom))
	    (data nil))
	(dotimes (time *duration*)
	  (let ((queue (copy-list (queue restroom))))
	    (setf (slot-value restroom 'queue) nil)
	    (push (length queue) data)
	    (loop while queue do (enter restroom (pop queue))))
	  (loop for person in *person-population*
		when (need-to-go-p person)
		  do (enter restroom person))
	  (tick restroom))
	(setf data (nreverse data))
	(setf (gethash facilities-per-restroom *data*) data)))

(with-open-file (out *file-name* :direction :output :if-exists :supersede)
  (cl-csv:write-csv-row
   (loop for facilities-per-restroom
	from (getf *facilities-per-restroom-range* :from) to (getf *facilities-per-restroom-range* :to)
	collect facilities-per-restroom) :stream out)
   (dotimes (time *duration*)
     (let ((row nil))
       (loop for facilities-per-restroom
	from (getf *facilities-per-restroom-range* :from) to (getf *facilities-per-restroom-range* :to)
	     do (push (nth time (gethash facilities-per-restroom *data*)) row)
	     finally (setf row (nreverse row)))
       (cl-csv:write-csv-row row :stream out))))

;; (loop for k being the hash-key in *data*
;;       using (hash-value v)
;;       do (format t "~a:~a~%" k v))
