(in-package :common-lisp)

(defpackage :com.takeico.r.restroom
  (:use :common-lisp)
  (:export :*person-population*
	   :*duration*
	   :restroom
	   :facility
	   :person
	   :queue
	   :enter
	   :need-to-go-p
	   :tick))

(in-package :com.takeico.r.restroom)

(defparameter *person-population* nil)
(defparameter *duration* (* 9 60))

(defclass restroom ()
     ((queue
       :reader queue
       :initform nil)
      (facilities
       :reader facilities)))

(defmethod initialize-instance :after ((restroom restroom) &key (facilities-per-room 3))
  (with-slots (queue facilities) restroom
     (setf facilities
	   (loop repeat facilities-per-room collect (make-instance 'facility)))))

(defmethod enter ((restroom restroom) person)
  (let ((unoccupied-facility (find-if-not #'occupied-p (facilities restroom))))
    (if unoccupied-facility
	(occupy unoccupied-facility person)
	(push person (slot-value restroom 'queue))))
  (delete person *person-population*))


(defmethod tick ((restroom restroom))
  (loop for facility in (facilities restroom) do (tick facility)))

(defclass facility ()
     ((occupier
       :initform nil)
      duration))

(defmethod occupy ((facility facility) person)
  (unless (occupied-p facility)
    (with-slots (occupier duration) facility
       (setf occupier person)
       (setf duration 1))
    (setf *person-population* (delete person *person-population*))
    t))

(defmethod occupied-p ((facility facility))
  (slot-value facility 'occupier))

(defmethod vacate ((facility facility))
  (with-slots (occupier) facility
     (push occupier *person-population*)
     (setf occupier nil)))

(defmethod tick ((facility facility))
  (when (occupied-p facility)
    (with-slots (duration occupier) facility
       (if (> duration (use-duration occupier))
	   (progn
	     (vacate facility)
	     (setf duration 0))
	   (incf duration)))))

(defclass person ()
     ((use-duration
       :initarg :use-duration
       :initform 1
       :reader use-duration)
      (frequency
       :initarg :frequency
       :initform 4
       :accessor frequency)))

(defmethod need-to-go-p ((person person))
  (<= (1+ (random *duration*)) (frequency person)))
