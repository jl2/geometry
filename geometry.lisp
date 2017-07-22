;;;; geometry.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:geometry)

;;; "geometry" goes here. Hacks and glory await!

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

(defstruct rgba-color 
  (red 0)
  (green 0)
  (blue 0)
  (alpha 0))

(defun random-color ()
  (make-rgba-color :red (+ 0.15 (random 0.5))
                   :green (+ 0.15 (random 0.5))
                   :blue (+ 0.15 (random 0.5))
                   :alpha 1.0))

(defstruct point
  "Three double-floats that identify a location in 3D space."
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float))

(defstruct rvector
  "Three double-floats."
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float))

(defun tri-normal (pt1 pt2 pt3)
  (normalize (cross  (psub pt2 pt1) (psub pt3 pt1))))

(defstruct triangle
  "Three points, a normal, and an attribute"
  (pt1 (make-point) :type point)
  (pt2 (make-point) :type point)
  (pt3 (make-point) :type point)
  (normal (make-rvector) :type rvector)
  (tcolor (random-color) :type rgba-color)
  (attribute 0 :type fixnum))

(defun distance (pt1 pt2)
  "Compute the distance between two points."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type point pt1 pt2))
  (let ((xd (- (point-x pt1) (point-x pt2)))
        (yd (- (point-y pt1) (point-y pt2)))
        (zd (- (point-z pt1) (point-z pt2))))
    (sqrt (+ (* xd xd) (* yd yd) (* zd zd)))))

(defun midpoint (pt1 pt2)
  (with-slots ((x1 x) (y1 y) (z1 z)) pt1
    (with-slots ((x2 x) (y2 y) (z2 z)) pt2
      (make-point
       :x (/ (+ x2 x1) 2)
       :y (/ (+ y2 y1) 2)
       :z (/ (+ z2 z1) 2)))))

(defun triangle-area (tri)
  "Compute the area of a triangle."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type triangle tri))
  (with-slots (pt1 pt2 pt3) tri
    (let* ((a (distance pt1 pt2))
           (b (distance pt1 pt3))
           (c (distance pt2 pt3))
           (s (* 0.5 (+ a b c))))
      (the double-float (sqrt (* s (- s a) (- s b) (- s c)))))))

(defmacro each-point-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (point-x ,p1) (point-x ,p2))
                 :y (funcall ,op (point-y ,p1) (point-y ,p2))
                 :z (funcall ,op (point-z ,p1) (point-z ,p2))))

(defmacro each-vector-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (rvector-x ,p1) (rvector-x ,p2))
                 :y (funcall ,op (rvector-y ,p1) (rvector-y ,p2))
                 :z (funcall ,op (rvector-z ,p1) (rvector-z ,p2))))

(defun vscale (k v)
  (declare (type double-float k)
           (type rvector v))
  (make-rvector :x (* k (rvector-x v))
                :y (* k (rvector-y v))
                :z (* k (rvector-z v))))

(defun psub (p1 p2)
  (declare (type point p1 p2))
  (each-point-to-vector #'- p1 p2))

(defun vsub (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'- v1 v2))

(defun vadd (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'+ v1 v2))

(defun pvadd (p v)
  (declare (type point p)
           (type rvector v))
  (make-point :x (+ (point-x p) (rvector-x v))
              :y (+ (point-y p) (rvector-y v))
              :z (+ (point-z p) (rvector-z v))))

(defun dot (v1 v2)
  (declare (type rvector v1 v2))
  (+ (* (rvector-x v1) (rvector-x v2))
     (* (rvector-y v1) (rvector-y v2))
     (* (rvector-z v1) (rvector-z v2))))

(defun cross (v1 v2)
  (declare (type rvector v1 v2))
  (with-slots ((x1 x) (y1 y) (z1 z)) v1
    (with-slots ((x2 x) (y2 y) (z2 z)) v2
      (make-rvector :x (- (* y1 z2) (* z1 y2))
                    :y (- (* z1 x2) (* x1 z2))
                    :z (- (* x1 y2) (* y1 x2))))))

(defun vlength (vect)
  (declare (type rvector vect))
  (let ((xl (rvector-x vect))
        (yl (rvector-y vect))
        (zl (rvector-z vect)))
    (sqrt (+ (* xl xl) (* yl yl) (* zl zl)))))

(defun normalize (vect)
  (declare (type rvector vect))
  (let ((vl (vlength vect)))
    (if (zerop vl)
        vect
        (vscale (/ 1.0 vl) vect))))

