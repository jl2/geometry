;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:geometry
  (:use #:cl)
  (:export

   #:make-point
   #:make-rvector
   #:make-triangle
   #:make-rgba-color
   #:point-x
   #:point-y
   #:point-z
   #:rvector-x
   #:rvector-y
   #:rvector-z
   #:pt1
   #:pt2
   #:pt3
   #:x
   #:y
   #:z
   #:normal
   #:area
   #:triangle-pt1
   #:triangle-pt2
   #:triangle-pt3
   #:triangle-normal
   #:triangle-color
   #:tri-normal
   #:tcolor
   #:rgba-color
   #:rgba-color-red
   #:rgba-color-green
   #:rgba-color-blue
   #:rgba-color-alpha
   #:red
   #:green
   #:blue
   #:alpha
   #:random-color
   #:distance
   #:midpoint
   #:triangle-area
   #:vscale
   #:psub
   #:vsub
   #:vadd
   #:pvadd
   #:dot
   #:cross
   #:vlength
   #:normalize
   ))
