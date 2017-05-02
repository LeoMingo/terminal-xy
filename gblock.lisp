(defpackage :gblock
  (:export :*x-axis-len*
           :*y-axis-len*
           :+gm-vert-len+
           :+gm-horiz-len+
           :*positive-x-axis-len*
           :*positive-y-axis-len*

           :*gblock*
           :*gb-map*
           :gb-map-init
           :gb-map-printer
           :x-to-gb-index
           :y-to-gb-index
           :insert-point))


(defconstant +gb-vert-len+ 7)
(defconstant +gb-horiz-len+ 7)


;;gm vert and horiz lengths have to be even numbers
;;since it makes more sense after we trim off the arrows and the origin axis
;;we have 2 axis that have the same lengths
;(defparameter +gm-vert-len+ 20)
;(defparameter +gm-horiz-len+ 20)
(defconstant +gm-vert-len+ 4)
(defconstant +gm-horiz-len+ 4)

(defvar *x-axis-len* (* +gb-horiz-len+ +gm-horiz-len+))
(defvar *y-axis-len* (* +gb-vert-len+  +gm-vert-len+))

(defvar *positive-x-axis-len* (/ (- *x-axis-len* 2) 2))
(defvar *positive-y-axis-len* (/ (- *y-axis-len* 2) 2))


(defparameter *gblock* (make-array `(,+gb-vert-len+ ,+gb-horiz-len+)
                             :adjustable t
                             :initial-element " "
                             :element-type 'string))



(defparameter *gb-map* (make-array `(,*y-axis-len* ,*x-axis-len*)
                             :adjustable t))

(defun gb-map-init ()
  (dotimes (i +gm-vert-len+)
    (dotimes (j +gm-horiz-len+)
      (setf (aref *gb-map* i j) (make-array `(,+gb-vert-len+ ,+gb-horiz-len+)
                                          :adjustable t
                                          :initial-element " "
                                          :element-type 'string)))))

(defun gb-map-printer ()
  (dotimes (gmv +gm-vert-len+)
    (dotimes (gbv +gb-vert-len+)
      (dotimes (gmh +gm-horiz-len+)
        (dotimes (gbh +gb-horiz-len+)
          (format t "~a" (aref (aref *gb-map* gmv gmh) gbv gbh))))
      (format t "~%"))))




;;Since gb-map doesn't have to be square shaped, we always need this two functions
;;seperated
(defun x->gb-index (x) 
  (let ((idx (/ (+ x *positive-x-axis-len*) +gb-horiz-len+)))
    (let ((gm-idx (floor idx))
          (gb-remainder (rem (numerator idx) (denominator idx))))
      `(,gm-idx ,gb-remainder))))

(defun y->gb-index (y)
"Since the direction of y-axis is in reverse of array vertical counting,
the solution would be (- y) shifting first.
And also the line of (aref (aref gb-map 0 whatever) 0 whatever) is occupied by the y-axis arrow tip
we have to add 1 as well"
  (let ((idx (/ (+ (- y) *positive-y-axis-len* 1) +gb-vert-len+)))
    (let ((gm-idx (floor idx))
          (gb-remainder (rem (numerator idx) (denominator idx))))
      `(,gm-idx ,gb-remainder))))


(defun insert-point (x y elem)
 (let ((x-list (x->gb-index x))
       (y-list (y->gb-index y)))
   (let ((gm-x-idx (car x-list))
         (gb-x-remainder (cadr x-list))
         (gm-y-idx (car y-list))
         (gb-y-remainder (cadr y-list)))
        (format t "~a~a~a~a" gm-x-idx gm-y-idx gb-x-remainder gb-y-remainder)
        (setf (aref (aref *gb-map* gm-x-idx gm-y-idx) gb-x-remainder gb-y-remainder) elem))
   ))









