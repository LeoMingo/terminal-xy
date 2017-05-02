(defpackage :increment-point-assign
  (:use :gblock :cl-utils)
  (:export  :+x-output-filename+
            :+coord-point-list-filename+
            
            :axis-x-output   ;This is for the outputing of input Xs of required points a function, which are limited by the zoom-measure value
            :make-coord-point-list
            :increment-point-assign))



(defconstant +x-output-filename+ "x-output")
(defconstant +coord-point-list-filename+ "coord-point-list")


(defun axis-x-output (;x-output-filename
                      )
  (let ((num-of-x (1+ (* *positive-x-axis-len* 2)))
        (num-of-y (1+ (* *positive-y-axis-len* 2)))
        (x-output-arr (make-adjustable-array nil)))
    (if (> *positive-x-axis-len* *positive-y-axis-len*)
      (dotimes (i num-of-y)
        (vector-push-extend (- i *positive-y-axis-len*) x-output-arr))
      (dotimes (i num-of-x)
        (vector-push-extend (- i *positive-x-axis-len*) x-output-arr)))
    
    (write-line-arr x-output-filename x-output-arr)))




(defun line->xy-arr (line)
  (let ((xy-str-arr (split-at line #\|))
        (xy (make-adjustable-array nil)))
    (dotimes (i 2)
      (vector-push-extend (multiple-value-bind (num) 
                            (parse-integer (aref xy-str-arr i)) num) xy))
    xy))

(defun make-coord-point-list (coord-point-list-filename)
;It is pre-assumed the gb-map would be a square, but if not, take the shorter axis length as measure.
  (let* ((x-len (* *positive-x-axis-len* 2))
         (y-len (* *positive-y-axis-len* 2))
         (xy-arr (read-line-arr coord-point-list-filename))
         (xy-arr-len (length xy-arr))) ;xy-arr-len is the points' amount
    (defvar *coord-point-list* (make-array `(,xy-arr-len) :adjustable t))
    (dotimes (i (length xy-arr))
      (setf (aref *coord-point-list* i) (line->xy-arr (aref xy-arr i))))
    *coord-point-list*))


(defun increment-point-assign (coord-point-list)
  (dotimes (i (length *coord-point-list*))
    (let ((x (fst (aref *coord-point-list* i)))
          (y (snd (aref *coord-point-list* i))))
      (if (and (<= x *positive-x-axis-len*) (<= y *positive-y-axis-len*))
        (insert-point x y "*")))))















