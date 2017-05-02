(defpackage :increment-point-assign
  (:use :gblock :cl-utils)
  (:export  :+x-output-filename+
            :+coord-point-list-filename+
            
            :axis-x-output   ;This is for the outputing of input Xs of required points a function, which are limited by the zoom-measure value
            :make-coord-point-list
            :increment-point-assign))



(defconstant +x-output-filename+ "x-output")
(defconstant +coord-point-list-filename+ "coord-point-list")


(defun axis-x-output (x-output-filename)
  (let ((num-of-x (1+ (* *positive-x-axis-len* 2)))
        (num-of-y (1+ (* *positive-y-axis-len* 2))))
    (if (> *positive-x-axis-len* *positive-y-axis-len*)
      (progn 
        (defvar x-output-arr (make-array '(num-of-y) :adjustable t :initial-element 0))
        (dotimes (i num-of-y)
          (setf (aref x-output-arr i) (- i *positive-y-axis-len*))))
      (progn
        (defvar x-output-arr (make-array '(num-of-x) :adjustable t :initial-element 0))
        (dotimes (i num-of-x)
          (setf (aref x-output-arr i) (- i *positive-x-axis-len*)))))
    (write-line-arr x-output-filename x-output-arr)))




(defun line->xy-arr (line)
  (multiple-value-bind (x y) (parse-integer line :junk-allowed t) `#(,x ,y)))

(defun make-coord-point-list (coord-point-list-filename)
;It is pre-assumed the gb-map would be a square, but if not, take the shorter axis length as measure.
  (let ((x-len (* *positive-x-axis-len* 2))
        (y-len (* *positive-y-axis-len* 2))
        (xy-arr (read-line-arr coord-point-list-filename)))
    (if (> x-len y-len)     
        (defvar coord-point-list (make-array `(,y-len) :adjustable t))
        (defvar coord-point-list (make-array `(,x-len) :adjustable t)))
    (dotimes (i (length xy-arr))
      (setf (aref coord-point-list i) (line->xy-arr (aref xy-arr i))))
    coord-point-list))


(defun increment-point-assign (coord-point-list)
  (dotimes (i (length coord-point-list))
    (insert-point (fst (aref coord-point-list i)) (snd (aref coord-point-list i)) "*")
    ))















