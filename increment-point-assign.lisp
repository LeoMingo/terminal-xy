(defpackage :increment-point-assign
  (:use :gblock :cl-utils)
  (:export increment-point-assign))


(defvar xy-arr (read-line-arr filename))
(defun line->xy-arr (line)
  (multiple-value-bind (x y) (parse-integer line :junk-allowed t) `#(,x ,y)))

(defun make-coord-point-list ()
;It is pre-assumed the gb-map would be a square, but if not, take the shorter axis length as measure.
  (let ((x-len (* positive-x-axis-len 2))
        (y-len (* positive-y-axis-len 2)))
    (if (> x-len y-len)     
        (defvar coord-point-list (make-array `(,y-len) :adjustable t))
        (defvar coord-point-list (make-array `(,x-len) :adjustable t)))
    (dotimes (i (length xy-arr))
      (setf (aref coord-point-list i) (line->xy-arr (aref xy-arr i))))
    ))


(defun increment-point-assign (coord-point-list)
  (dotimes (i (length coord-point-list))
    (insert-point (fst (aref coord-point-list i)) (snd (aref coord-point-list i)) "*")
    ))




