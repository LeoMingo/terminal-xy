(defpackage :terminal-xy
  (:use :cl
        :cl-utils
        :gblock
        :axis-utils
        :increment-point-assign))


;;;;main;;;;

#||
(defun l () 
  (load "cl-utils") (load "gblock") (load "axis-utils") (load "increment-point-assign"))
||#

(defun test-run () 
  (gb-map-init) 
  (draw-axis) 
  (defvar *cpl* (make-coord-point-list +coord-point-list-filename+)) 
  (increment-point-assign *cpl*) 
  (gb-map-printer))



;(gb-map-init)
;(draw-axis)
;(axis-x-output)

;(let ((cpl (make-coord-point-list)))
;    (increment-point-assign cpl))

;(gb-map-printer)




















