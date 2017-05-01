(defpackage :terminal-xy
  (:use :cl-utils
        :gblock
        :axis-utils
        :increment-point-assign))


;;;;main;;;;

(gb-map-init)
(draw-axis)
(axis-x-output)

(let ((cpl (make-coord-point-list)))
    (increment-point-assign cpl))

(gb-map-printer)




















