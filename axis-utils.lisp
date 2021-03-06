(defpackage :axis-utils
  (:use gblock)
  (:export :draw-axis
           :big-char-insert
          
    ;       :zoom-measure-setter
    ))




(defconstant sp " ") ; space

(defvar *empty-big-char* 
`((,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  ))

(defvar *big-a* 
`((,sp ,sp ,sp "^" ,sp ,sp ,sp)
  (,sp ,sp "/" ,sp "\\" ,sp ,sp)
  (,sp "|" ,sp ,sp ,sp "|" ,sp)
  (,sp "/" ,sp ,sp ,sp "\\" ,sp)
  (,sp "|" "=" "=" "=" "|" ,sp)
  (,sp "/" ,sp ,sp ,sp "\\" ,sp)
  ("/" ,sp ,sp ,sp ,sp ,sp "\\")))

(defvar *big-x*
`(("|" ,sp ,sp ,sp ,sp ,sp "|")
  ("|" "\\" ,sp ,sp ,sp "/" "|")
  (,sp ,sp "\\" ,sp "/" ,sp ,sp)
  (,sp ,sp ,sp "X" ,sp ,sp ,sp)
  (,sp ,sp "/" ,sp "\\" ,sp ,sp)
  ("|" "/" ,sp ,sp ,sp "\\" "|")
  ("|" ,sp ,sp ,sp ,sp ,sp "|")))

(defvar *big-y*
`(("|" ,sp ,sp ,sp ,sp ,sp "|")
  (,sp "\\" ,sp ,sp ,sp "/" ,sp)
  (,sp ,sp "-" "|" "-" ,sp ,sp)
  (,sp ,sp ,sp "|" ,sp ,sp ,sp)
  (,sp ,sp ,sp "|" ,sp ,sp ,sp)
  (,sp ,sp ,sp "|" ,sp ,sp ,sp)
  (,sp ,sp ,sp "|" ,sp ,sp ,sp)))

(defvar *big-greater-than*
`(("_" "_" ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp "\\" "=" "=" ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp "\\" ,sp)
  (,sp ,sp ,sp ,sp ,sp ,sp ">")
  (,sp ,sp ,sp ,sp ,sp "/" ,sp)
  (,sp ,sp "/" "=" "=" ,sp ,sp)
  ("-" "-" ,sp ,sp ,sp ,sp ,sp)))

(defvar *big-carrot-accent*
`((,sp ,sp ,sp "^" ,sp ,sp ,sp)
  (,sp ,sp "/" ,sp "\\" ,sp ,sp)
  (,sp "|" ,sp ,sp ,sp "|" ,sp)
  (,sp "|" ,sp ,sp ,sp "|" ,sp)
  (,sp "/" ,sp ,sp ,sp "\\" ,sp)
  ("|" ,sp ,sp ,sp ,sp ,sp "|")
  ("|" ,sp ,sp ,sp ,sp ,sp "|")))

(defvar *x-axis-arrow-head1* 
`((,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  ("\\" ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp "\\" ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp "\\" ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp "\\" ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp "\\" ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp "\\" ,sp)
  ))

(defvar *x-axis-arrow-head2* 
`(("-" "-" "-" "-" "-" "-" ">")
  (,sp ,sp ,sp ,sp ,sp "/" ,sp)
  (,sp ,sp ,sp ,sp "/" ,sp ,sp)
  (,sp ,sp ,sp "/" ,sp ,sp ,sp)
  (,sp ,sp "/" ,sp ,sp ,sp ,sp)
  (,sp "/" ,sp ,sp ,sp ,sp ,sp)
  ("/" ,sp ,sp ,sp ,sp ,sp ,sp)
  ))

(defvar *y-axis-arrow-head1* 
`((,sp ,sp ,sp ,sp ,sp ,sp "^")
  (,sp ,sp ,sp ,sp ,sp "/" ,"|")
  (,sp ,sp ,sp ,sp "/" ,sp ,"|")
  (,sp ,sp ,sp "/" ,sp ,sp ,"|")
  (,sp ,sp "/" ,sp ,sp ,sp ,"|")
  (,sp "/" ,sp ,sp ,sp ,sp ,"|")
  ("/" ,sp ,sp ,sp ,sp ,sp ,"|")
  ))
(defvar *y-axis-arrow-head2*
`((,sp ,sp ,sp ,sp ,sp ,sp ,sp)
  ("\\" ,sp ,sp ,sp ,sp ,sp ,sp)
  (,sp "\\" ,sp ,sp ,sp ,sp ,sp)
  (,sp ,sp "\\" ,sp ,sp ,sp ,sp)
  (,sp ,sp ,sp "\\" ,sp ,sp ,sp)
  (,sp ,sp ,sp ,sp "\\" ,sp ,sp)
  (,sp ,sp ,sp ,sp ,sp "\\" ,sp)
  ))


#||
(defun big-char-in,sert (big-char bigX bigY)
  (setf (aref gb-map bigX bigY) big-char))
||#
;(defmacro big-char-insert (big-char gm-v gm-h)
;  `(setf (aref *gb-map* gm-v gm-h) ,big-char))

(defun big-char-insert (big-char gm-v gm-h)
  (let ((bc2Darray (make-array `(,+gb-vert-len+ ,+gb-horiz-len+) 
                               :adjustable t
                               :initial-contents big-char)))
  (setf (aref *gb-map* gm-v gm-h) bc2Darray))
  nil)

(defun insert-arrow-head (xy-op)
  (cond ((equal xy-op "y")
         (big-char-insert *y-axis-arrow-head1* 0 (- (/ +gm-horiz-len+ 2) 1))
         (big-char-insert *y-axis-arrow-head2* 0 (/ +gm-horiz-len+ 2)))        
        ((equal xy-op "x")
         (big-char-insert *x-axis-arrow-head1* (- (/ +gm-vert-len+ 2) 1) (- +gm-horiz-len+ 1))
         (big-char-insert *x-axis-arrow-head2* (/ +gm-vert-len+ 2) (- +gm-horiz-len+ 1)))        
        (t nil)))


(defun x-axis-drawer ()
  (dotimes (x *x-axis-len*)
    (insert-point (- x *positive-x-axis-len*) 0 "-"))
  (insert-arrow-head "x"))

(defun y-axis-drawer ()
  (dotimes (y *x-axis-len*)
    (insert-point 0 (- y *positive-y-axis-len*) "|"))
  (insert-arrow-head "y"))


(defconstant +origin-character+ "+")
(defun origin-drawer ()
  (insert-point 0 0 +origin-character+))

(defun draw-axis ()
  (x-axis-drawer)
  (y-axis-drawer)
  (origin-drawer))
















