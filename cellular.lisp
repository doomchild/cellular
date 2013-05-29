(ql:quickload "lispbuilder-sdl")

(defparameter *buffer-x* 30)
(defparameter *buffer-y* 30)
(defparameter *rect-w* 20)
(defparameter *rect-h* 20)

(defparameter *main-buffer* (make-array (* *buffer-x* *buffer-y*) :initial-element 0 :element-type 'integer))
(defparameter *secondary-buffer* (make-array (* *buffer-x* *buffer-y*) :initial-element 0 :element-type 'integer))

(defparameter *run-simulation* t)

(defun element-at (buf x y)
  (aref buf (+ (* x *buffer-y*) y)))

(defun (setf element-at) (value buf x y)
  (setf (aref buf (+ (* x *buffer-y*) y)) value))

(setf (element-at *main-buffer* 10 10) 1
      (element-at *main-buffer* 11 10) 1
      (element-at *main-buffer* 12 10) 1
      (element-at *main-buffer* 13 10) 1
      (element-at *main-buffer* 14 10) 1
      (element-at *main-buffer* 15 10) 1)

(defun get-neighbor-coords (x y)
  (let ((mx (1- x)) (my (1- y)) (px (1+ x)) (py (1+ y)))
    (if (< mx 0) (setf mx (1- *buffer-x*)))
    (if (< my 0) (setf my (1- *buffer-y*)))
    (if (>= px *buffer-x*) (setf px 0))
    (if (>= py *buffer-y*) (setf py 0))

    (list (cons mx y)
	  (cons mx py)
	  (cons x py)
	  (cons px py)
	  (cons px y)
	  (cons px my)
	  (cons x my)
	  (cons mx my))))

(defun get-neighbor-values (buffer x y)
  (let ((neighbor-coords (get-neighbor-coords x y)))
    (loop for coord in neighbor-coords collect (element-at buffer (car coord) (cdr coord)))))

(defun process-cells (fb tb)
  (loop for y upto (1- *buffer-y*) do
       (loop for x upto (1- *buffer-x*) do
	    (let* ((t-neighbors (count 1 (get-neighbor-values fb x y))))
	      (cond
		((< t-neighbors 2) (setf (element-at tb x y) 0))
		((> t-neighbors 3) (setf (element-at tb x y) 0))
		((= t-neighbors 3) (setf (element-at tb x y) 1))
		(t (setf (element-at tb x y) (element-at fb x y))))))))

(defun run ()
  (sdl:with-init ()
    (sdl:window 600 600 :fps (make-instance 'sdl:fps-timestep :dt 500))
    
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (when (sdl:key= key :sdl-key-escape)
			 (sdl:push-quit-event))
		       (when (sdl:key= key :sdl-key-return)
			 (if (eq *run-simulation* t)
			     (setf *run-simulation* nil)
			     (setf *run-simulation* t))))
      (:video-expose-event () (sdl:update-display))
      (:idle ()
	     (sdl:clear-display sdl:*black*)

	     (sdl:with-timestep
	       (when (eq *run-simulation* t)
		 (progn
		   (process-cells *main-buffer* *secondary-buffer*)
		   (rotatef *main-buffer* *secondary-buffer*))))

	     (draw-buffer *main-buffer*)

	     (sdl:update-display)))))

(defun draw-buffer (buffer)
  (let ((r (sdl:rectangle :x 0 :y 0 :w *rect-w* :h *rect-h*)))
    (loop for y upto (1- *buffer-y*) do
	 (loop for x upto (1- *buffer-x*) do
	      (sdl:set-rectangle-* r :x (* x *rect-w*) :y (* y *rect-h*))
	      (if (> (element-at buffer x y) 0)
		  (sdl:draw-box r :color sdl:*black*)
		  (sdl:draw-box r :color sdl:*white* :stroke-color sdl:*black*))))))
