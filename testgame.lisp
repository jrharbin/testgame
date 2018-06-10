(defpackage :testgame
  (:use :common-lisp :trivial-gamekit))

(in-package :testgame)

(defvar *canvas-width* 800)
(defvar *canvas-height* 500)
(defvar *origin* (gamekit:vec2 0 0))

(defvar *player-ship-position* (gamekit:vec2 0 0))
(defvar *step-size* 10d0)
(defvar *large-step-size* 30d0)

(gamekit:register-resource-package :keyword "/home/jharbin/repos/testgame/images/")
(gamekit:define-image :ship "testship.png")

(gamekit:define-image :background "space.png")
(gamekit:define-image :enemy "enemy.png")

(gamekit:defgame hello-gamekit () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Testgame"))  ; window's title

(gamekit:start 'hello-gamekit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For enemy arrays
(defclass entity-array ()
  ((status)
   (image :initarg :image)
   (x-width :initarg :x-width :initform 3)
   (y-width :initarg :y-width :initform 3)
   (x-offset :initarg :x-offset :initform 40)
   (y-offset :initarg :y-offset :initform 40)
   (global-offset :initform (gamekit:vec2 0 0))))

(defmethod initialize-instance :after ((e entity-array) &key)
  "Setup the slot array for the different instances"
  (with-slots (status x-width y-width) e
    (setf status (make-array (list x-width y-width)
			     :element-type 'fixnum
			     :initial-element 1))))

(defmethod render ((e entity-array))
  "Render all active entities (status greater than zero) in the array"
  (with-slots (status image global-offset x-width y-width x-offset y-offset) e
    (loop for i from 0 upto (- x-width 1) do
      (loop for j from 0 upto (- y-width 1) do
	(when (> (aref status i j) 0)
	    (gamekit:draw-image (vec2 (+ (gamekit:x global-offset) (* x-offset i))
				      (+ (gamekit:y global-offset) (* y-offset j)))
				image))))))

(defmethod set-entity-status ((e entity-array) x-id y-id &optional (new-elt-status 0))
  (setf (aref (slot-value e 'status) x-id y-id) new-elt-status))

(defvar *enemies* (make-instance 'entity-array :image :enemy :x-width 6 :y-width 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game keys
(defun setup-keys ()
  (gamekit:bind-button :left :pressed
		       (lambda () (decf (gamekit:x *player-ship-position*) *step-size*)))
  (gamekit:bind-button :right :pressed
		       (lambda () (incf (gamekit:x *player-ship-position*) *step-size*)))
  (gamekit:bind-button :left :repeating
		       (lambda () (decf (gamekit:x *player-ship-position*) *large-step-size*)))
  (gamekit:bind-button :right :repeating
		       (lambda () (incf (gamekit:x *player-ship-position*) *large-step-size*))))

(setup-keys)

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second 5))

(defun update-position (position time)
  "Update position vector depending on the time supplied"
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:x position) (+ 200 (* 400 (cos angle))))))

(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:draw-image *origin* :background)
  (render *enemies*)
  (update-position (slot-value *enemies* 'global-offset) (/ (real-time-seconds) 1))
  (gamekit:draw-image *player-ship-position* :ship))

(defun end ()
  (gamekit:stop))
