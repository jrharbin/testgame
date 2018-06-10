(defpackage :testgame
  (:use :common-lisp :trivial-gamekit))

(in-package :testgame)

(defvar *canvas-width* 800)
(defvar *canvas-height* 500)

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))

(gamekit:register-resource-package :keyword "/home/jharbin/repos/testgame/images/")
(gamekit:define-image :ship "testship.png")

(gamekit:define-image :background "space.png")

(gamekit:defgame hello-gamekit () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Testgame"))  ; window's title

(gamekit:start 'hello-gamekit)

(defvar *current-ship-position* (gamekit:vec2 0 0))

(defun setup-keys ()
  (gamekit:bind-button :left :pressed
		       (lambda () (decf (gamekit:x *current-ship-position*) 10d0)))
  (gamekit:bind-button :right :pressed
		       (lambda () (incf (gamekit:x *current-ship-position*) 10d0))))

(setup-keys)

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second 5))

(defun update-position (position time)
  "Update position vector depending on the time supplied"
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:x position) (+ 400 (* 300 (cos angle)))
          (gamekit:y position) (+ 250 (* 250 (sin angle))))))

(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:draw-image *origin* :background)
  ;;(update-position *current-ship-position* (/ (real-time-seconds) 1))
  (gamekit:draw-image *current-ship-position* :ship))

(defun end ()
  (gamekit:stop))
