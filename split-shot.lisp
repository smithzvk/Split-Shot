;; (ql:quickload '(:iterate :trivial-gamekit))

(defpackage :split-shot
  (:use :cl :iterate :gamekit))

(in-package :split-shot)

(defgame split-shot () ())

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *origin* (vec2 0 0))

(defvar *cannon-pos* (vec2 (floor *width* 2) 0))

(defvar *shot-pos* *cannon-pos*)

(defvar *initial-vel* 30)

(defvar *shot-vel* (vec2 0 30))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))

(defvar *last-time* (real-time-seconds)
  "Denotes timestamp of the last model frame.")

(defmethod gamekit:act ((app split-shot))
  (let ((new-time (real-time-seconds)))
    (setf *shot-pos* (add *shot-pos* (mult (- new-time *last-time*) *shot-vel*)))
    (setf *last-time* new-time)))

(defmethod post-initialize ((this split-shot))
  ;; Initialize world state
  (setf *last-time* (real-time-seconds))
  (setf *shot-pos* *cannon-pos*)
  (setf *shot-vel* (vec2 0 0))

  ;; Setup bindings
  (bind-button :q :pressed
               (lambda ()
                 (gamekit:stop)))
  (bind-button :space :pressed
               (lambda ()
                 (setf *shot-vel* (vec2 0 *initial-vel*)))))

(defmethod draw ((this split-shot))
  (draw-rect (add *shot-pos* *origin*)
             5 10 :fill-paint *black* :rounding 5.0))
