(defpackage :split-shot
  (:use :cl :iterate :gamekit))

(in-package :split-shot)

(defgame split-shot () ())

(defvar *black* (vec4 0 0 0 1))
(defvar *origin* (vec2 0 0))

(defmethod draw ((this split-shot))
  (draw-rect *origin* 100 100 :fill-paint *black* :rounding 10.0))
