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

(defun perp-vec (vec)
  "Create a perpendicular vector by rotating 90 degrees clockwise."
  (vec2 (y vec) (- (x vec))))

(defvar *turn-right* nil)
(defvar *turn-left* nil)

(defmethod act ((app split-shot))
  (let ((new-time (real-time-seconds)))
    (cond (*turn-right*
           (let ((perp (normalize (perp-vec *shot-vel*))))
             (setf *shot-vel* (add (mult 1 perp) *shot-vel*))))
          (*turn-left*
           (let ((perp (normalize (perp-vec *shot-vel*))))
             (setf *shot-vel* (add (mult -1 perp) *shot-vel*)))))
    (setf *shot-pos* (add *shot-pos* (mult (- new-time *last-time*) *shot-vel*)))
    (setf *last-time* new-time)))

(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))

(defmacro add-bindings ((keys state &body body) &rest more-bindings)
  "Setup bindings without as much boiler plate."
  (let ((lambda-sym (gensym "ADD-BINDINGS-")))
    `(progn (let ((,lambda-sym (lambda (key)
                                 (declare (ignorable key))
                                 ,@body)))
              ,@(iter (for k :in (ensure-list keys))
                  (collect `(bind-button
                             ,k ,state
                             (lambda () (funcall ,lambda-sym ,k))))))
            ,@(if more-bindings
                  `((add-bindings ,@more-bindings))
                  nil))))

(defun key-index (key)
  (case key
    (:q 0)
    (:w 1)
    (:e 2)
    (:r 3)
    (:t 4)
    (:y 5)
    (:u 6)
    (:i 7)
    (:o 8)
    (:p 9)))

(defun handle-split (key-index)
  (cond ((= key-index 0)
         (setf *turn-right* t))
        ((= key-index 9)
         (setf *turn-left* t))
        (t
         ;; pass
         )))

(defun key-release (key-index)
  (cond ((= key-index 0)
         (setf *turn-right* nil))
        ((= key-index 9)
         (setf *turn-left* nil))
        (t
         ;; pass
         )))

(defmethod post-initialize ((this split-shot))
  ;; Initialize world state
  (setf *last-time* (real-time-seconds))
  (setf *shot-pos* *cannon-pos*)
  (setf *shot-vel* (vec2 0 0))
  (setf *turn-right* nil)
  (setf *turn-left* nil)

  ;; Setup bindings
  (add-bindings
      (:escape :pressed
        (stop))
      ((:q :w :e :r :t :y :u :i :o :p) :pressed
       (handle-split (key-index key)))
      ((:q :w :e :r :t :y :u :i :o :p) :released
       (key-release (key-index key)))
      (:space :pressed
              (setf *shot-vel* (vec2 0 *initial-vel*)))))

(defmethod draw ((this split-shot))
  (draw-rect (add *shot-pos* *origin*)
             10 10 :fill-paint *black* :rounding 5.0))
