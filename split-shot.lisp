
(in-package :split-shot)

(defvar *width* 800)
(defvar *height* 600)

(defgame split-shot ()
  ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "SplitShot"))

(defvar *black* (vec4 0 0 0 1))
(defvar *white* (vec4 1 1 1 1))
(defvar *origin* (vec2 0 0))

(defvar *cannon-pos* (vec2 (floor *width* 2) 0))
(defvar *cannon-rotation* 0.0)
(defvar *cannon-turn-right* nil)
(defvar *cannon-turn-left* nil)

(defvar *shot-pos* *cannon-pos*)

(defvar *turn-right* nil)
(defvar *turn-left* nil)

(defvar *initial-vel* 50)

(defvar *shot-vel* (vec2 0 30))

(defvar *shot-state* (make-array 10 :initial-element 100)
  "This holds the global state of all of your shots.")

(defvar *key-pressed* (make-array 10 :initial-element nil)
  "Holds the current pressed state of the shot controlling keys.")

(defstruct shot pos vel index)

(defun shot-mass (shot)
  (iter (for i :from (shot-index shot) :below (length *shot-state*))
    (while (> (aref *shot-state* i) 0))
    (summing (aref *shot-state* i))))

(defvar *shots* ()
  "A list of current shots.")

(defstruct target pos)

(defvar *targets* ()
  "A list of targets that need to be hit to win the level.")

(defun real-time-seconds ()
  "Return seconds since certain point of time."
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))

(defvar *last-time* (real-time-seconds)
  "Denotes timestamp of the last model frame.")

(defun perp-vec (vec)
  "Create a perpendicular vector by rotating 90 degrees clockwise."
  (vec2 (y vec) (- (x vec))))

(defmethod act ((app split-shot))
  (let* ((new-time (real-time-seconds))
         (dt (- new-time *last-time*)))

    (when *cannon-turn-left* (incf *cannon-rotation* 0.1))
    (when *cannon-turn-right* (decf *cannon-rotation* 0.1))

    ;; Handle shot motion
    (iter (for shot :in *shots*)
      (when (and (aref *key-pressed* 0)
                 (> (aref *shot-state* 0) 0d0))
        (let ((perp (normalize (perp-vec (shot-vel shot)))))
          (setf (shot-vel shot) (add (mult (* 30 dt) perp)
                                     (shot-vel shot)))
          (setf (aref *shot-state* 0) (max 0d0 (- (aref *shot-state* 0) (* 30 dt))))))

      (when (and (aref *key-pressed* 9)
                 (> (aref *shot-state* 9) 0d0))
        (let ((perp (normalize (perp-vec (shot-vel shot)))))
          (setf (shot-vel shot) (add (mult (* -30 dt) perp) (shot-vel shot)))
          (setf (aref *shot-state* 9) (max 0d0 (- (aref *shot-state* 9) (* 30 dt))))))

      ;; Integrate motion
      (setf (shot-pos shot) (add (shot-pos shot) (mult dt (shot-vel shot)))))

    (setf *last-time* new-time)))

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

(defmethod post-initialize ((this split-shot))
  ;; Initialize world state
  (setf *last-time* (real-time-seconds))

  (setf *shot-state* (make-array 10 :initial-element 100))
  (setf *shots* nil)

  (setf *turn-right* nil)
  (setf *turn-left* nil)

  (setf *cannon-rotation* 0.0)
  (setf *cannon-turn-right* nil)
  (setf *cannon-turn-left* nil)

  ;; Setup bindings
  (add-bindings
      (:escape :pressed
        (stop))
      ((:q :w :e :r :t :y :u :i :o :p) :pressed
       (setf (aref *key-pressed* (key-index key)) t))
      ((:q :w :e :r :t :y :u :i :o :p) :released
       (setf (aref *key-pressed* (key-index key)) nil))

      (:a :pressed (setf *cannon-turn-left* t))
      (:a :released (setf *cannon-turn-left* nil))
      (:d :pressed (setf *cannon-turn-right* t))
      (:d :released (setf *cannon-turn-right* nil))

      (:space :pressed
              ;; Like a reset, for now
              (setf *shots*
                    (list (make-shot :pos *cannon-pos*
                                     :vel (vec2 (* *initial-vel*
                                                   (- (sin *cannon-rotation*)))
                                                (* *initial-vel*
                                                   (cos *cannon-rotation*)))
                                     :index 0))))))

(defun draw-shot ()
  (let ((w 10) (h 5))
    (draw-rect (vec2 (/ w -2) (/ h -2))
               w h :fill-paint *white* :rounding 5.0)))

(defun draw-cannon ()
  (with-pushed-canvas ()
    (let ((w 7) (h 30))
      (draw-circle *origin* 15 :fill-paint *white*)
      (rotate-canvas 0)
      (draw-rect (vec2 (/ w -2) 0) w h :fill-paint *white*))))

(defmethod draw ((this split-shot))
  (draw-rect *origin*
             *width* *height* :fill-paint *black*)
  (iter (for shot :in *shots*)
    (with-pushed-canvas ()
      (translate-canvas (x (shot-pos shot))
                        (y (shot-pos shot)))
      (rotate-canvas (atan (y (shot-vel shot))
                           (x (shot-vel shot))))
      (draw-shot)))
  (with-pushed-canvas ()
    (translate-canvas (x *cannon-pos*) (y *cannon-pos*))
    (rotate-canvas *cannon-rotation*)
    (draw-cannon)))
