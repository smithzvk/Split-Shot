
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
(defvar *red* (vec4 1 0 0 1))
(defvar *green* (vec4 0 1 0 1))

(defvar *origin* (vec2 0 0))

(defvar *cannon-pos* (vec2 (floor *width* 2) 0))
(defvar *cannon-rotation* 0.0)
(defvar *cannon-turn-right* nil)
(defvar *cannon-turn-left* nil)

(defvar *shot-pos* *cannon-pos*)

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

(defvar *live-targets* ()
  "A list of targets that need to be hit to win the level.")
(defvar *hit-targets* ()
  "A list of targets that have already been hit.")

(defun dot-product (va vb)
  (rtg-math.vector2:dot
   (bodge-math::value-of va)
   (bodge-math::value-of vb)))

(defun distance-squared (va vb)
  (rtg-math.vector2:distance-squared
   (bodge-math::value-of va)
   (bodge-math::value-of vb)))

(defun length-squared (v)
  (rtg-math.vector2:length (bodge-math::value-of v)))

(defun perp-vec (vec)
  "Create a perpendicular vector by rotating 90 degrees clockwise."
  (vec2 (y vec) (- (x vec))))

(defun line-collision (pos line margin)
  "Test if point, POS, is within collision distance, MARGIN, of LINE specified
as a list containing a starting point, a unit direction vector, and a length.
This is a polling collision detector, so MARGIN should be chosen such that it is
larger you ever expect to step in a given frame time step.

If there is an collision, return the perpendicular distance to the line.
Positive is to the right of the line as you stand at its starting point and look
down its direction.

Note that this isn't a sphero-cylinder style collision test.  Ends of lines are
not rounded and the margin doesn't apply to distances past the end of the line."
  (destructuring-bind (start direction length) line
    (let* ((diff (gamekit:subt pos start))
           (a (dot-product diff direction)))
      (when (<= 0 a length)
        (let* ((perp (perp-vec direction))
               (p (dot-product diff perp)))
          (when (<= (- margin) p margin)
            p))))))

(defun target-collision (pos target rad)
  (< (distance-squared pos target)
     (* rad rad)))

(defparameter *boundary*
  (list (list (vec2 0 0)              (vec2  0 +1) *height*)
        (list (vec2 0 *height*)       (vec2 +1  0) *width*)
        (list (vec2 *width* *height*) (vec2  0 -1) *height*)
        (list (vec2 *width* 0)        (vec2 -1  0) *width*)))

(defparameter *levels*
  (list
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (/ *width* 2) *height*))
    :walls ())
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (/ (* 1 *width*) 3) *height*)
                   (vec2 (/ (* 2 *width*) 3) *height*))
    :walls ())))

(defvar *level* nil
  "The current level")

(defvar *level-number* 0
  "The current level number")

(defun real-time-seconds ()
  "Return seconds since certain point of time."
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))

(defvar *last-time* (real-time-seconds)
  "Denotes timestamp of the last model frame.")

(defmethod act ((app split-shot))
  (let* ((new-time (real-time-seconds))
         (dt (- new-time *last-time*))
         (removal-list ()))

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
      (setf (shot-pos shot) (add (shot-pos shot) (mult dt (shot-vel shot))))

      ;; Detect collisions
      (when (or
             ;; Detect out of bounds
             (iter (for wall :in *boundary*)
               (for collision := (line-collision (shot-pos shot) wall 500))
               (finding wall :such-that (and collision (< collision 0))))
             ;; Detect collisions with walls
             (iter (for wall :in (getf *level* :walls))
               (for collision := (line-collision (shot-pos shot) wall 5))
               (finding wall :such-that collision)))
        (push shot removal-list)))

    (iter (for shot :in removal-list)
      (setf *shots* (remove shot *shots*))

      (iter (for i :from (shot-index shot) :below (length *shot-state*))
        (if (= 0 (aref *shot-state* i))
            (finish)
            (setf (aref *shot-state* i) 0)))

      (let ((hit-targets ()))
        (iter (for target :in *live-targets*)
          (when (target-collision (shot-pos shot) target 10)
            (push target hit-targets)))
        (iter (for target :in hit-targets)
          (push target *hit-targets*)
          (setf *live-targets* (remove target *live-targets*)))))

    (unless *live-targets*
      (format t "~%Level Complete!")
      (incf *level-number*)
      (init-level (nth *level-number* *levels*)))

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

(defun find-shot (key)
  "Determine which shot the given key applies to."
  (let ((shots (cons nil *shots*))
        (in-shot nil)
        (interior nil))
    (values
     (iter (for val :in-vector *shot-state* :with-index i)
       (cond ((and (not in-shot) (> val 0))
              (setf in-shot t)
              (pop shots))
             (in-shot
              (cond ((= val 0)
                     (setf in-shot nil
                           interior nil))
                    ((and (< (+ i 1) (length *shot-state*))
                          (> (aref *shot-state* (+ i 1)) 0))
                     (setf interior t))
                    (t
                     (setf interior nil)))))
       (when (> i key) (finish))
       (finding (first shots) :such-that (= i key)))
     interior)))

(defun handle-split (shot i)
  (let ((pos (position shot *shots* :test 'eq))
        (perp (normalize (perp-vec (shot-vel shot))))
        (left-shot (make-shot :pos (shot-pos shot)
                              :index (shot-index shot)))
        (right-shot (make-shot :pos (shot-pos shot)
                               :index (+ i 1))))
    ;; First split the shot
    (setf (aref *shot-state* i) 0)

    ;; Update the velocity given the new shots
    (setf (shot-vel left-shot)
          (add (mult (/ -4000 (shot-mass left-shot)) perp) (shot-vel shot)))
    (setf (shot-vel right-shot)
          (add (mult (/ 4000 (shot-mass right-shot)) perp) (shot-vel shot)))
    (setf *shots* (concatenate
                   'list
                   (subseq *shots* 0 pos)
                   (list left-shot right-shot)
                   (subseq *shots* (+ pos 1))))))

(defun init-level (level)
  (setf *level* level)

  (setf *shot-state* (make-array 10 :initial-element 100))
  (setf *shots* nil)

  (setf *live-targets* (getf *level* :targets))
  (setf *hit-targets* ())

  (destructuring-bind (pos min max) (getf *level* :cannon)
    (setf *cannon-pos* pos)
    (setf *cannon-rotation* 0.0)
    (setf *cannon-turn-right* nil)
    (setf *cannon-turn-left* nil)))

(defmethod post-initialize ((this split-shot))
  ;; Initialize world state
  (setf *last-time* (real-time-seconds))

  (init-level (nth *level-number* *levels*))

  ;; Setup bindings
  (add-bindings
      (:escape :pressed
        (stop))
      ((:q :w :e :r :t :y :u :i :o :p) :pressed
       (multiple-value-bind (shot interior) (find-shot (key-index key))
         (if (and shot interior)
             ;; If the key is in the interior of a shot, then perform a split
             (handle-split shot (key-index key))
             (setf (aref *key-pressed* (key-index key)) t))))
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

(defun draw-wall (direction length)
  (draw-line *origin* (mult length direction) *white* :thickness 5.0))

(defun draw-target (color)
  (draw-circle *origin* 15 :fill-paint color))

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
    (draw-cannon))
  (iter (for target :in *live-targets*)
    (with-pushed-canvas ()
      (translate-canvas (x target) (y target))
      (draw-target *red*)))
  (iter (for target :in *hit-targets*)
    (with-pushed-canvas ()
      (translate-canvas (x target) (y target))
      (draw-target *green*)))
  (iter (for (start direction length)
             :in (append *boundary* (getf *level* :walls)))
    (with-pushed-canvas ()
      (translate-canvas (x start) (y start))
      (draw-wall direction length))))
