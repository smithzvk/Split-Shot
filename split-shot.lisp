
(in-package :split-shot)

(defvar *width* 800)
(defvar *arena-height* 500)
(defvar *height* 600)

(defvar *keys*
  '(:1 :q :2 :w :3 :e :4 :r :5 :t :6 :y :7 :u :8 :i :9 :o :0))

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
(defvar *shot-fired* nil)


(defvar *paused* nil
  "Indicates when we are in a paused state.  In the paused state you can rewind
  the game state.")
(defvar *history* nil
  "This holds the history of the game state for the given level.  This allows
  for rewinding the game state.")
(defvar *future* nil
  "This holds the saved future of the game state for the given level.  This allows
  for moving forward in the game state history.")

(defvar *shot-state* (make-array 19 :initial-element 100)
  "This holds the global state of all of your shots.")

(defvar *key-pressed* (make-array 19 :initial-element nil)
  "Holds the current pressed state of the shot controlling keys.")

(defparameter *fast-forward* 1.0)

(defstruct shot pos vel ilo ihi)

(defun shot-mass (shot)
  (iter (for i :from (shot-ilo shot) :to (shot-ihi shot))
    (summing (aref *shot-state* i))))

(defun find-shot (index)
  (let ((shot (iter (for shot :in *shots*)
                (finding shot :such-that (<= (shot-ilo shot)
                                             index
                                             (shot-ihi shot))))))
    (if shot
        (values shot (< (shot-ilo shot)
                        index
                        (shot-ihi shot)))
        nil)))

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
  (list (list (vec2 0 0)                    (vec2  0 +1) *arena-height*)
        (list (vec2 0 *arena-height*)       (vec2 +1  0) *width*)
        (list (vec2 *width* *arena-height*) (vec2  0 -1) *arena-height*)
        (list (vec2 *width* 0)              (vec2 -1  0) *width*)))

(defparameter *levels*
  (list
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (/ *width* 2) *arena-height*))
    :walls ())
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (/ *width* 2) *arena-height*))
    :walls (list
            (list (vec2 (* 0.75 *width*) (/ *arena-height* 2))
                  (vec2 -1 0)
                  (* 0.5 *width*))))
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (* 0.75 *width*) *arena-height*)
                   (vec2 (* 0.25 *width*) *arena-height*))
    :walls ())
   (list
    :cannon (list (vec2 (/ *width* 2) 0) (- (/ pi 2)) (/ pi 2))
    :targets (list (vec2 (* 0.75 *width*) 0)
                   (vec2 (* 0.25 *width*) 0))
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
         (dt (* (- new-time *last-time*)
                *fast-forward*))
         (removal-list ()))
    (cond
      (*paused*
       (when (and *cannon-turn-left* *history*)
         (let ((state (pop *history*)))
           (push state *future*)
           (destructuring-bind (shots shot-state) state
             (setf *shots* shots)
             (setf *shot-state* shot-state))))
       (when (and *cannon-turn-right* *future*)
         (let ((state (pop *future*)))
           (push state *history*)
           (destructuring-bind (shots shot-state) state
             (setf *shots* shots)
             (setf *shot-state* shot-state)))))
      (t

       ;; First save the current state and clear the future
       (when *shots*
         (push (list (mapcar #'copy-shot *shots*) (copy-seq *shot-state*))
               *history*)
         (setf *future* nil))

       (when *cannon-turn-left* (incf *cannon-rotation* 0.1))
       (when *cannon-turn-right* (decf *cannon-rotation* 0.1))

       ;; Steering of shots
       (iter (for key :in-vector *key-pressed* :with-index i)
             (when key
               (multiple-value-bind (shot interior) (find-shot i)
                 (when (and shot (not interior))
                   (let ((perp (normalize (perp-vec (shot-vel shot)))))
                     (cond ((= i (shot-ilo shot))
                            (setf (shot-vel shot)
                                  (add (mult (/ (* 18000 dt) (shot-mass shot)) perp)
                                       (shot-vel shot)))
                            (setf (aref *shot-state* (shot-ilo shot))
                                  (max 0 (- (aref *shot-state* (shot-ilo shot))
                                            (* 30 dt)))))
                           ((= i (shot-ihi shot))
                            (setf (shot-vel shot)
                                  (add (mult (/ (* -18000 dt) (shot-mass shot)) perp)
                                       (shot-vel shot)))
                            (setf (aref *shot-state* (shot-ihi shot))
                                  (max 0 (- (aref *shot-state* (shot-ihi shot))
                                            (* 30 dt)))))))
                   (when (= 0 (aref *shot-state* (shot-ilo shot)))
                     (incf (shot-ilo shot)))
                   (when (= 0 (aref *shot-state* (shot-ihi shot)))
                     (decf (shot-ihi shot)))
                   (when (< (shot-ihi shot) (shot-ilo shot))
                     (push shot removal-list))))))

       ;; Handle shot motion
       (iter (for shot :in *shots*)

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

             (iter (for i :from (shot-ilo shot) :below (length *shot-state*))
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

       (unless (iter (for val :in-vector *shot-state*)
                     (thereis (> val 0)))
         (cond (*live-targets*
                (format t "~%Try again!")
                (init-level (nth (mod *level-number* (length *levels*))
                                 *levels*)))
               (t (format t "~%Level Complete!")
                  (incf *level-number*)
                  (init-level (nth (mod *level-number* (length *levels*))
                                   *levels*)))))))

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
  (position key *keys*))

(defun handle-split (shot i)
  (let ((pos (position shot *shots* :test 'eq))
        (perp (normalize (perp-vec (shot-vel shot))))
        (left-shot (make-shot :pos (shot-pos shot)
                              :ilo (shot-ilo shot)
                              :ihi (- i 1)))
        (right-shot (make-shot :pos (shot-pos shot)
                               :ilo (+ i 1)
                               :ihi (shot-ihi shot))))
    ;; First split the shot
    (setf (aref *shot-state* i) 0)

    ;; Update the velocity given the new shots
    (setf (shot-vel left-shot)
          (add (mult (/ -6000 (shot-mass left-shot)) perp) (shot-vel shot)))
    (setf (shot-vel right-shot)
          (add (mult (/ 6000 (shot-mass right-shot)) perp) (shot-vel shot)))
    (setf *shots* (concatenate
                   'list
                   (subseq *shots* 0 pos)
                   (list left-shot right-shot)
                   (subseq *shots* (+ pos 1))))))

(defun init-level (level)
  (unless level (stop))

  (setf *level* level)

  (setf *key-pressed* (make-array (length *keys*)
                                  :initial-element nil))

  (setf *shot-state* (make-array (length *keys*)
                                 :initial-element 100))
  (setf *shots* nil)
  (setf *shot-fired* nil)
  (setf *paused* nil)
  (setf *history* nil)
  (setf *future* nil)

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
  (setf *level-number* 0)

  (init-level (nth (mod *level-number* (length *levels*)) *levels*))

  ;; Setup bindings
  (add-bindings
      (:escape :pressed
        (stop))
      ((:1 :q :2 :w :3 :e :4 :r :5 :t :6 :y :7 :u :8 :i :9 :o :0)
       :pressed
       (multiple-value-bind (shot interior) (find-shot (key-index key))
         (if (and shot interior)
             ;; If the key is in the interior of a shot, then perform a split
             (handle-split shot (key-index key))
             (setf (aref *key-pressed* (key-index key)) t))))
      ((:1 :q :2 :w :3 :e :4 :r :5 :t :6 :y :7 :u :8 :i :9 :o :0)
       :released
       (setf (aref *key-pressed* (key-index key)) nil))

      (:left :pressed (setf *cannon-turn-left* t))
      (:left :released (setf *cannon-turn-left* nil))
      (:right :pressed (setf *cannon-turn-right* t))
      (:right :released (setf *cannon-turn-right* nil))

      (:up :pressed (setf *fast-forward* (* *fast-forward* 2)))
      (:up :released (setf *fast-forward* (/ *fast-forward* 2)))
      (:down :pressed (setf *fast-forward* (/ *fast-forward* 2)))
      (:down :released (setf *fast-forward* (* *fast-forward* 2)))
      (:space :pressed
              (cond ((not *shot-fired*)
                     (setf *shot-fired* t
                           *shots*
                           (list (make-shot
                                  :pos *cannon-pos*
                                  :vel (vec2 (* *initial-vel*
                                                (- (sin *cannon-rotation*)))
                                             (* *initial-vel*
                                                (cos *cannon-rotation*)))
                                  :ilo 0
                                  :ihi (- (length *shot-state*) 1)))))
                    (t (setf *paused* (not *paused*)))))))

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
  ;; Clear screen
  (draw-rect *origin*
             *width* *height* :fill-paint *black*)

  ;; Draw arena
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
      (draw-wall direction length)))

  ;; Draw interface
  (draw-rect (vec2 0 *arena-height*)
             *width* (- *height* *arena-height*)
             :fill-paint *black*)
  (let ((box-width (/ *width* (length *keys*))))
    (iter
      (for key :in *keys*)
      (for idx :from 0)
      (with-pushed-canvas ()
        (translate-canvas (+ 2 (* idx box-width))
                          *arena-height*)
        (draw-rect *origin* (- box-width 4) (aref *shot-state* idx)
                   :fill-paint *black*
                   :stroke-paint *white*)
        (unless *shots*
          (draw-rect *origin* (- box-width 4) (aref *shot-state* idx)
                     :fill-paint *red*))
        (draw-text (string-downcase (symbol-name key)) *origin*)))
    (iter
      (for shot :in *shots*)
      (iter
        (for idx :from (shot-ilo shot) :to (shot-ihi shot))
        (with-pushed-canvas ()
          (translate-canvas (+ 2 (* idx box-width))
                            *arena-height*)
          (draw-rect *origin* (- box-width 4) (aref *shot-state* idx)
                     :fill-paint *red*)
          (draw-text (string-downcase (symbol-name (nth idx *keys*)))
                     *origin*))))))
