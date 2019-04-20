
(in-package :split-shot)

(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))
