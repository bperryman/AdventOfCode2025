(defun parse-entry (str)
  (let ((mult (if (char= #\L (char str 0)) -1 1)))
    (* mult (parse-integer str :start 1))))

(defun load-data (file)
  "Load the data file ready for use"
  (aoc:read-data-file file :line-processor #'parse-entry))

(defun move-dial (current movement)
  (mod (+ current 100 movement) 100))

(defun solution-1 (file)
  (let ((data (load-data file))
        (dial-setting 50)
        (zeros 0))
    (do ((movements data (cdr movements)))
        ((null movements) zeros)
        (setf dial-setting (move-dial dial-setting (car movements)))
        (when (zerop dial-setting)
          (incf zeros)))))

(defun crosses-zero-times (current movement)
  (let ((raw-movement (+ current movement)))
    (and (not (zerop current))
         (or (<= raw-movement 0)
             (>= raw-movement 100)
             (zerop (move-dial current movement))))))

(defun rotations-and-excess (movement)
  "Returns the number of rotations and the partial rotation"
  (multiple-value-bind (rotations excess) (floor (abs movement) 100)
    (values rotations (* (signum movement) excess))))

(defun crosses-zero-count (current movement)
  (multiple-value-bind (rotations excess) (rotations-and-excess movement)
    (let ((raw-movement (+ current excess)))
      (+ rotations
         (if (and (not (zerop current))
                  (or (<= raw-movement 0)
                      (>= raw-movement 100)
                      (zerop (move-dial current movement))))
             1
             0)))))

(defun solution-2 (file)
  (let ((data (load-data file))
        (dial-setting 50)
        (zeros 0))
    (do ((movements data (cdr movements)))
        ((null movements) zeros)
        (incf zeros (crosses-zero-count dial-setting (car movements)))
        (setf dial-setting (move-dial dial-setting (car movements))))))
