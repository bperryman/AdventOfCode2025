(defun load-data (file)
  (aoc:read-data-file file))

(defun largest-digit-in-string (str &optional (start 0) (end (1- (length str))))
  "Return the digit and position of the largest digit possible from the string"
  (let ((highest #\0)
        (highest-index start))
    (loop for i from start to end
          when (char> (char str i) highest)
            do (setf highest-index i
                     highest (char str i))
          finally (return (values highest highest-index)))))

(defun largest-n-digit-number (str n)
  (let* ((len (length str))
         (start 0)
         (end (- len n))
         (return-string (make-string n :initial-element #\0)))
    (dotimes (i n (parse-integer return-string))
      (multiple-value-bind (digit index) (largest-digit-in-string str start end)
        (setf (char return-string i) digit
              start (1+ index)
              end (1+ end))))))

(defun solution-1 (file)
  (let ((nums (load-data file)))
    (loop for batteries in nums
          sum (largest-n-digit-number batteries 2))))

(defun solution-2 (file)
  (let ((nums (load-data file)))
    (loop for batteries in nums
          sum (largest-n-digit-number batteries 12))))
