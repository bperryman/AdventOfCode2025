;; Day 4
(defun load-data (file)
  (aoc:read-data-file file :dataset-processor #'aoc:process-dataset-to-matrix))

(defun contains-roll-p (map position)
  (char= #\@ (aoc:data-at map position)))

(defun rolls-surrounding (map position)
  (let* ((size (aoc:dataset-size map))
         (points (aoc:surrounding-points position
                                         :diagonals t
                                         :min-point #@(0 0)
                                         :max-point (aoc:create-point (1- (aoc:point-x size))
                                                                      (1- (aoc:point-y size))))))
    (loop for pt in points
          count (contains-roll-p map pt))))

(defun solution-1 (file)
  (let* ((data (load-data file))
         (count 0))
    (aoc:do-all-points #'(lambda (pt)
                           (when (contains-roll-p data pt)
                             (let ((surrounding (rolls-surrounding data pt)))
                               (when (< surrounding 4)
                                 ; (format t "Only ~d rolls surrounding point ~a~%" surrounding pt)
                                 (incf count)
                                 ))))
      data)
    count))

(defun clear-rolls (data)
  (let ((count 0)
        (moved-rolls ()))
    (aoc:do-all-points #'(lambda (pt)
                           (when (contains-roll-p data pt)
                             (let ((surrounding (rolls-surrounding data pt)))
                               (when (< surrounding 4)
                                 (incf count)
                                 (push pt moved-rolls)))))
      data)
    (dolist (pt moved-rolls count)
      (setf (aoc:data-at data pt) #\.))))

(defun solution-2 (file)
  (let* ((data (load-data file))
         (count 0))
    (do ((cleared (clear-rolls data) (clear-rolls data)))
        ((= cleared 0) count)
      (incf count cleared))))
