;; New solution, fingers crossed, for day 2
(defun load-data (file)
  (let ((data (aoc:read-data-file file)))
    (mapcar #'(lambda (ranges)
                (mapcar #'(lambda (number)
                            (parse-integer number))
                        (uiop/utility:split-string ranges :separator "-")))
            (uiop/utility:split-string (first data) :separator ","))))

(defun is-silly-number-p (n)
  (let* ((num (format nil "~d" n))
         (mid-point (/ (length num) 2)))
    (and (evenp (length num))
         (string= (subseq num 0 mid-point) (subseq num mid-point)))))

(defun silly-numbers-in-range (start end)
  (loop for num from start to end
        when (is-silly-number-p num)
          sum num))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (loop for (start end) in data
          sum (silly-numbers-in-range start end))))
