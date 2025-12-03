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

(defun silly-numbers-in-range (start end silly-test)
  (loop for num from start to end
        when (funcall silly-test num)
          sum num))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (loop for (start end) in data
          sum (silly-numbers-in-range start end #'is-silly-number-p))))

(defun number-of-digits (num)
  "Returns the number of digits in the number num."
  (1+ (floor (log num 10))))

(defun is-divisible-by-p (num divisor)
  (integerp (/ num divisor)))

(defun split-string-every-n (str n)
  "Split a string into pieces every n characters. Returns a list of the characters."
  (let ((len (length str)))
    (loop for i from 0 below len by n
          collect (subseq str i (+ i n)))))

(defun split-string-into (str pieces)
  "Split a string into an equal number of pieces."
  (split-string-every-n str (/ (length str) pieces)))

(defun all-strings-equal-p (strings)
  "The list of passed strings are checked for similarity"
  (every #'(lambda (s) (string= (car strings) s)) strings))

(defun is-nested-silly-repeating-pattern-p (num)
  (let ((str-num (format nil "~d" num))
        (num-digits (number-of-digits num)))
    (loop for i from 1 to (floor num-digits 2)
          when (and (is-divisible-by-p num-digits i)
                    (all-strings-equal-p (split-string-every-n str-num i)))
          return t)))

(defun solution-2 (file)
  (let ((data (load-data file)))
    (loop for (start end) in data
          sum (silly-numbers-in-range start end #'is-nested-silly-repeating-pattern-p))))
