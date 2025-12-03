(defun load-data (file)
  (aoc:read-data-file file))

(defun largest-2-digit-number (str)
  (loop for c across str
        for i from 1 below (length str)
        maximize (loop for next-c across (subseq str i)
                       maximize (parse-integer (format nil "~c~c" c next-c)))))

(defun solution-1 (file)
  (let ((nums (load-data file)))
    (loop for batteries in nums
          sum (largest-2-digit-number batteries))))

#|

(defun largest-n-digit-number (str n)
  "Returns the largest n digit number taken from str without sorting the digits."
  (cond
    ((< (length str) n)
     (error "Not enough digits in string ~a to retrieve ~a digits" str n))
    ((= n 0) "")
    ((= (length str) n)
     (parse-integer str))
    (t (loop for digit across str
             for i from 1 to (- (length str) n)
             maximize (max (parse-integer
                            (format nil "~c~a"
                                    digit
                                    (largest-n-digit-number (subseq str 1) (- n 1))))
                           (largest-n-digit-number (subseq str 1) n))))))



|#

(defun remove-lowest-digit (digit-string)
  (let ((lowest-digit (char (sort (copy-seq digit-string) #'char<) 0)))
    (remove lowest-digit digit-string :count 1)))

(defun largest-n-digit-number (str n)
  (let ((len (length str)))
    (do ((digit-string (subseq str (- len n)))
         (i (- len n 1) (1- i)))
        ((= i -1) (parse-integer digit-string))
      (when (char<= (char digit-string 0) (char str i))
        (setf digit-string (format nil "~c~a" (char str i) (remove-lowest-digit digit-string)))))))

;; This function isn't quite right, so I'm not getting the correct answer - the one I'm getting is too low.
(defun solution-2 (file)
  (let ((nums (load-data file)))
    (loop for batteries in nums
          sum (largest-n-digit-number batteries 12))))
