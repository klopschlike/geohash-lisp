(defvar +lookup+ "0123456789bcdefghjkmnpqrstuvwxyz")

(defun string-to-numbers (geo-string)
  "Converts base32 string to a list of according numbers"
  (mapcar #'(lambda (x) (position x +lookup+)) (coerce geo-string 'list)))

(defun string-from-numbers (number-list)
  "Converts a list of numbers into according base32 representation"
  (format nil "~{~A~}"
    (mapcar #'(lambda (x) (subseq +lookup+ x (+ x 1))) number-list)))

(defun numbers-to-number (numbers &key (base 32))
  "Takes a list of numbers as representation for each digit on certain base"
  (apply #'+
    (loop for x in (reverse numbers) 
          for y from 0
      collect (* x (expt base y)))))

(defun number-to-binary-list (input-number)
  "Converts a number to its binary representation as a list of bits"
  (reverse
    (loop for i below (integer-length input-number)
          collect (if (logbitp i input-number) 1 0))))

(defun merge-list-alternating (list-a list-b)
  "Creates a list with alternating items from both input lists"
  (loop for i from 0 to (- (length list-a) 1) 
        collect (nth i list-a) 
        if (nth i list-b)
        collect (nth i list-b)))

(defun split-list-alternating (binary-list)
  "Pushes elements into two alternating lists"
    (loop for i in (reverse binary-list)
          for y from 0
          if (evenp y)
            collect i into evens
          else
            collect i into odds
          finally (return (list (reverse evens) (reverse odds)))))

(defun geohash-to-numeric (geohash)
  (numbers-to-number (string-to-numbers geohash)))

(defun geohash-to-numbers (geohash)
  "Konvertiert einen hash in die zwei repräsentativen zahlen"
  (loop for x in (split-list-alternating (number-to-binary-list (geohash-to-numeric geohash)))
        collect (numbers-to-number x :base 2)))

(defun geohash-resolution-hash-length (hash-length)
  (list (expt 2 (ceiling  (/ (* 5 hash-length) 2)))
        (expt 2 (floor  (/ (* 5 hash-length) 2)))))

(defun geohash-resolution (geohash)
  ; 1 based
  "Returns length each dimension"
  (geohash-resolution-hash-length (length geohash)))

(defun geohash-binary-length (hash-length)
  (list (ceiling  (/ (* 5 hash-length) 2))
        (floor  (/ (* 5 hash-length) 2))))

(defun geohash-to-fraction (geohash)
  "Converts a hash to two representing fractions of the lower left corner"
  (map 'list #'/
       (geohash-to-numbers geohash)
       (geohash-resolution geohash)))

(defun geohash-to-coordinates (geohash)
  "Converts a geohash to its coordinates"
  ; Calculates coordinates in bottom left
  (map 'list #'(lambda (frac conv)
                 (* (- frac (/ 1 2)) (- (second conv) (first conv))))
       (geohash-to-fraction geohash)
       '((-90 90) (-180 180))))

(defun coordinates-to-fraction (coordinates)
  (map 'list #'(lambda (coord conv)
                 (+ (/ coord (- (second conv) (first conv))) (/ 1 2)))
       coordinates
       '((-180 180) (-90 90))))

(defun coordinates-to-geohash (coordinates hash-length)
  (apply #'merge-list-alternating (map 'list #'(lambda (frac len binlen)
                                                 (coerce (format nil "~V,'0b" binlen (floor (* frac len)) ) 'list))
       (coordinates-to-fraction coordinates)
       (geohash-resolution-hash-length hash-length)
       (geohash-binary-length hash-length))))

(defun binary-to-hash (binary-numbers)
  "Converts a list of binary numbers to its geohash/base32 representation"
  (if (> (length binary-numbers) 0)
      (append (list (numbers-to-number (subseq binary-numbers 0 5) :base 2))  (binary-to-hash (subseq binary-numbers 5)  ))
      '()))

