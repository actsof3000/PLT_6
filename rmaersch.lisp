(defun checkPrimeRec (x &optional (y (- x 1)))
  (or (= y 1)
    (and (/= (rem x y) 0)
      (checkPrimeRec x (- y 1))
    )
  )
)

(defun checkPrime (list)
  (setf n (listToInt (car list)))
  (if (> n 1)
    ((lambda (i l)
      (if (checkPrimeRec i)
        (checkPrime (cdr l))
        0
      )
    ) n list)
    1
  )
)

(defun intToList (i)
  (cond ((> i 99) (setf n 100))
  ((> i 9) (setf n 10))
  (t (setf n 1)))
  (if (= n 1)
    (cons (floor i n) '())
    (cons (floor i n) (intToList (mod i n)))
  )
)

(defun listToInt (l)
  (if (> (length l) 0)
    ((lambda (list)
      (setf n (expt 10 (- (length list) 1)))
      (+ (* (car list) n) (listToInt (cdr list)))
    ) l)
    0
  )
)

(defun getRotationList (l)
  (rotationList l '())
)

(defun rotationList (inList rotList)
  (if (> (length inList) (length rotList))
    (
      rotationList (append (cdr inList) (list (car inList))) (append (list inList) rotList)
    )
    rotList
  )
)

(defun specialPrime (n)
  (setf list (getRotationList (intToList n)))
  (if (> n 1)
    ((lambda (i)
      (if (checkPrimeRec n (- n 1))  
        (+ (checkPrime list) (specialPrime (- n 1)))
        (specialPrime (- n 1))
      )
    ) n)
    0
  )
)

(defun countSpecialPrimes (n)
  (specialPrime n)
)


