(defun checkPrime (x y)
  (if (> y 1)
    (if (= (mod x y) 0)
      0
      (checkPrime x (- y 1))
    )
    1
  )
)
(defun intToList (i n)
  (if (= n 1)
    (cons (floor i n) '())
    (cons (floor i n) (intToList (mod i n) (/ n 10)))
  )
)
(defun rotationList (inList rotList)
  (print (member '(1 2 3) '((1 2 3) (3 1 2) (2 3 1))))
  (if (member inList rotList)
    (
      rotationList (append (cdr inList) (list (car inList))) (append (list inList) rotList)
    )
    rotList
  )
)


  (rotationList '(1 2 3) nil

  )

