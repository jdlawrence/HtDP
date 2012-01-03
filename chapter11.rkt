;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter11) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; Ex 11.2.1
;; repeat: symbol number -> list
;; Creates a list with a symbol repeated n times
(define (repeat symbol n)
  (cond
    [(zero? n) empty]
    [else (cons symbol (repeat symbol (sub1 n)))]))

(check-expect
 (repeat 'yes 3)
 (cons 'yes (cons 'yes (cons 'yes empty))))

;; Ex 11.2.2
;; Given function f
;; f : number  ->  number
(define (f x)
  (+ (* 3 (* x x)) 
     (+ (* -6 x)
        -1)))

;; tabulate-f
;; makes a list of f(n) for natural numbers n
(define (tabulate-f n)
  (cond
    [(zero? n) (cons (make-posn 0 -1) empty)]
    [else (cons (make-posn n (f n) ) (tabulate-f (sub1 n)))]))

(check-expect 
 (tabulate-f 3)
 (cons (make-posn 3 8) (cons (make-posn 2 -1) (cons (make-posn 1 -4) (cons (make-posn 0 -1) empty)))))

;; Ex 11.2.3
;; apply-n: list of-shapes number -> true
;; Applies move-picture to a list-of-shapes n times
;; **Commented out, see chap 10*******************
#|
(define (apply-n n)
  (cond
    [(zero? n) FACE]
    [else (move-picture 4 (apply-n (sub1 n)))]))
|#

;; Ex 11.2.4
;; depth: dl -> number
;; Consumes a deep list and outputs the number of "cons" that occurred
(define (depth dl)
  (cond
    [(symbol? dl) 0]
    [else(+ 1 (depth (first dl)))]))

(define dl_real
  (cons (cons (cons (cons (cons 'Smooth empty) empty) empty) empty) empty))

(check-expect
 (depth dl_real)
 5)

;; make-deep: symbol number -> dl
;; Consumes a symbol s and a number and makes a deep list containing s and
;; n-conses
(define (make-deep s n)
  (cond
    [(zero? n) s]
    [else(cons (make-deep s (sub1 n)) empty)]))

(check-expect
 (make-deep 'smooth 3)
 (cons (cons (cons 'smooth empty) empty) empty))

;; Ex 11.3.1
;; ;; random-n-m : integer integer  ->  integer
;; Takes in two integers m & n and generates a random number between the two
;; Assume: n < m
(define (random-n-m n m)
  (+ (random (- m n)) n))

;; Ex 11.3.2
;; tie-dyed:  N -> list-of-N
;; Takes in a natural number n a produces a list of n natural numbers

;; Helper Method from 9.5.8
;; draw-circles: posn a-list-of-numbers -> true
;; Takes in a posn p and a list of numbers representing the radii of circles
;; draws a circle in red for each of the radii in the list
(define (draw-circles p a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) true]
    [else (and 
           (draw-circle p (first a-list-of-numbers) 'red)
           (draw-circles p (rest a-list-of-numbers)))]))

(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else(cons (random-n-m 20 120) (tie-dyed (- n 1)))]))

;;*** Circle Drawing examples commented out******
;;(start 500 500)
;;(draw-circles (make-posn 250 250) (tie-dyed 7))
;;***********************************************


;; Ex 11.3.3
;; create-temps: N integer integer -> list-of-integers
;; Takes in a natural number and two integers low and high
;; Outputs a list of n integers
(define (create-temps n low high)
  (cond
    [(zero? n) empty]
    [else(cons (random-n-m low high) (create-temps (- n 1) low high))]))

;;(create-temps 10 100 110)

;; Helper Method from 9.5.4
;; check-range? : a-list-of-numbers number number -> boolean
;; Takes in a list of temperatures (reprepresented as numbers),
;; a lower boundary, and an upper boundary
;; and checks whether all numbers are in the boundary
(define (check-range? a-list-of-numbers lower upper)
  (cond 
    [(empty? a-list-of-numbers) true]
    [else (and
           (and (>= (first a-list-of-numbers) lower) (<= (first a-list-of-numbers) upper))
           (check-range? (rest a-list-of-numbers) lower upper))]))

(check-range? (create-temps 10 100 110) 100 110)

;; Ex 11.3.4
;; create-prices: N -> list
;; Consumes a natural number n
;; Outputs a list of corresponding prices between $.10 and $10.00
;; ****Note: Skipped tie-in with exercise 9.5.3; obviously this list is not a dollar store!***
(define (create-prices n)
  (cond
    [(zero? n) empty]
    [else(cons (/ (random-n-m 1 100) 10) (create-prices (- n 1)))]))

;; Ex 11.3.5

(define canvasWidth 1500)
(define canvasHeight 1000)
(define numColumns 16)
(define numRows 4)
(define columnWidth (/ canvasWidth numColumns))
(define rowHeight (/ canvasHeight numRows))

;; columnValues: integer -> list
;; Consumes an "index" number of desired columns
;; Outputs a list containing the x-values for the lines to draw those columns
(define (columnValues index)
  (cond
    [(zero? (- index 1)) empty]
    [else(cons (* columnWidth (- index 1)) (columnValues (- index 1)))]))

;; rowValues: integer -> list
;; Consumes an "index" number of desired rows
;; Outputs a list containing the y-values for the lines to draw those rows
(define (rowValues index)
  (cond
    [(zero? (- index 1)) empty]
    [else(cons (* rowHeight (- index 1)) (rowValues (- index 1)))]))

;; drawColumns: list -> true
;; Consumes a list containing the x-values of each column boundary
;; Outputs true when finished
(define (drawColumns columns)
  (cond
    [(empty? columns) true]
    [else(and 
          (draw-solid-line (make-posn (first columns) 0) 
                           (make-posn (first columns) canvasHeight))
          (drawColumns (rest columns)))]))

;; drawRows: list -> true
;; Consumes a list containing the y-values for each row boundary
;; Outputs true when finished
(define (drawRows rows)
  (cond
    [(empty? rows) true]
    [else(and
          (draw-solid-line (make-posn 0 (first rows))
                           (make-posn canvasWidth (first rows)))
          (drawRows (rest rows)))]))

;(columnValues numColumns)
;(rowValues numRows)

;; *****Draw Commands for Riot Balloons*** 
;(start canvasWidth canvasHeight)
;(drawColumns (columnValues numColumns))
;(drawRows (rowValues numRows))

;; pointsList: integer -> list-of-posn
;; Consumes an number "n" for the number of points in the list
;; Outputs a list with the position of random points
(define (pointsList n)
  (cond
    [(zero? n) empty]
    [else(cons (make-posn (random-n-m 0 canvasWidth) (random-n-m 0 canvasHeight))
               (pointsList (- n 1)))]))

;; draw-points: list-of-posns -> true
(define (draw-points list-of-points)
  (cond 
    [(empty? list-of-points) true]
    [else(and
          (draw-solid-disk (first list-of-points) 5 'red)
          (draw-points (rest list-of-points)))]))

;; *****Draw Commands for Riot Balloons*** 
;;(draw-points (pointsList 1200))

;; Ex 11.4.1

;; ! : N  ->  N
;; to compute n  ·  (n - 1)  ·  ...  ·  2  ·  1
;; (meaning n factorial)
(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)) )]))

(check-expect
 (! 2)
 2)

(check-expect
 (! 10)
 3628800)


;; Ex 11.4.2

;; product N N -> N
;; Consumes two natural numbers, n & m, where m > n
;; Outputs the product of the numbers between n(exclusive) and m (inclusive)
(define (product n m)
  (/ (! m) (! n)))

(check-expect
 (product 3 5)
 20)

;; Ex 11.4.3

;; product-from-minus-11: N [>= -11]  ->  N
;; to compute n  ·  (n - 1)  ·  ...  ·  -10  ·   -11
;; Computer the product of a number from n down to -11 (exclusive)
(define (product-from-minus-11 n-above-minus-11)
  (cond
    [(= n-above-minus-11 -11) 1]
    [else (* n-above-minus-11 (product-from-minus-11 (sub1 n-above-minus-11)))]))

(check-expect
 (product-from-minus-11 -9)
 90)

;; Ex 11.4.4

;; tabulate-f2: N [>= 20]  ->  list
;; makes a list of f(n) from exercises 11.2.2 paired with natural numbers n
;; (makes a list of posns)
(define (tabulate-f20 n-above-20)
  (cond
    [(= n-above-20 20) empty]
    [else (cons (make-posn n-above-20 (f n-above-20) ) (tabulate-f20 (sub1 n-above-20)))]))

(check-expect
 (tabulate-f20 23)
 (cons (make-posn 23 1448) (cons (make-posn 22 1319) (cons (make-posn 21 1196) empty))))

;; Ex 11.4.5

;; tabulate-f-lim: N[limit] N[>= limit] -> list-of-posns
;; Consumes two natural numbers and output a table containing a list of positions for the function described in 11.2.2
;; with n > limit
(define (tabulate-f-lim limit n)
  (cond
    [(= n limit) empty]
    [else (cons (make-posn n (f n)) (tabulate-f-lim limit (sub1 n)))]))

(check-expect
 (tabulate-f-lim 20 21)
 (cons (make-posn 21 1196) empty))

;; Ex 11.4.6

;; tabulate-f-up-to-20 : N [<= 20]  ->  N
;; Consumes a natural number and makes a list of the n paired with f(n) (from 11.2.2)
;; List goes from n to 20
(define (tabulate-f-up-to-20 n-below-20) 
  (cond
    [(= n-below-20 20) empty]
    [else (cons (make-posn n-below-20 (f n-below-20)) (tabulate-f-up-to-20 (add1 n-below-20)))]))

(check-expect
 (tabulate-f-up-to-20 17)
 (cons (make-posn 17 764) (cons (make-posn 18 863) (cons (make-posn 19 968) empty))))

;; Ex 11.4.7
;; is-not-divisible-by<=i: N[>=1] N -> boolean
;; Consumes a natural number i that >=1 a natural number m where i < m
;; Outputs true is m is not divisible by any number between 1(exclusive) and i(inclusive)
;; Produce false otherwise
(define (is-not-divisible-by<=i i m)
  (cond
    [(= 1 i) true]
    [else(cond
           [(= (modulo m i) 0) false]
           [else(is-not-divisible-by<=i (sub1 i) m)])]))

(check-expect
 (is-not-divisible-by<=i 9 19)
 true)

;; prime?: N -> boolean
;; Consumes a natural number and outputs whether or not it is prime
(define (prime? n)
  (is-not-divisible-by<=i (sub1 n) n))

(check-expect 
 (prime? 7)
 true)

;; Ex 11.5.1
;; add: N N -> N
;; consumes two natural numbers n & x
;; Outputs the sum of n + x without using the "+" symbol
(define (add n x)
  (cond
    [(zero? n) x]
    [else(add1 (add (sub1 n) x))]))

(check-expect
 (add 5 8)
 13)

;; Ex 11.5.1
;; multiply-by-pi: N -> floating#
;; consumes a natural number n
;; Outputs the number n multiplied by 3.14
(define (multiply-by-pi n)
  (cond
    [(zero? n) 0]
    [else (+ 3.14 (multiply-by-pi (sub1 n)))]))

;; Ex 11.5.2
;; multiply: N N -> N
;; consumes two natural number n & x
;; Outputs the product n * x
(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else (add x (multiply (sub1 n) x))]))


(check-expect
 (multiply-by-pi 3 )
 9.42)

(check-expect
 (multiply 5 6)
 30)

;; Ex 11.5.3
;; exponent: N floating# -> floating#
;; consumes a natural number n and a base x
;; Outputs x to the n power
(define (exponent n x)
  (cond
    [(zero? n) 1]
    [else (multiply (exponent (sub1 n) x) x)]))

(check-expect
 (exponent 3 5)
 125)

;; Ex 11.5.4
;; addDL: deep-list deep-list -> deep-list
;; Consumes two deep lists 
;; Outputs a deep-list representing the sum of two deep-lists
(define (addDL dl1 dl2)
  (make-deep 'smooth (+ (depth dl1) (depth dl2))))

(check-expect
 (depth (addDL (make-deep 'smooth 3) (make-deep 'smoth 5)))
 8)
