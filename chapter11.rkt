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
(define numColumns 4)
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

(start canvasWidth canvasHeight)
(drawColumns (columnValues numColumns))
(drawRows (rowValues numRows))

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

(draw-points (pointsList 12000))

