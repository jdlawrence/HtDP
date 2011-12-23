;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter9) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;; Ex 9.3.1
(define a-list (cons 10 (cons 20 (cons 5 empty))))

;1) 
(check-expect
 (rest a-list)
 (cons 20 (cons 5 empty)))

;2) 
(check-expect 
 (first (rest a-list))
 20)

;3)
(check-expect
 (rest (rest a-list))
 (cons 5 empty))
;4)
(check-expect
 (first (rest (rest a-list)))
 5)

;5)
(check-expect
 (rest (rest (rest a-list)))
 empty)

;; Ex 9.1.3

;; add-up-3 : list-of-3-numbers  ->  number
;; to add up the three numbers in a-list-of-3-numbers
(define (add-up-3 a-list-of-3-numbers) 
  (+  (first a-list-of-3-numbers)
      (first (rest a-list-of-3-numbers)) 
      (first (rest (rest a-list-of-3-numbers)))))

(check-expect
 (add-up-3 (cons 5 (cons 2 (cons 5 empty))))
 12)

;; distance-to-0-for-3: list-of-3-numbers  ->  number
;; to add up the squares of three numbers in a-list-of-3-numbers
(define (distance-to-0-for-3 a-list-of-3-numbers) 
  (sqrt
   (+  (sqr(first a-list-of-3-numbers))
       (sqr(first (rest a-list-of-3-numbers))) 
       (sqr(first (rest (rest a-list-of-3-numbers)))))))

(check-within
 (distance-to-0-for-3 (cons 2 (cons 2 (cons 2 empty))))
 (* 2 (sqrt 3)) .1)

;; Ex 9.3.1
;; contains-doll? : list-of-symbols  ->  boolean
;; to determine whether the symbol 'doll occurs on a-list-of-symbols
#|
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (cond
            [(symbol=? (first a-list-of-symbols) 'doll) true]
            [else (contains-doll? (rest a-list-of-symbols))])]))  
|#

(check-expect 
 (contains-doll? (cons 'arrow (cons 'doll empty)))
 true)

;; Ex 9.3.2
;; contains-doll? : list-of-symbols  ->  boolean
;; to determine whether the symbol 'doll occurs on a-list-of-symbols
;; Alternative with "or" statement used instead on cond
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or 
           (symbol=? (first a-list-of-symbols) 'doll)
           (contains-doll? (rest a-list-of-symbols)))]))

;; Ex 9.3.
;; contains? : symbol list-of-symbols  ->  boolean
;; to determine whether the symbol given symbol occurs on a-list-of-symbols
(define (contains? a-symbol a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or 
           (symbol=? (first a-list-of-symbols) a-symbol)
           (contains? a-symbol (rest a-list-of-symbols)))]))

(check-expect 
 (contains? 'doll (cons 'arrow (cons 'doll empty)))
 true)

;; sum : list-of-numbers  ->  number
;; to compute the sum of the numbers on a-list-of-nums
(define (sum a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) 0]
    [else (+ (first a-list-of-nums) (sum (rest a-list-of-nums)))]))

;; Ex 9.5.1
(check-expect 
 (sum (cons 17.05 (cons 1.22 (cons 2.59 empty))))
 20.86)

;; Ex 9.5.2
;; how-many-symbols: a-list-of-symbols -> number
;; Counts the number of symbols in a number
(define (how-many-symbols a-list-of-symbols)
  (cond 
    [(empty? a-list-of-symbols) 0]
    [else (+ 1 (how-many-symbols (rest a-list-of-symbols)))]))

(check-expect 
 (how-many-symbols (cons 17.05 (cons 1.22 (cons 2.59 empty))))
 3)

(define (how-many-numbers a-list-of-numbers)
  (cond 
    [(empty? a-list-of-numbers) 0]
    [else (+ 1 (how-many-numbers (rest a-list-of-numbers)))]))

(check-expect 
 (how-many-numbers (cons 17.05 (cons 1.22 (cons 2.59 empty))))
 3)

;; Ex 9.5.3
;; dollar-store?: a-list-of-prices -> boolean
;; Checks whether all of the prices in a list 
;; are less than 1
(define (dollar-store? a-list-of-prices)
  (cond
    [(empty? a-list-of-prices) true]
    [else (and (< (first a-list-of-prices) 1) (dollar-store? (rest a-list-of-prices)))]))

(check-expect 
 (dollar-store? empty)
 true)

(check-expect
 (dollar-store? (cons .1 (cons 1.5 empty)))
 false)

;; Ex 9.5.4 
;; check-range1? : a-list-of-numbers -> boolean
;; Takes in a list of temperatures (reprepresented as numbers)
;; and checks whether all of them are between the range 5 to 95 C
(define (check-range1? a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) true]
    [else (and
           (and (>= (first a-list-of-numbers) 5) (<= (first a-list-of-numbers) 95))
           (check-range1? (rest a-list-of-numbers)))]))

(check-expect
 (check-range1? (cons 6 (cons 95 empty)))
 true)

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

(check-expect
 (check-range? (cons 6 (cons 95 empty)) 5 84)
 false)

;; Ex 9.5.5
;; convert: a-list-of-numbers -> number
;; Takes in a list of digits and produces a number
;; least significant digit is the first
(define (convert a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) 0]
    [else (+ (first a-list-of-numbers)
             (* 10 (convert (rest a-list-of-numbers))))]))

(check-expect
 (convert (cons 5 (cons 4 empty)))
 45)

(check-expect
 (convert (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
 4321)

;; check-for-guess-list: a-list-of-numbers number -> symbol
;; Takes in a list of digits which represents a player's guess
;; along with a number, which is the answer
;; Outputs results of 'TooSmall 'Perfect or 'TooLarge
(define (check-for-guess-list a-list-of-numbers answer)
  (cond 
    [(< (convert a-list-of-numbers) answer) 'TooSmall]
    [(= (convert a-list-of-numbers) answer) 'Perfect]
    [(> (convert a-list-of-numbers) answer) 'TooLarge]))

(check-expect 
 (check-for-guess-list (cons 3 (cons 7 empty)) 63)
 'TooLarge)

;;(guess-with-gui-list 2 check-for-guess-list)

;; Ex 9.5.6
;; delta: a-list-of-numbers a-list-of-number -> number
;; Takes in two inventory lists, substracts the value of the first from
;; the second, and outputs the difference
(define (delta first second)
  (- (sum second) (sum first)))

(check-expect
 (delta (cons 5 (cons 7 empty)) (cons 2 (cons 15 empty)))
 5)

;; Ex 9.5.7
;; average-price: a-list-of-numbers -> number
;; Takes in a-list-of-numbers and divides the sum by the number
;; of items in the list
(define (average-price a-list-of-numbers)
  (/ (sum a-list-of-numbers) (how-many-numbers a-list-of-numbers)))

(check-expect
 (average-price (cons 5 (cons 15 empty)))
 10)

;; Ex 9.5.8
;; draw-circles: posn a-list-of-numbers -> true
;; Takes in a posn p and a list of numbers representing the radii of circles
;; draws a circle in red for each of the radii in the list
(define (draw-circles p a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) true]
    [else (and 
           (draw-circle p (first a-list-of-numbers) 'red)
           (draw-circles p (rest a-list-of-numbers)))]))

;; *****Functions from Chapter 11, uses draw-circles
;; Ex 11.3.1
;; ;; random-n-m : integer integer  ->  integer
;; Takes in two integers m & n and generates a random number between the two
;; Assume: n < m
(define (random-n-m n m)
  (+ (random (- m n)) n))
 
;; Ex 11.3.2
;; tie-dyed:  number -> list
;; Takes in a natural number n a produces a list of n natural numbers
;; See draw-circles code in 9.5.8 for application
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else(cons (random-n-m 20 120) (tie-dyed (- n 1)))]))

(start 500 500)
(draw-circles (make-posn 250 250) (tie-dyed 7))