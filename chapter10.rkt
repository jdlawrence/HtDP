;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter10) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; hours->wages : list-of-numbers  ->  list-of-numbers
;; to create a list of weekly wages from a list of weekly hours (alon)
(define (hours->wages alon)
  (cond
    [(empty? alon) empty]
    [else (cond
            [(> (first alon) 100) (error 'hour->wages "too many hours")]
            [else(cons (wage (first alon)) (hours->wages (rest alon)))])]))

;; wage : number  ->  number
;; to compute the total wage (at $12 per hour)
;; of someone who worked for h hours
(define (wage h)
  (* 14 h))

(check-expect
 (hours->wages (cons 5 (cons 10 empty)))
 (cons 70 (cons 140 empty)))

;; Ex 10.1.1
;; Simply change the multiplier in wage from 12 to 14

;; Ex 10.1.2
;; See hours-wages above: new functionality checks to make sure that
;; no employee is working more than 100 hours

;; Ex 10.1.3
;; convertFC_indiv: number -> number
;; Takes in a number in Fahrenheit and converts it to celsius
(define (convertFC_indiv fahren)
  (* 5/9 (- fahren 32)))

;; convertFC: list-of-numbers -> list-of-numbers
;; Takes in a list of temperatures in Fahrenheit and spits 
;; them out in celsius
(define (convertFC alon)
  (cond
    [(empty? alon) empty]
    [else (cons (convertFC_indiv (first alon)) (convertFC (rest alon)))]))

(check-within
 (convertFC_indiv 100)
 37.7 .1)

(check-within 
 (convertFC (cons -40 (cons 100 empty)))
 (cons -40 (cons 37.7 empty)) .1)

;; Ex 10.1.4 Skipped, too repititive

;; Ex 10.1.5
;; eliminate-exp: number list-of-numbers -> list-of-numbers
;; Consumates a threshold (ua) and a list of toy prices (lotp).
;; Outputs all numbers which are equal to or below the threshold
(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty]
    [else 
     (cond
       [(<= (first lotp) ua) (cons (first lotp) (eliminate-exp ua (rest lotp)))]
       [else (eliminate-exp ua (rest lotp))])]))

(check-expect
 (eliminate-exp 6 (cons 6 (cons 7 empty)))
 (cons 6 empty))

;; Ex 10.1.6 (Skipped straight to substitute)
;; substitute: symbol symbol list-of-symbols -> list-of-sumbolsnumbers
;; Takes a list of numbers and replaces "old" symbol with "new" symbol
;; in a new list of symbols
(define (substitute new old los)
  (cond 
    [(empty? los) empty]
    [else
     (cond
       [(symbol=? (first los) old) (cons new (substitute new old (rest los)))]
       [else (cons (first los) (substitute new old (rest los)))])]))

(check-expect
 (substitute 'a 'b (cons 'c (cons 'd empty)))
 (cons 'c (cons 'd empty)))

(check-expect
 (substitute 'yes! 'c (cons 'c (cons 'd empty)))
 (cons 'yes! (cons 'd empty)))

;; Ex 10.1.7
;; quadratic-roots: number number number -> 'symbol or list-of-numbers
;; Given three numbers a,b,c
;; 1)if a = 0, its output is 'degenerate.
;; 2)if b2 < 4 · a · c, the quadratic equation has no solution; quadratic-roots produces 'none in this case.
;; 3) if b2 = 4 · a · c, the equation has one solution: -b/(2a)
;; if b2 > 4 · a · c, the equation has two solutions: (-b+sqrt(b^2-4ac))/(2a) and (-b-sqrt(b^2-4ac))/(2a)
(define (quadratic-roots a b c)
  (cond 
    [(= a 0) 'degenerate]
    [(< (* b b) (* 4 a c)) 'none]
    [(= (* b b) (* 4 a c)) (/ (* b -1) (* 2 a))]
    [else (cons (/ (+ (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) (cons (/ (- (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) empty))]))

(check-expect
 (quadratic-roots 0 5 2)
 'degenerate)

(check-expect
 (quadratic-roots 10 5 2)
 'none)

(check-expect
 (quadratic-roots 3 6 3)
 -1)

(check-within
 (quadratic-roots 3 9 3)
 (cons -.38 (cons -2.62 empty)) .01)

;; Ex 10.1.9
;; controller: number -> list-of-items
;; Takes in a price in cents
;; Outputs a list with amount in dollar, unit of dollar, the symbol 'and
;; the cent amount, and unit of cent (cent or cents)
(define (controller price)
  (cons (quotient price 100) ;; dollar amount
        (cons (cond 
                [(= (quotient price 100) 1) 'dollar]
                [else 'dollars]) ;; dollar unit
              (cons 'and ;; 'and
                    (cons (remainder price 100) ;; cent amount
                          (cons (cond
                                  [(= (remainder price 100) 1) 'cent]
                                  [else 'cents]) empty))))))

(controller 201)

;; Ex 10.2.1
;; Data structure: inventory record (ir)
;; Has a name (symbol) and a price (number)
(define-struct ir (name price))

;; contains-doll? : inventory  ->  boolean
;; to determine whether an-inv contains a record for 'doll
(define (contains-doll? an-inv) 
  (cond
    [(empty? an-inv) false]
    [else(cond
           [(symbol=? (ir-name (first an-inv)) 'doll) true]
           [else (contains-doll? (rest an-inv))])])) 

;; contains? : symbol inventory  ->  boolean
;; to determine whether inventory contains a record for asymbol
(define (contains? asymbol an-inv) 
  (cond
    [(empty? an-inv) false]
    [else(cond
           [(symbol=? (ir-name (first an-inv)) asymbol) true]
           [else (contains? asymbol (rest an-inv) )])]))

(check-expect
 (contains-doll? (cons (make-ir 'dog 12) (cons (make-ir 'doll 13) empty)))
 true)

(check-expect
 (contains? 'cat (cons (make-ir 'cat 33) (cons (make-ir 'dog 12) (cons (make-ir 'cat 41) empty))) )
 true)

(image-width (circle 10 "solid" "red"))
