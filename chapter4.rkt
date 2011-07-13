;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (is-outside-3-7-inclusive n)
  (not (and (< 3 n) (>= 7 n))))

(define (equation-1 n)
  (= (+ (* 4 n) 2) 62))

(define (equation-2 n)
  (= (* 2 n n) 102))

(define (equation-3 n)
  (= (+ (* 4 n n) (* 6 n) 2) 462))


;; Tests for equation-1,2,3
;(equation-1 15)
;(equation-2 (sqrt 51)) 
;(equation-3 10)

(define (interest amount)
  (cond
    [(<= amount 1000) (* .04 amount)]
    [(<= amount 5000) (* .045 amount)]
    [else (* .05 amount)]))

(define (tax amount)
  (cond 
    [(<= amount 240) 0]
    [(and (< 240 amount) (<= amount 480)) (* .15 amount)]
    [else (* .28 amount)]))

(define (netpay hours)
  (- (gross hours) (tax (gross hours))))

(define (gross hours)
  (* 12 hours))
  
;(interest 20000)
;(tax 300)
;(netpay 25)

(define (pay-back amount)
  (cond
    [(<= amount 500) (* .0025 amount)]
    [(<= amount 1500) (+ (* .0025 500) (* .005 (- amount 500)))]
    [(<= amount 2500) (+ (* .0025 500) (* .005 1000) (* .0075 (- amount 1500)))]
    [else (+ (* .0025 500) (* .005 1000) (* .0075 1000) (* .01 (- amount 2500)))]))

;(pay-back 4001)

(define (how-many a b c)
  (cond
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))

(how-many 2 4 2)
   
  
  