;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dummy) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
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
 (prime? 10)
 true)