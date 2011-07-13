;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Ex 8.3.1
;1)
(check-expect (+ (* (/ 12 8) 2/3) 
                 (- 20 (sqrt 4))) 19)
;2)
(check-expect 
 (cond
   [(= 0 0) false]
   [(> 0 1) (symbol=? 'a 'a)]
   [else (= (/  1 0) 9)])
 false)

;3) 
(check-expect 
 (cond
   [(= 2 0) false]
   [(> 2 1) (symbol=? 'a 'a)]
   [else (= (/  1 2) 9)])
 true)

;; Ex 8.3.2
;; f : number number  ->  number
(define (f x y)
  (+ (* 3 x) (* y y)))

;1)
(check-expect
 (+ (f 1 2) (f 2 1))
 14)

;2)
(check-expect
 (f 1 (* 2 3))
 39)

;3) 
(check-expect
 (f (f 1 (* 2 3)) 19)
 478)

;; Ex 8.7.1
;1) legal
;2) illegal
;3) legal
;4) illegal
;5) legal

;; Ex 8.7.2
;1) value
;2) value
;3) value

;; Ex 8.7.3
(define-struct ball (x y speed-x speed-y))
;1)
(check-expect
 (number? (make-ball 1 2 3 4))
 false)

;2)
(check-expect 
 (ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))
 3)

;3)
(check-expect 
 (ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))
 6)

(ball-speed-y 5)
