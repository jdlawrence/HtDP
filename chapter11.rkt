;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
    [(zero? n) empty]
    [else (cons (make-posn n (f n) ) (tabulate-f (sub1 n)))]))

(check-expect 
 (tabulate-f 2)
 (cons (make-posn 2 -1) (cons (make-posn 1 -4) empty)))

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
