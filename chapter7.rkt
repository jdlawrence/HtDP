;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter7) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp")))))
;; Data Definitions
(define-struct star (last first dob ssn))
;; A star is a person with a 
;; last name, first name, dob, and ssn

(define-struct square (nw length))
;; A square has nw, which is a posn denoting its northwest corner
;; and a length of all sides
(define-struct circle (center radius))
;; A circle has center,which is a posn denoting its center
;; and a radius, which is the radius of the circle
(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* (square-length a-shape) 4)]
    [(circle? a-shape) (* (* 2 (circle-radius a-shape)) pi)]))

;; Ex 7.1.2
(define test-square(make-square (make-posn 20 20) 3))
(define test-circle(make-circle (make-posn 10 99) 1))

;(perimeter test-circle)

;; Ex 7.1.3
(define (area a-shape)
  (cond
    [(square? a-shape) (sqr(square-length a-shape))]
    [(circle? a-shape) (* (sqr(circle-radius a-shape)) pi)]))
;(area test-square)

;; 7.5.1

;; area-of-disk : number -> number 
;; to compute the area of a disk with radius r 
(define (area-of-disk r)
(* 3.14 (* r r)))

;; checked-area-of-disk : Scheme-value -> number 
;; to compute the area of a disk with radius v, 
;; if v is a number 
(define (checked-area-of-disk v)
(cond 
  [(and (number? v) (> v 0)) (area-of-disk v)] 
  [else (error 'checked-area-of-disk "positive number expected")]))

(checked-area-of-disk 3)

;; Data Definition
(define-struct vec (x y))
;; A vector is a speed vector having x, the x velocity
;; and y, the y velocity

;; checked-make-vec : (pos)number (pos)number -> vec
;; Takes in two number, checks to make sure they're positive
;; if yes, makes a vector, if no, outputs an error message
(define (checked-make-vec x y)
  (cond
    [(and (> x 0) (> y 0)) (make-vec x y)]
    [else (error 'checked-make-vec "Two positive numbers expected")]))

(checked-make-vec -5 2)