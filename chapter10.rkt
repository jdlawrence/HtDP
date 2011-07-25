;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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