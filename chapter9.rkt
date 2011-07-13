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