;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter5) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp")))))
(define (reply s) 
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine] 
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

(reply 'HowAreYou?)

;; check-guess: number number -> symbol
;; compares 2 numbers and outputs various depending 
;; on the result of the comparison
(define (check-guess guess target)
  (cond 
    [(< guess target) 'TooSmall]
    [(= guess target) 'Perfect]
    [(> guess target) 'TooLarge]))


;; Check-guess with GUI test
;(guess-with-gui check-guess)

;; check-guess3: number number number number-> symbol
;; Takes in the 3 digits of a number, in order from 
;; least significant to most significant.
;; Outputs the result of the 3-digit number compared 
;; to the target numbers
(define (check-guess3 ones tens hundreds target)
  (check-guess (create-guess ones tens hundreds) target))

;; create-guess: number number number -> number
;; Takes in 3 numbers in order from least significant 
;; to most significant place and creates a 3 digit #
(define (create-guess ones tens hundreds)
  (+ ones (* 10 tens) (* 100 hundreds)))
;(guess-with-gui-3 check-guess3)

;; what-kind: number number number -> symbol
;; takes in coefficients a, b, and c of quadratic equation
;; outputs the number of solutions and/or degenerate 
;; if a equals '0'
(define (what-kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (* b b) (* 4 a c)) 'two]
    [(= (* b b) (* 4 a c)) 'one]
    [(< (* b b) (* 4 a c)) 'zero]))

(what-kind 3 9 3)

;; check-color: symbol symbol symbol symbol -> symbol
;; Takes in two target colors and two guess colors and outputs
;; 'Perfect, if the first target is equal to the first guess and 
;; the second target is equal to the second guess;
;; 'OneColorAtCorrectPosition, if the first guess is equal to the 
;; first target or the second guess is equal to the second target;
;; 'OneColorOccurs, if either guess is one of the two targets; 
;; and 'NothingCorrect, otherwise.
(define (check-color target1 target2 guess1 guess2)
  (cond
    [(and (symbol=? target1 guess1) (symbol=? target2 guess2)) 'Perfect]
    [(or (symbol=? target1 guess1) (symbol=? target2 guess2)) 'OneColorAtCorrectPosition]
    [(or (symbol=? target1 guess2) (symbol=? target2 guess1)) 'OneColorOccurs]
    [else 'NothingCorrect]))

(master check-color)