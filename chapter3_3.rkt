;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter3_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; File full of conversions


;; basic conversions
(define (inches->cm inches)
  (* inches 2.54))

(define (feet->inches feet)
  (* feet 12))

(define (yards->feet yards)
  (* yards 3))

(define (rods->yards rods)
  (* rods 5.5))

(define (furlongs->rods furlongs)
  (* furlongs 40))

(define (miles->furlongs miles)
  (* miles 8))

;; multiple unit conversions
(define (feet->cm feet)
  (inches->cm (feet->inches feet)))

(define (yards->cm yards)
  (feet->cm (yards->feet yards)))

(define (rods->inches rods)
  (/ (yards->cm (rods->yards rods)) 2.54))

(define (rods->inches2 rods)
  (feet->inches
   (yards->feet
    (rods->yards rods))))

(define (miles->feet miles)
  (yards->feet
   (rods->yards
    (furlongs->rods
     (miles->furlongs miles)))))

(rods->inches 1)

(rods->inches2 1)
