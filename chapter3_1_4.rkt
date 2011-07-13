;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter3_1_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;comments in Racket!!!

;; profit: number -> number
;; to compute the profit as the difference between revenue and costs
;; at some given ticket-price
(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price))
  )


;; revenue : number -> number
;; to compute revenue, given ticket price
(define (revenue ticket-price)
  (* (attendees ticket-price) ticket-price)
  )

;; cost : number -> number
;; to compute the costs, given ticket price
(define (cost ticket-price)
  (* (attendees ticket-price) 150))
  )

;; attendees : number -> number
;; to compute the number of attendees, given ticket-price
(define (attendees ticket-price)
  (+ 120 
     (* (- 5 ticket-price) (/ 15 .10)))
  )

(profit 5)
