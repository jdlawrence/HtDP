;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dummy) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; add-at-end: posn polygon -> polygon
;; adds the first posn in a polygon to the end of the list
(define (add-at-end p a-poly)
  (cond
    [(empty? (rest a-poly)) (cons (first a-poly) (cons p empty))]
    [else(cons (first a-poly) (add-at-end p (rest a-poly)))]))


(define dummy-triangle
  (cons (make-posn 10 10)
        (cons (make-posn 60 60)
              (cons (make-posn 10 60)
                    empty))))


(add-at-end (first dummy-triangle) dummy-triangle)