;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter12) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; sort : list-of-numbers  ->  list-of-numbers (sorted)
;; to create a list of numbers with the same numbers as
;; alon sorted in descending order
(define (sort alon)
  (cond
    [(empty? alon) empty]
    [(cons? alon) (insert (first alon) (sort (rest alon)))]))

;; insert : number list-of-numbers (sorted)  ->  list-of-numbers (sorted)
;; to create a list of numbers from n and the numbers on
;; alon that is sorted in descending order; alon is sorted
(define (insert n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else (cond
            [(>= n (first alon)) (cons n alon)]
            [(<  n (first alon)) (cons (first alon) (insert n (rest alon)))])]))

(define templist
  (cons 1 (cons 3 (cons 4 (cons 5 empty)))))

(check-expect
 (sort templist)
 (cons 5 (cons 4 (cons 3 (cons 1 empty)))))

(define-struct mail (from date message))

(define temp-mail
  (make-mail 'jamil 34 'whats_the_damn_deal?))

;(mail-from temp-mail)

;; Ex 12.2.1
;; sort-by-date: list-of-mail -> list-of-mail (sorted by date)
;; creates a list of mail messages with the same mail as 
;; alom sorted in descending order by date
(define (sort-by-date alom)
  (cond
    [(empty? alom) empty]
    [(cons? alom) (insert-by-date (first alom) (sort-by-date (rest alom)))]))

;; insert-by-date: mail-message list-of-mail -> list-of-mail (sorted by date)
;; creates a list of mail which includes the mail-message m and 
;; is sorted in descending order by date
(define (insert-by-date m alom)
  (cond
    [(empty? alom) (cons m empty)]
    [else 
     (cond
       [(>= (mail-date m) (mail-date (first alom))) (cons m alom)]
       [(< (mail-date m) (mail-date (first alom))) (cons (first alom) (insert-by-date m (rest alom)))])]))

(check-expect 
 (sort-by-date (cons (make-mail 'jamil 2 'lawrence) (cons (make-mail 'jamil 3 'lawrence) (cons (make-mail 'jamil 4 'lawrence) empty))))
 (cons (make-mail 'jamil 4 'lawrence) (cons (make-mail 'jamil 3 'lawrence) (cons (make-mail 'jamil 2 'lawrence) empty))))

;; sort-by-name: list-of-mail -> list-of-mail (sorted by name)
;; creates a list of mail message with the same mail as 
;; alom sorted in descending order by name
(define (sort-by-name alom)
  (cond
    [(empty? alom) empty]
    [(cons? alom) (insert-by-name (first alom) (sort-by-name (rest alom)))]))

;; insert-by-name: mail-message list-of-mail -> (sorted by name)
;; creates a list of mail which includes the mail message m and
;; is sorted in descending order by name
(define (insert-by-name m alom)
  (cond
    [(empty? alom) (cons m empty)]
    [else
     (cond
       [(string>=? (mail-from m) (mail-from (first alom))) (cons m (rest alom))]
       [(string<? (mail-from m) (mail-from (first alom))) (cons (first alom) (insert-by-name m (rest alom)))])]))

(check-expect 
 (sort-by-name (cons (make-mail "x" 5 "lawrence") (cons (make-mail "y" 5 "lawrence") (cons (make-mail "z" 5 "lawrence") empty))))
 (cons (make-mail "z" 5 "lawrence") (cons (make-mail "y" 5 "lawrence") (cons (make-mail "x" 5 "lawrence") empty))))


;; Ex 12.3.1
;; draw-polygon : polygon  ->  true
;; to draw the polygon specified by a-poly 
(define (draw-polygon a-poly)
  ;****old function given by textbook*****
  ;(connect-dots (cons (last a-poly) a-poly)))
  (connect-dots (add-at-end (first a-poly) a-poly)))

;; connect-dots : polygon  ->  true
;; to draw connections between the dots of a-poly
(define (connect-dots a-poly)
  (cond
    [(empty? (rest a-poly)) true]
    [else (and (draw-solid-line (first a-poly) (second a-poly) 'red)
               (connect-dots (rest a-poly)))]))

;; last : polygon  ->  posn
;; to extract the last posn on a-poly
(define (last a-poly)
  (cond
    [(empty? (rest a-poly)) (first a-poly)]
    [else (last (rest a-poly))]))

;; add-at-end: polygon -> polygon
;; adds the first posn in a polygon to the end of the list
(define (add-at-end p a-poly)
  (cond
    [(empty? (rest a-poly)) (cons (first a-poly) (cons p empty))]
    [else(cons (first a-poly) (add-at-end p (rest a-poly)))]))


(start 300 300)

(define dummy-triangle
  (cons (make-posn 10 10)
        (cons (make-posn 60 60)
              (cons (make-posn 10 60)
                    empty))))

(add-at-end (first dummy-triangle) dummy-triangle)

(draw-polygon dummy-triangle)