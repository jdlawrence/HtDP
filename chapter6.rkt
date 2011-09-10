;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter6) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
(define (distance-to-0 a-posn)
  (sqrt 
   (+ (sqr (posn-x a-posn))
      (sqr (posn-y a-posn)))))

;; Tests
;(distance-to-0 (make-posn (* 2 3) (* 2 4)))

;; Sequence of drawing commands using teachpack draw.ss
;(start 300 300)
;(draw-solid-line (make-posn 1 1) (make-posn 500 500) 'red)
;(draw-solid-rect (make-posn 20 10) 50 200 'blue)
;(draw-circle (make-posn 200 10) 50 'red)
;(draw-solid-disk (make-posn 200 10) 48 'green)
;(stop)

;; dimensions of traffic light
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)
;; the positions of the bulbs
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))
;; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
(stop)

;; clear-bulb: symbol -> true
;; takes in a color of light to turn off
;; replaces filled circle with empty circle of same color
(define (clear-bulb color)
  (cond
    [(symbol=? color 'red) (clear-redraw (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)]
    [(symbol=? color 'yellow) (clear-redraw (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)]
    [(symbol=? color 'green) (clear-redraw (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)]))
;; clear-redraw: a-posn number symbol -> true
;; takes in a position and a color 
;; erases the solid circle at that color and replaces it with
;; an open circle
(define (clear-redraw a-posn radius color)
  (and (clear-solid-disk a-posn radius color)
       (draw-circle a-posn radius color)))
;(clear-bulb 'green)

;; draw-bulb: symbol -> true
;; takes in a color and turns that light on
(define (draw-bulb color)
  (cond
    [(symbol=? color 'red) (draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)]
    [(symbol=? color 'yellow) (draw-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)]
    [(symbol=? color 'green) (draw-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)]))
;(draw-bulb 'yellow)

;; switch: symbol symbol -> true
;; takes in two colors, clears the bulb for the first color 
;; and draws the bulb of the second color
(define (switch colorOff colorOn)
  (and (clear-bulb colorOff)
       (draw-bulb colorOn)))
;(switch 'yellow 'green)

;; next : symbol -> symbol
;; to switch a traffic light's current color and to return the next one
(define (next current-color)
  (cond
    [(and (symbol=? current-color 'red) (switch 'red 'green)) 'green]
    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red)) 'red]
    [(and (symbol=? current-color 'green) (switch 'green 'yellow)) 'yellow]))

;(draw-bulb 'red)
;(next 'red)
;(next 'green)
;(next 'yellow)
;(next 'red)

;;*********************************************************************************
(define-struct fighter (designation acceleration top-speed range))

(define (within-range a-fighter target)
  (cond
    [(> target (fighter-range a-fighter)) 'OutOfRange]
    [(<= target (fighter-range a-fighter)) 'WithinRange]))
;(within-range (make-fighter 'f22 400 800 23) 842)

(define (reduce-range a-fighter)
  (make-fighter (fighter-designation a-fighter)
                (fighter-acceleration a-fighter)
                (fighter-top-speed a-fighter)
                (* (fighter-range a-fighter) 0.2)))
#| Tests
(define a-fighter(make-fighter 'f22 400 800 23))
(define sucky-fighter(reduce-range a-fighter))
(fighter-range a-fighter) 
(fighter-range sucky-fighter)
|#

#| 6.5.1
;;process-movie: movie symbol -> ???
(define (process-movie a-movie a-producer)
  ...(movie-title a-movie)...
  ...(movie-producer a-movie)...)

;;process-boyfriend: boyfriend symbol -> ???
(define (process-boyfriend a-boyfriend phone)
  ...(boyfriend-name a-boyfriend)...
  ...(boyfriend-hair a boyfriend)...
  ...(boyfriend-eyes a boyfriend)...
  ...(boyfriend-phone a boyfriend)...)
|#

;; 6.5.2
(define-struct time (hours minutes seconds))

;; time-> seconds: time number
;; Input is a time, output is the time since midnight
;; in seconds
(define (time->seconds a-time)
  (+ (* (time-hours a-time) 3600)
     (* (time-minutes a-time) 60)
     (time-seconds a-time)))
;; Test
;(time->seconds (make-time 12 30 2))  

;; 6.6.1
;; circle: symbol symbol symbol -> true
;; uses a posn as its center 
;; a number as its radius
;; and a symbol as its color
(define-struct circle (center radius color))

;; 6.6.2
;; draw-a-circle: circle -> true
;; consumes a circle and draws it based off of the 
;; center, radius, and color
(define (draw-a-circle a-circle)
  (draw-circle (make-posn (posn-x (circle-center a-circle))
                          (posn-y (circle-center a-circle)))
               (circle-radius a-circle)
               (circle-color a-circle)))
;; Tests (drawing a test circle)
(define test-circle (make-circle (make-posn 150 150) 100 'blue))
;(start 300 300)
;(draw-a-circle test-circle)

;;6.6.3
;; in-circle: circle posn -> boolean
;; consumes a circle and determines whether or not posn 
;; is inside or outside circle

(define (in-circle a-circle a-posn)
  (if (> (distance a-circle a-posn) (circle-radius a-circle)) false true))

(define (distance a-circle a-posn)
  (sqrt (+ (sqr (- (posn-x (circle-center a-circle)) 
                   (posn-x a-posn)))
           (sqr (- (posn-y (circle-center a-circle)) 
                   (posn-y a-posn))))))

(define test-posn (make-posn 8 6))

;; Tests
#|
(distance test-circle test-posn)
(in-circle test-circle test-posn)
|#

;(posn-x (circle-center test-circle))

; 6.6.4 
;; translate-circle: circle delta -> circle
;; takes in a circle and a delta and makes a new circle
;; that is translated delta units to the right
(define (translate-circle a-circle delta)
  (make-circle (make-posn (+ (posn-x (circle-center a-circle)) delta)
                          (posn-y (circle-center a-circle)))
               (circle-radius a-circle)
               (circle-color a-circle)))
;(translate-circle (make-circle (make-posn 50 50) 10 'red) 22)

;6.6.5
;; clear-a-circle: circle -> true
;; consumes a circle and clears it from the canvas
;; based off of the center, radius, and color
(define (clear-a-circle a-circle)
  (clear-circle (circle-center a-circle)
                (circle-radius a-circle)
                (circle-color a-circle)))
;(clear-a-circle test-circle)

;; draw-and-clear-circle: number -> true
;; Consumes a number which indicates the amounf of seconds
;; to wait between the drawing and clearing of a circle
(define (draw-and-clear-circle a-circle)
  (and (draw-a-circle a-circle)
       (sleep-for-a-while 0.012)
       (clear-a-circle a-circle)))
;(start 300 300)
;(draw-and-clear-circle test-circle)

;; move-circle : number circle  ->  circle
;; to draw and clear a circle, translate it by delta pixels
(define (move-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle) (translate-circle a-circle delta)]
    [else a-circle]))

(start 300 300)
(draw-a-circle (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 (move-circle 5 test-circle)))))))))))))))))))))
