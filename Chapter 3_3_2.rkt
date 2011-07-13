;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Chapter 3_3_2|) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
(define PI 3.1415923)

(define (volume-cylinder radius height)
  (* PI radius radius height))

(volume-cylinder 10 10)

(define (area-cylinder radius height)
  (* 2 PI radius height))

(area-cylinder 3 1)

(define (area-pipe1 innerRadius length thickness)
  (+ (* 2 PI innerRadius length) (* 2 PI (+ innerRadius thickness) length)))

;; Following are all part of area-pipe2 and its ancillary functions
(define (area-pipe2 innerRadius length thickness)
  (+ (innerArea innerRadius length) (outerArea innerRadius length thickness)))

(define (innerArea innerRadius length)
  (* 2 PI innerRadius length))

(define (outerArea innerRadius length thickness)
  (* 2 PI (outerRadius innerRadius thickness) length))

(define (outerRadius innerRadius thickness)
  (+ innerRadius thickness))

;; End of area-pipe2 functions

(area-pipe1 3 3 4)
(area-pipe2 3 3 4)

;; Conversion from fahrenheit to celsius
(define (convertFC fahren)
  (/ (* 5 (- fahren 32)) 9.0))

;; Conversion from celsius to fahrenheit 
(define (convertCF celsius)
  (+ (/ (* celsius 9) 5) 32))

(define (I f)
  (convertFC (convertCF f)))

(convertFC -40)
(convertCF 42)
(I 32)
