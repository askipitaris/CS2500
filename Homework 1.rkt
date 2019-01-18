;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;EXCERCISE 4
;Takes natural number and produces string.
;Number -> String
(define (Qwerty N)
  (cond
    [(= N 6) ""]
    [(= N 5) "Q"]
    [(= N 4) "QW"]
    [(= N 3) "QWE"]
    [(= N 2) "QWER"]
    [(= N 1) "QWERT"]
    [(= N 0) "QWERTY"]
    [else  "PUT A VALID NUMBER"]))

(check-expect (Qwerty 6) "")
(check-expect (Qwerty 5) "Q")
(check-expect (Qwerty 4) "QW")
(check-expect (Qwerty 3) "QWE")
(check-expect (Qwerty 2) "QWER")
(check-expect (Qwerty 1) "QWERT")
(check-expect (Qwerty 0) "QWERTY")
(check-expect (Qwerty 432439) "PUT A VALID NUMBER")

;EXCERCISE 5
;consumes a natural number and extracts
;that many keyboard characters from the string "qwerty".
;Number -> String
(define (qwerty n)
  (cond
    [(and (>= 6 n) (<= 0 n)) (substring "qwerty" 0 n)]
    [else "qwerty"]))
(check-expect (qwerty 5) "qwert")
(check-expect (qwerty 228) "qwerty")

;EXCERCISE 6
;consumes two counting numbers: one that represents the speed of a car
;and the other one the speed limit of the road.
;Number Number -> String
(define (ticket speed limit)
  (cond
    [(> limit speed) "fine"]
    [(and (< limit speed) (>= (+ limit 5) speed)) "danger"]
    [else (string-append "You drove " (number->string speed) " mph!")]))
(check-expect (ticket 40 45) "fine")
(check-expect (ticket 42 40) "danger")
(check-expect (ticket 69 40) "You drove 69 mph!")

;EXCERCISE 7
;"grows" the image of “Hello World”

(define (txt size)
  (place-image (text "Hello World" size "black") 250 150 (empty-scene 500 300)))

; Sets font size to 1 on left-click
; world int int string -> world

(define (mouse w x y key)
  (cond
    [(string=? key "button-down") 1]
    [else w]))

; Checks the world state and adds one if its not 80
; world -> world

(define (is80 size)
  (cond
    [(= size 80) 80]
    [else (+ size 1)]))

; Uses all fuctions to grow the text to 80 and reset to 1 on click
; world -> world

(define (grow size)
  (big-bang size
    [to-draw txt]
    [on-tick is80]
    [on-mouse mouse]))

(check-expect (txt 6) (place-image (text "Hello World" 6 "black") 250 150 (empty-scene 500 300)))
(check-expect (txt 50) (place-image (text "Hello World" 50 "black") 250 150 (empty-scene 500 300)))
(check-expect (txt 80) (place-image (text "Hello World" 80 "black") 250 150 (empty-scene 500 300)))
(check-expect (mouse 3 50 100  "button-down") 1)
(check-expect (mouse 60 70 150 "button-down") 1)
(check-expect (mouse 65 50 130 "move") 65)
(check-expect (is80 80) 80)
(check-expect (is80 79) 80)
(check-expect (is80 2) 3)