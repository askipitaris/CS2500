;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HW3 KindaComplete|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; --------- Exercise 5 --------- 
; world is one of the following strings: 
; -"STRING"
; -"G"
; -"O1"
; -"O2"
; -"D"

(define START (rectangle 500 300 "solid" "white"))
(define G (rectangle 500 300 "solid" "pale green"))
(define O1 (rectangle 500 300 "solid" "spring green"))
(define O2 (rectangle 500 300 "solid" "lime green"))
(define D (rectangle 500 300 "solid" "dark green"))

; change-state takes a key and changes the world state
; change-state : world key-> world

(define (change-state world key)
  (cond
    [(string=? "START" world)
     (if (string=? key "g") "G" "START")]
    [(string=? "G" world)
     (if (string=? key "o") "O1" (if (string=? key "g") "G" "START"))]
    [(string=? "O1" world) 
     (if (string=? key "o") "O2"(if (string=? key "g") "G" "START"))]
    [(string=? "O2" world)
     (if (string=? key "o") "O2" (if (string=? key "d") "D"(if (string=? key "g") "G" "START")))]
    [(string=? "D" world) "D"]
    [else "START"]))

(check-expect (change-state "START" "g") "G")
(check-expect (change-state "START" "b") "START")
(check-expect (change-state "G" "o") "O1")
(check-expect (change-state "G" "g") "G")
(check-expect (change-state "G" "a") "START")
(check-expect (change-state "O1" "o") "O2")
(check-expect (change-state "O1" "g") "G")
(check-expect (change-state "O1" "a") "START")
(check-expect (change-state "O2" "o")"O2")
(check-expect (change-state "O2" "d") "D")
(check-expect (change-state "O2" "g") "G")
(check-expect (change-state "O2" "i") "START")
(check-expect (change-state "D" "p") "D")

; dislays an imaged based off of last user input
; display : key -> world -> image

(define (display world)
  (cond
    [(string=? "START" world) START]
    [(string=? "G" world) G]
    [(string=? "O1" world) O1]
    [(string=? "O2" world) O2]
    [(string=? "D" world) D]
    [else START]))

(check-expect (display "START") START)
(check-expect (display "G") G)
(check-expect (display "O1") O1)
(check-expect (display "O2") O2)
(check-expect (display "D") D)

; Big-bang
(define (main world)
  (big-bang world
    [on-key change-state]
    [to-draw display]))

#;(define (display-temp w )
    (cond
      [(string=? "START" w) ...]
      [(string=? "G" w) ...]
      [(string=? "O1" w)...]
      [(string=? "O2" w) ...]
      [(string=? "D" w)...]))

#;(define (change-state-temp w k)
    (cond
      [(string=? "START" world)  (...)]
      [(string=? "G" world) (...)]
      [(string=? "O1" world) (...)]
      [(string=? "O2" world) (...)]
      [(string=? "D" world) (...)]
      [else "START"]))

; --------- Excercise 6 ---------

; Prints a location based off of user input
; location : string -> string string

(define (location input)
  (if (string? input) (make-posn (substring input 0 1) (substring input 1 3))
      "decode: bad input string"))

(check-expect (location "150") (make-posn "1" "50"))
(check-expect (location 150) "decode: bad input string")

#;(define (location-temp in)
    (cond
      [(if (string? in) (...)
           (...))]))

; Consumes a String and produces a (make-posn ...)
; whose x-component is the first character of the given string, and
; whose y-component is the remainder of the given string.
; An empty input string should error with "decode: bad input string"

; --------- Excercise 7 ---------

;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle


(define-struct circl (x y r outline c))
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid,
;;   and c its color
(define circle-thing (make-circl 10 10 10 "outline" "red"))


(define-struct squar (x y size outline c))
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: Supply a good interpretation of Square.
(define square-thing (make-squar 50 50 10 "outline" "blue"))


(define-struct recta (x y width height outline c))
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: Supply a good interpretation of Rectangle.
(define rectangle-thing (make-recta 30 30 10 5 "outline" "green"))


#;(define (shapes-temp sh ...)
    (cond
      [(circl? sh) (...)]
      [(sqaur? sh) (...)]
      [(recta? sh) (...)]))


; Shifts the location of a shape in the x direction
; shapes-shift-x : shapes dx -> new pos

; Problem 3
(define (shape-shift-x sh delta)
  (cond
    [(circl? sh) (make-circl (+ (circl-x sh) delta)
                             (circl-y sh) (circl-r sh)
                             (circl-outline sh) (circl-c sh))]
    [(squar? sh) (make-squar (+ (squar-x sh) delta)
                             (squar-y sh) (squar-size sh)
                             (squar-outline sh) (squar-c sh))]
    [(recta? sh) (make-recta (+ (recta-x sh) delta)
                             (recta-y sh) (recta-width sh)
                             (recta-height sh) (recta-outline sh) (recta-c sh))]
    [else "NOT A SHAPE"]))

(check-expect (shape-shift-x circle-thing 1) (make-circl 11 10 10 "outline" "red"))
(check-expect (shape-shift-x square-thing 1) (make-squar 51 50 10 "outline" "blue"))
(check-expect (shape-shift-x rectangle-thing 1) (make-recta 31 30 10 5 "outline" "green"))


;Problem 4
(define (p x y)
  (make-posn x y)) 


;;inside-circ tells the user if the selected point is within the circle.
; inside-circ : shape posn -> overlap? 
(define (inside-circ sh p)
  (cond
    [(and(>= (+ (sqrt(- (sqr (circl-r sh))
                        (sqr(- (posn-y p) (circl-y sh))))) (circl-x sh)) (posn-x p))
         (<= (- (posn-x p) (sqrt(- (sqr(circl-r sh))
                                   (sqr(- (posn-y p) (circl-y sh)))))) (posn-x p)))
     "the point is in the circle"]
    [else "the point is out of the circle"]))

;;point near the center 
(check-expect (inside-circ circle-thing (p 5 10)) "the point is in the circle")
;;point right on the border
(check-expect (inside-circ circle-thing (p 10 20)) "the point is in the circle")
;;point just out of the circle
(check-expect (inside-circ circle-thing (p 11 20)) "the point is out of the circle")

; inside-square tells the user if the selected point is within the square.
; inside-sqaure : shape posn -> overlap? 
(define (inside-square sh p)
  (cond
    [(and (and (>= (posn-x p) (- (squar-x sh) (/ (squar-size sh) 2)))
               (<= (posn-x p) (+ (squar-x sh) (/ (squar-size sh) 2))))
          (and (>= (posn-y p) (- (squar-y sh) (/ (squar-size sh) 2)))
               (<= (posn-y p) (+ (squar-y sh) (/ (squar-size sh) 2))))) "the point is in the square"]
    [else "the point is out of the square"]))

;;point on the center 
(check-expect (inside-square square-thing (p 50 50)) "the point is in the square")
;;point right on the border
(check-expect (inside-square square-thing (p 55 50)) "the point is in the square")
;;point just out of the square
(check-expect (inside-square square-thing (p 56 50)) "the point is out of the square")



; inside-rectangle tells the user if the selected point is within the rectangle.
; inside-rectangle : shape posn -> overlap? 
(define (inside-rectangle sh p)
  (cond
    [(and (and (>= (posn-x p) (- (recta-x sh) (/ (recta-width sh) 2)))
               (<= (posn-x p) (+ (recta-x sh) (/ (recta-width sh) 2))))
          (and (>= (posn-y p) (- (recta-y sh) (/ (recta-height sh) 2)))
               (<= (posn-y p) (+ (recta-y sh) (/ (recta-height sh) 2)))))
     "the point is in the rectangle"]
    [else "the point is out of the rectangle"]))

;; rectangle x=30 y=30 width=10 height=5
;;point on the center 
(check-expect (inside-rectangle rectangle-thing (p 30 30)) "the point is in the rectangle")
;;point right on the border
(check-expect (inside-rectangle rectangle-thing (p 35 30)) "the point is in the rectangle")
;;point just out of the rectangle
(check-expect (inside-rectangle rectangle-thing (p 36 30)) "the point is out of the rectangle")

;; shape-in? takes in a posn and a shape and outputs a string
(define (shape-in? sh p)
  (cond
    [(circl? sh) (inside-circ sh (make-posn (posn-x p) (posn-y p)))]
    [(squar? sh) (inside-square sh (make-posn (posn-x p) (posn-y p)))]
    [(recta? sh) (inside-rectangle sh (make-posn (posn-x p) (posn-y p)))]))

(check-expect (shape-in? rectangle-thing (p 30 30)) "the point is in the rectangle")
(check-expect (shape-in? square-thing (p 55 50)) "the point is in the square")
(check-expect (shape-in? circle-thing (p 11 20)) "the point is out of the circle")

;Problem 5

; shape-draw takes a shape and a scene and draws the shape onto the scene.
; shape-draw : sh sc -> image
(define sc (empty-scene 500 500))

(define (shapes-draw sh sc)
  (cond
    [(circl? sh) (place-image (circle (circl-r sh) 
                                      (circl-outline sh) (circl-c sh)) 
                              (circl-x sh) (circl-y sh) sc)]
    [(squar? sh) (place-image (square (squar-size sh) 
                                      (squar-outline sh) (squar-c sh)) 
                              (squar-x sh) (squar-y sh) sc)]
    [(recta? sh) (place-image (rectangle (recta-width sh) 
                                         (recta-height sh) (recta-outline sh) 
                                         (recta-c sh)) (recta-x sh)  (recta-y sh) sc)]))

(check-expect (shapes-draw square-thing sc)
              (place-image (square 10 "outline" "blue") 50 50 sc))
(check-expect (shapes-draw circle-thing sc)
              (place-image (circle 10 "outline" "red") 10 10 sc))
(check-expect (shapes-draw rectangle-thing sc)
              (place-image (rectangle 10 5 "outline" "green") 30 30 sc))