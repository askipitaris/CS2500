;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProblemSet7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ----------- EXERCISE THREE -----------
(define l1 (list 0 1 2 3))
(define l2 (list 3 2 1 0))
(define l3
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13
        12 11 10 9 8 7 6 5 4 3 2 1))
(define l4
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
        17 18 19 20 21 22 23 24 25))
(define l5
  (list 1 7 25 8 2 59 38 0 5 4))

;; NeLon [Number Number -> Boolean] -> Number
;; determines the largest or smallest number on l

(define (abstract1 lon op)
  (cond [(empty? (rest lon)) (first lon)]
        [else
         (if (op (first lon)
                 (abstract1 (rest lon) op))
             (first lon)
             (abstract1 (rest lon) op))]))

(check-expect (abstract1 l1 >) 3)
(check-expect (abstract1 l1 <) 0)
(check-expect (abstract1 l2 >) 3)
(check-expect (abstract1 l2 <) 0)
(check-expect (abstract1 l5 >) 59)
(check-expect (abstract1 l5 <) 0)

;; NeLon -> Number
;; determines the smallest number on l

(define (inf-1 lon)
  (abstract1 lon <))

;(check-expect (inf-1 l3) 1)  // Commented out because they take a long time to run
;(check-expect (inf-1 l4) 1)
;(check-expect (inf-1 15) 0)

;; NeLon -> Number
;; determines the largest number in l

(define (sup-1 lon)
  (abstract1 lon >))

;(check-expect (sup-1 l3) 25) // Commented out because they take a long time to run
;(check-expect (sup-1 l4) 25)
;(check-expect (sup-1 15) 59)


;; these functions are slow because you are forced to run through lists over and over again
;; for the if statement, you need to compare one number to the rest of the list to return a boolean
;; and at the end of it, you have to recur again

;; Nelon -> Number
;; determines the smallest 
;; number on l

(define (inf lon)
  (cond
    [(empty? (rest lon))
     (first lon)]
    [else
     (min (first lon) (inf (rest lon)))]))

(check-expect (inf l1) 0)
(check-expect (inf l2) 0)
(check-expect (inf l3) 1)
(check-expect (inf l4) 1)
(check-expect (inf l5) 0)

;; the second version of the function is a lot faster because
;; there is a lot less recursion going on so it is more efficeint 

; Nelon -> Number
; determines the largest 
; number on l
(define (sup lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else (max (first lon) (sup (rest lon)))]))

(check-expect (sup l1) 3)
(check-expect (sup l2) 3)
(check-expect (sup l3) 25)
(check-expect (sup l4) 25)
(check-expect (sup l5) 59)
    
;; NeLon [ [Real Real ... ] -> Real ]
; where real means real number

;; determines the biggest or smallest number in l

(define (abstract2 lon op)
  (cond
    [(empty? (rest lon))(first lon)]
    [else (op (first lon) (abstract2 (rest lon) op))]))

(check-expect (abstract2 l1 max) 3)
(check-expect (abstract2 l1 min) 0)
(check-expect (abstract2 l2 max) 3)
(check-expect (abstract2 l2 min) 0)
(check-expect (abstract2 l5 max) 59)
(check-expect (abstract2 l5 min) 0)

;; NeLon -> Number
;; returns the smallest number in the list

(define (inf-2 lon)
  (abstract2 lon min))

(check-expect (inf-2 l3) 1)
(check-expect (inf-2 l4) 1)
(check-expect (inf-2 l5) 0)

;; NeLon -> Number
;; returns the largest number in the list

(define (sup-2 lon)
  (abstract2 lon max))
    
(check-expect (sup-2 l3) 25)
(check-expect (sup-2 l4) 25)
(check-expect (sup-2 l5) 59)

; inf-2 and sup-2 run much faster because you're only recurring once every time the function runs

;; ----------- EXERCISE FOUR -----------

;; Here is one more parametric data definition:
; A [Maybe X] is one of: 
; – #false 
; – X

;; Interpret these data definitions:
;; [Maybe String], [Maybe [List-of String]], and [List-of [Maybe String]].

;; a [Maybe String] is one of:
; - String
; #false

;; a [Maybe List-of String] is one of:
; - List-of String
; - #false

;; a [List-of [Maybe String]] is a list of
;; [Maybe String] which is comprised of 
; -strings
; #false

;;What does the following function signature mean:
; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise

;; the function function signature means that
;; the function takes in a string and a list of strings
;; and outputs either #false or a list of strings


(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(check-expect (occurs "hi" '()) #f)
(check-expect (occurs " " (list "b" "c" "d")) #f)


(define (occurs str alos)
  (cond [(empty? alos) #false]
        [(cons? alos)
         (if (string=? (first alos) str)
             (rest alos)
             (occurs str (rest alos)))]))
