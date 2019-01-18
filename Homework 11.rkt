;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node [left val right])
 
; A NumTree is one of
; - Number
; - (make-node NumTree Number NumTree)
; INTERPRETATION: irrelevant

;; Exercise 4

;; produces the product of the sums of each path from root to each leaf
;; product-of-sums: NumTree -> Number

(check-expect (product-of-sums (make-node 1 2 3)) 15)
(check-expect (product-of-sums (make-node (make-node 2 4 6) 3 5)) 936) 
(check-expect (product-of-sums (make-node 1 0 3)) 3)

(define (product-of-sums a-nt)
  (local (;; NumTree Accumulator -> Number
          ;; Takes in a Numtree and an accumlator and returns the product of the sums of the branches
          ;; given (make-node 1 2 3) -> (* (+ 2 1) (+ 2 3)) -> 15
          ;; accumulator a is the value of the sums of numbers that have been summed up in
          ;; the branches of the nodes
          (define (product-of-sums/a a-nt acc)
            (cond
              [(number? a-nt) (+ acc a-nt)]
              [(node? a-nt) (* (product-of-sums/a (node-left a-nt) (+ (node-val a-nt) acc))
                               (product-of-sums/a (node-right a-nt) (+ (node-val a-nt) acc)))])))
    (product-of-sums/a a-nt 0)))


;; Exercise 5

;; consumes a list of digits and produces the corresponding number
;; The first item on the list is the most significant digit
;; to10 : [List-of Number] -> Number


(check-expect (to10 '(1 0 2)) 102)
(check-expect (to10 '(0 0 2 3)) 23)
(check-expect (to10 '()) 0)

(define (to10 l)
  (local (;; [List-of Number] Accumlator -> Number
          ;; Takes in a list of numbers and accumulator and produces the corresponding number
          ;; given '(0 2 0) -> 20
          ;; accumulator a is the value of the sum of numbers in the list times 10 to
          ;; a power
          (define (to10/a l acc)
            (cond
              [(empty? l) acc]
              [else (to10/a (rest l)
                            (+ (first l) (* 10 acc)))])))
    (to10/a l 0)))