;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProblemSet9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; EXERCISE ONE 

;; [List-of [List-of Any]] -> [List-of [List-of Any]]
;; lists the cartesian products of the lists contained in the list


(define (cartesian-product lol)
  (cond [(empty? lol) '()]
        [else
         (flatten (lists-and-list (drop-right lol)
                                  (list-ref lol (- (length lol) 1))))]))

                      

(check-expect (cartesian-product '((0 1 2)  (3 4)))
              '((0 3) (0 4) (1 3) (1 4) (2 3) (2 4)))

(check-expect (cartesian-product '((a b) (red green blue) (hutt putt)))
              '((a red hutt)
                (a red putt)
                (a green hutt)
                (a green putt)
                (a blue hutt)
                (a blue putt)
                (b red hutt)
                (b red putt)
                (b green hutt)
                (b green putt)
                (b blue hutt)
                (b blue putt)))
(check-expect (cartesian-product '()) '())
(check-expect (cartesian-product (list (list 'a 'b)))
              (list (list 'a) (list'b)))

;; drop-right: [List-of X] -> [List-of X]
; drops the last term in the list

(define (drop-right list)
  (cond [(empty? (rest list)) '()]
        [(cons? list)
         (cons (first list)
               (drop-right (rest list)))]))

(check-expect (drop-right (list 5 4 3 2))
              (list 5 4 3))
           
                   


;; [List-of [List-of Any]] [List-of Any] -> [List-of [List-of Any]]
;; creates the cartesian product between a list of lists and a list

(define (lists-and-list lol alist)
  (foldr (lambda (x sofar)
           (two-cartesian-product x sofar))
         alist
         lol))

(check-expect (lists-and-list
               (list (list 5 4) (list 3 4))
               (list 'a 'b))
              (list
               (list 5 (list 3 'a))
               (list 5 (list 3 'b))
               (list 5 (list 4 'a))
               (list 5 (list 4 'b))
               (list 4 (list 3 'a))
               (list 4 (list 3 'b))
               (list 4 (list 4 'a))
               (list 4 (list 4 'b))))

(check-expect (lists-and-list
               (list (list 5 4) (list 2 3) (list 8 7))
               (list 'a 'b))
              (list
               (list 5 (list 2 (list 8 'a)))
               (list 5 (list 2 (list 8 'b)))
               (list 5 (list 2 (list 7 'a)))
               (list 5 (list 2 (list 7 'b)))
               (list 5 (list 3 (list 8 'a)))
               (list 5 (list 3 (list 8 'b)))
               (list 5 (list 3 (list 7 'a)))
               (list 5 (list 3 (list 7 'b)))
               (list 4 (list 2 (list 8 'a)))
               (list 4 (list 2 (list 8 'b)))
               (list 4 (list 2 (list 7 'a)))
               (list 4 (list 2 (list 7 'b)))
               (list 4 (list 3 (list 8 'a)))
               (list 4 (list 3 (list 8 'b)))
               (list 4 (list 3 (list 7 'a)))
               (list 4 (list 3 (list 7 'b)))))



;; [List-of [List-of [List-of Any]]] -> [List-of [List-of Any]]
;; appends nested lists contained in the list of nested lists

(define (flatten lol)
  (map (lambda (x)
         (whysomanylists x)) lol))

(check-expect (flatten
               (list
                (list 5 (list 5 (list 5 'a)))
                (list 5 (list 5 (list 5 'b)))
                (list 5 (list 5 (list 7 'a)))
                (list 5 (list 5 (list 7 'b)))
                (list 5 (list 3 (list 5 'a)))
                (list 5 (list 3 (list 5 'b)))
                (list 5 (list 3 (list 7 'a)))
                (list 5 (list 3 (list 7 'b)))
                (list 4 (list 5 (list 5 'a)))
                (list 4 (list 5 (list 5 'b)))
                (list 4 (list 5 (list 7 'a)))
                (list 4 (list 5 (list 7 'b)))
                (list 4 (list 3 (list 5 'a)))
                (list 4 (list 3 (list 5 'b)))
                (list 4 (list 3 (list 7 'a)))
                (list 4 (list 3 (list 7 'b)))))
              (list
               (list 5 5 5 'a)
               (list 5 5 5 'b)
               (list 5 5 7 'a)
               (list 5 5 7 'b)
               (list 5 3 5 'a)
               (list 5 3 5 'b)
               (list 5 3 7 'a)
               (list 5 3 7 'b)
               (list 4 5 5 'a)
               (list 4 5 5 'b)
               (list 4 5 7 'a)
               (list 4 5 7 'b)
               (list 4 3 5 'a)
               (list 4 3 5 'b)
               (list 4 3 7 'a)
               (list 4 3 7 'b)))

(check-expect (flatten '()) '())

;; whysomanylists: [List-of [List-of Any]] -> [List-of Any]
;; appends a nested list together


(define (whysomanylists lol)
  (cond [(empty? lol) '()]
        [(cons? lol)
         (append
          (whysomanylists (first lol))
          (whysomanylists (rest lol)))]
        [else (list lol)]))


(check-expect (whysomanylists (list 5 (list 3 'a)))
              (list 5 3 'a))
(check-expect (whysomanylists '()) '())

;; [List-of X] [List-of Y] -> [List-of (list X Y)]
;; produces the cartesian product of the two lists

(define (two-cartesian-product l1 l2)
  (cond [(empty? l1) '()]
        [(cons? l1)
         (append
          (combine (first l1) l2)
          (two-cartesian-product (rest l1) l2))]))
               
  
(check-expect (two-cartesian-product '(0 1 2)  '(3 4))
              '((0 3) (0 4) (1 3) (1 4) (2 3) (2 4)))
(check-expect (two-cartesian-product '() '()) '())

;; X [List-of Y] -> [List-of (list X Y)]

(define (combine x l2)
  (map (λ (y)
         (list x y)) l2))

(check-expect (combine 5 (list 5 4 3))
              (list (list 5 5)
                    (list 5 4)
                    (list 5 3)))
(check-expect (combine 5 '()) '() )

;; EXERCISE TWO
; A LeafyBinaryTree is one of
;- 'leaf
;- (make-node leaf leaf)
(define-struct node (left right))

;; All possible leafy trees of size n
;; number -> [List-of LeafyBinaryTree]
(define (all-leafy-trees n)
  (cond [(zero? n) (list 'leaf)]
        [(= 1 n) (list (make-node 'leaf 'leaf))]
        [else (apply append(list(expand-right n)
                                (expand-left n)
                                (expand-both n)))]))
(check-expect (all-leafy-trees 1)
              (list (make-node 'leaf 'leaf)))
(check-expect (all-leafy-trees 2)
              (list
               (make-node (make-node 'leaf 'leaf) 'leaf)
               (make-node 'leaf (make-node 'leaf 'leaf))
               (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))
;(check-expect (length (all-leafy-trees 3)) 21)

; Expands the right node
; Number -> LBT
(define (expand-right n)
  (map (λ (x)
         (make-node x 'leaf))
       (all-leafy-trees (sub1 n))))
(check-expect (expand-right 3)
              (list
               (make-node (make-node (make-node 'leaf 'leaf) 'leaf)
                          'leaf)
               (make-node (make-node 'leaf (make-node 'leaf 'leaf))
                          'leaf)
               (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))
                          'leaf)))

; Expands the left node
; Number -> LBT
(define (expand-left n)
  (map (λ (x)
         (make-node 'leaf x))
       (all-leafy-trees (sub1 n))))
(check-expect (expand-left 3)
              (list
               (make-node 'leaf
                          (make-node (make-node 'leaf 'leaf) 'leaf))
               (make-node 'leaf
                          (make-node 'leaf (make-node 'leaf 'leaf)))
               (make-node 'leaf
                          (make-node (make-node 'leaf 'leaf)(make-node 'leaf 'leaf)))))

; Expands the both nodes
; Number -> LBT
(define (expand-both n)
  (map (λ (x)
         (make-node x x))
       (all-leafy-trees (sub1 n))))
(check-expect (expand-both 3)
              (list
               (make-node (make-node (make-node 'leaf 'leaf) 'leaf)
                          (make-node (make-node 'leaf 'leaf) 'leaf))
               (make-node (make-node 'leaf (make-node 'leaf 'leaf))
                          (make-node 'leaf (make-node 'leaf 'leaf)))
               (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))
                          (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))))

(define (next-lbt n)
  (all-leafy-trees (sub1 n)))
