;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProblemSet10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
(define-struct page [title content])
; A Wiki is a [List-of Page]
; A Page is a (make-page Symbol [List-of Symbol])
; INTERPRETATION: An individual page contains a title and a
; list of links to other pages (represented by their titles).
(define wiki
  (list
   (make-page 'a '(b))
   (make-page 'b '(a c d))
   (make-page 'c '(a b d))
   (make-page 'd '(a))
   (make-page 'e '())
   (make-page 'f  '(e))))

; Ex 2: out-links

; searches for a page and all links given a name and wiki
; symbol wiki -> [List-of Page]
(define (out-links page wiki)
  (cond
    [(empty? wiki) '()]
    [(cons? wiki) (if (symbol=? (page-title (first wiki)) page)
                      (page-content (first wiki))
                      (out-links page (rest wiki)))]))
(check-expect (out-links 'a wiki) '(b))
(check-expect (out-links 'b wiki) '(a c d))
(check-expect (out-links 'c wiki) '(a b d))
(check-expect (out-links 'e wiki) '())
(check-expect (out-links 'f wiki) '(e))

; Ex 3: in-links

; retruns all pages that link to the given page
; symbol wiki -> [List-of Pages]
(define (in-links page wiki)
  (cond
    [(empty? wiki) '()]
    [(cons? wiki) (if (member? page (page-content (first wiki)))
                      (cons (page-title (first wiki)) (in-links page (rest wiki)))
                      (in-links page (rest wiki)))]))
(check-expect (in-links 'a wiki) '(b c d))
(check-expect (in-links 'b wiki) '(a c))
(check-expect (in-links 'c wiki) '(b))
(check-expect (in-links 'e wiki) '(f))
(check-expect (in-links 'f wiki) '())

; Ex 4: cycle?

; Temp for generative recursion

#;(define (general P)
    (cond
      [(trivial? P) (solve P)]
      [else
       (combine-solutions
        P
        (general
         (generate P)))]))

; Functions

; checks if there are any cycles in the wiki
; wiki -> boolean
(define (cycle? wiki)
  (cond
    ; Termination Arg: If the wiki is empty, there are no cycles
    [(empty? wiki) #f]
    ; Termination Arg: When the wiki contains only pages with both in-links and out-links,
    ;                  there must be a cycle
    [(all-connected? wiki) #t] 
    [else
     ; Generative Idea: Generates a new wiki, removing any pages
     ;                  that don't have both in-links and out-links
     (cycle? (keep-in-and-out wiki))]))
(check-expect (cycle? (list
                       (make-page 'a '(b))
                       (make-page 'b '(c))
                       (make-page 'c '()))) #f)
(check-expect (cycle? (list
                       (make-page 'a '(b))
                       (make-page 'b '(c))
                       (make-page 'c '(a)))) #t)
(check-expect (cycle? (list
                       (make-page 'a '(b))
                       (make-page 'b '(c))
                       (make-page 'c '(a)))) #t)
(check-expect (cycle? (list
                       (make-page 'a '(b))
                       (make-page 'b '(c)) 
                       (make-page 'c '(e)))) #f)
(check-expect (cycle? (list
                       (make-page 'a '(c))
                       (make-page 'b '(c))
                       (make-page 'c '()))) #f)
(check-expect (cycle? (list
                       (make-page 'a '(c))
                       (make-page 'b '(b c))
                       (make-page 'c '()))) #t)
(check-expect (cycle? wiki) #t)

; filters out pages that do not have both in-links and out-links
; wiki -> wiki
(define (keep-in-and-out wiki)
  ; [X] [X -> Boolean] [List-of X] -> [List-of X]
  (filter (λ (p) (in-and-out? (page-title p) wiki)) wiki))
(check-expect (keep-in-and-out wiki)
              (list (make-page 'a (list 'b))
                    (make-page 'b (list 'a 'c 'd))
                    (make-page 'c (list 'a 'b 'd))
                    (make-page 'd (list 'a))))

; checks if the given page has both in-links and out-links
; page wiki -> boolean
(define (in-and-out? page wiki)
  (and (cons? (in-links page wiki))
       (cons? (out-links page wiki))))
(check-expect (in-and-out? 'a wiki) #t)
(check-expect (in-and-out? 'b wiki) #t)
(check-expect (in-and-out? 'e wiki) #f)

; determines if all pages have at least one in-link and out-link.
; wiki -> boolean
(define (all-connected? wiki)
  ; [X] [X -> Boolean] -> [List-of X]
  (andmap (λ (p) (in-and-out? (page-title p) wiki)) wiki))
(check-expect (all-connected? wiki) #f)
(check-expect (all-connected? (keep-in-and-out wiki)) #t)