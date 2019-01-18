;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProblemSet8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lab8-teachpack.rkt")

#|---------- Excercise 1 ----------|#

; A Road is one of:
; - 'dead-end
; - (make-straightaway String PositiveNumber Road)
; - Intersection
(define-struct straightaway [name distance more])

; INTERPRETATION: A road with some name and some amount of distance
; until the next portion of road

 
; An Intersection is a [List-of Road]

(define newbury (make-straightaway "newbury" 5 'dead-end))
(define hemenway (make-straightaway "hemenway" 8 'dead-end))
(define st-stephen (make-straightaway "st stephen" 13 hemenway))
(define forsyth (make-straightaway "forsyth" 3 st-stephen))
(define int1 (list newbury forsyth))
(define fenway (make-straightaway "fenway" 10 int1))
(define int2 (list newbury hemenway))

;; total-road-length: Road -> Number
;; calculates the total length of the road and all its connected roads

(define (total-road-length road)
  (cond
    [(symbol? road) 0]
    [(straightaway? road) (add-straightaway road)]
    [(list? road) (add-connecting road)]))

(check-expect (total-road-length 'dead-end) 0)
(check-expect (total-road-length newbury) 5)
(check-expect (total-road-length forsyth) 24)
(check-expect (total-road-length fenway) 39)
(check-expect (total-road-length int2) 13)


;; add-straightaway: Straightaway -> Number
;; calculates the length of the straightaway

(define (add-straightaway sa)
  (+ (total-road-length (straightaway-more sa))
     (straightaway-distance sa)))
(check-expect (add-straightaway forsyth) 24)
(check-expect (add-straightaway newbury) 5)

;; add-connecting: Intersection -> Number
;; calculates the length of the intersection

(define (add-connecting intersection)
  (foldr (lambda (x sofar)
           (+ (total-road-length x) sofar))
         0 intersection))
(check-expect (add-connecting int1) 29)
(check-expect (add-connecting int2) 13)
(check-expect (add-connecting '()) 0)


#|---------- Excercise 2 ----------|#

;; road-names: Road -> List-of String
;; produces a list of names of roads connected to the given road
;; including the name of that road if it has one!

(define (road-names road)
  (cond
    [(symbol? road) '() ]
    [(straightaway? road) (straightaway-list road)]
    [(list? road) (intersection-list road)]))
(check-expect (road-names 'dead-end) '())
(check-expect (road-names newbury) (list "newbury"))
(check-expect (road-names fenway) (list "fenway" "newbury" "forsyth" "st stephen" "hemenway"))

;; straightaway-list: Straightaway -> List-of String
;; makes a list of roads connected to the straightaway

(define (straightaway-list straightaway)
  (cons (straightaway-name  straightaway)
        (road-names (straightaway-more straightaway))))
(check-expect (straightaway-list forsyth) (list "forsyth" "st stephen" "hemenway"))
(check-expect (straightaway-list newbury) (list "newbury"))

;; intersection-list: Intersection -> List-of String
;; makes a list of roads connected to the intersection

(define (intersection-list intersection)
  (foldr append '() (map road-names intersection)))
(check-expect (intersection-list int1)
              (list "newbury"
                    "forsyth"
                    "st stephen"
                    "hemenway"))
(check-expect (intersection-list int2) (list "newbury" "hemenway"))
(check-expect (intersection-list '()) '())

#|---------- Excercise 3 ----------|#


; A JSON is one of:
; - String
; - Number
; - Boolean
; - 'null
; - JSONArray
; - JSONObject
 
; A JSONArray is a [List-of JSON]
 
; A JSONObject is a (make-object [List-of JSONPair])
 
; A JSONPair is a (list Symbol JSON)


; ----------- TEMPS -----------

; json-temp : JSON -> ???
#;(define (json-temp ajson)
    (cond
      [(string? ajson) ...]
      [(number? ajson) ...]
      [(boolean? ajson) ...]
      [(symbol? ajson) ...]
      [(json-array? ajson) ...(json-array-temp ajson) ...]
      [(json-object? ajson) ... (json-object-temp ajson) ...]))
 
; json-array-temp : JSONArray -> ???
#;(define (json-array-temp ja)
    (cond
      [(empty? ja) ...]
      [(cons? ja)
       ... (json-temp (first ja)) ...
       ... (json-array-temp (rest ja)) ...]))
 
 
; json-object-temp : JSONObject -> ???
#;(define (json-object-temp jo)
    (... (list-of-json-pair-temp (object-content jo)) ...))
 
; list-of-json-pair-temp : [List-of JSONPair] -> ???
#;(define (list-of-json-pair-temp lojp)
    (cond [(empty? lojp) ...]
          [(cons? lojp) ... (json-pair-temp (first lojp)) ...
                        ... (list-of-json-pair-temp (rest lojp)) ...]) ...)
 
; json-pair-temp : JSONPair -> ???
#;(define (json-pair-temp jp)
    (... (first jp) ... (json-temp (second jp)) ...)) 

;; flatten-json: JSON -> [List-of (list String JSON)]
;; IF a String, Number, Boolean, or 'null, returns (list (list X j))
;; where X is one of: "STRING" "NUMBER" "BOOLEAN" "NULL" depending on what j is
;; IF a JSONOBJECT, then names of its pairs will be string-appended
;; onto the names of the flattening of associated value
;; IF a JSONArray, then the indices of its values will be string-appended
;; onto the names of the flattening of the associated value

; Flattens a given JSON
;; flatten-json: JSON -> [List-of (list String JSON)]
(define (flatten-json ajson)
  (cond
    [(string? ajson) (list (list "STRING" ajson))]
    [(number? ajson) (list (list "NUMBER" ajson))]
    [(boolean? ajson) (list (list "BOOLEAN" ajson))]
    [(symbol? ajson) (list (list "NULL" ajson))]
    [(empty? ajson) (list (list "EMPTY" '()))]
    [(json-array? ajson) (flatten-json-array ajson)]
    [(json-object? ajson) (flatten-json-object ajson)]))
(check-expect (flatten-json "hi") (list (list "STRING" "hi")))
(check-expect (flatten-json 7) (list (list "NUMBER" 7)))
(check-expect (flatten-json #false) (list (list "BOOLEAN" #false)))
(check-expect (flatten-json 'null) (list (list "NULL" 'null)))
(check-expect (flatten-json '()) (list (list "EMPTY" '())))
(check-expect (flatten-json
               (list 0 #true (list "hello" "hi")))
              (list
               (list "0-NUMBER" 0)
               (list "1-BOOLEAN" #true)
               (list "2-0-STRING" "hello")
               (list "2-1-STRING" "hi")))
(check-expect (flatten-json
               (make-object
                   (list (list 'formal "hello")
                         (list 'informal "howdy"))))
              (list 
               (list "formal-STRING" "hello")
               (list "informal-STRING" "howdy")))
(check-expect (flatten-json
               (list 'null '()
                     (make-object
                         (list (list 'greeting
                                     (make-object
                                         (list (list 'formal "hello")
                                               (list 'informal "howdy")
                                               (list 'garbage (list true 1)))))))))
              (list (list "0-NULL" 'null)
                    (list "1-EMPTY" '())
                    (list "2-greeting-formal-STRING" "hello")
                    (list "2-greeting-informal-STRING" "howdy")
                    (list "2-greeting-garbage-0-BOOLEAN" true)
                    (list "2-greeting-garbage-1-NUMBER" 1)))

;; JSONObject -> [List-of (list String JSON)]
;; breaks down the JSONObject and sends it to be processed by flatten-list-of-json-pair
(define (flatten-json-object jo)
  (flatten-list-of-json-pair (object-content jo)))
(check-expect (flatten-json-object
               (make-object
                   (list (list 'formal "hello")
                         (list 'informal "howdy"))))
              (list 
               (list "formal-STRING" "hello")
               (list "informal-STRING" "howdy")))
(check-expect (flatten-json-object
               (make-object (list (list 'nothing 'null)
                                  (list 'yes #true))))
              (list
               (list "nothing-NULL" 'null)
               (list "yes-BOOLEAN" #true)))

;; [List-of JSONPair] -> [List-of (list String JSON)]
;; appends the lists created flatten-json-pair
(define (flatten-list-of-json-pair lojp)
  (cond [(empty? lojp) '()]
        [(cons? lojp)
         (append (flatten-json-pair (first lojp))
                 (flatten-list-of-json-pair (rest lojp)))]))
(check-expect (flatten-list-of-json-pair '()) '() )
(check-expect (flatten-list-of-json-pair
               (list (list 'formal "hello")
                     (list 'informal "howdy")))
              (list 
               (list "formal-STRING" "hello")
               (list "informal-STRING" "howdy")))
(check-expect (flatten-list-of-json-pair
               (list (list 'formal
                           (list "a" "b" "c"))
                     (list 'informal "howdy")))
              (list
               (list "formal-0-STRING" "a")
               (list "formal-1-STRING" "b")
               (list "formal-2-STRING" "c")
               (list "informal-STRING" "howdy")))      

;; JSONPair -> [List-of (list String JSON)]
;; adds the symbol from the JSONPair and applies to what's contained in the JSON part of the pair
(define (flatten-json-pair jp)
  ;; [List-of (list String JSON)] -> [List-of (list String JSON)]
  ;;input: (list String JSON)
  ;; -> output: (list String JSON)] [List-of (list String JSON)] -> List-of (list String JSON) 
  (map (lambda (x)
         (list
          (string-append
           (symbol->string  (first jp))
           "-"
           (first x))
          (second x)))
       ;;[List-of (list String JSON)]
       (flatten-json (second jp))))
(check-expect (flatten-json-pair (list 'hi "hello"))
              (list (list "hi-STRING" "hello")))
(check-expect (flatten-json-pair (list 'hi 'greeting)) (list (list "hi-NULL" 'greeting)))

;; JSONArray -> [List-of (list String JSON)]]
; flattens a JSON array to be combined with the index
(define (flatten-json-array array)
  (combine-indices
   (build-list (length array) identity)
   (map flatten-json array)))
(check-expect (flatten-json-array
               (list 0 #true (list "hello" "hi")))
              (list
               (list "0-NUMBER" 0)
               (list "1-BOOLEAN" #true)
               (list "2-0-STRING" "hello")
               (list "2-1-STRING" "hi")))
(check-expect (flatten-json-array
               (list 0 #true (make-object
                                 (list (list 'hi "hello")
                                       (list 'you 7)))))
              (list
               (list "0-NUMBER" 0)
               (list "1-BOOLEAN" #true)
               (list "2-hi-STRING" "hello")
               (list "2-you-NUMBER" 7)))
(check-expect (flatten-json-array '() ) '() )

;; [List-of Number] [List-of [List-of (list String JSON)]] -> [List-of (list String JSON)]
;; combines the index with the flattened JSON
(define (combine-indices indices flats)
  (cond [(and (empty? indices) (empty? flats)) '()]
        [(and (cons? indices) (cons? flats))
         (append (map
                  (lambda (fj)
                    (list
                     (string-append
                      (number->string (first indices))
                      "-"
                      (first fj))
                     (second fj)))
                  (first flats))
                 (combine-indices (rest indices) (rest flats)))]
        [else (error "Something went wrong")]))
(check-expect (combine-indices '() '()) '())
(check-expect (combine-indices (list 0 1 2) (list (list (list "NUMBER" 0))
                                                  (list (list "BOOLEAN" #true))
                                                  (list (list "0-STRING" "hello")
                                                        (list "1-STRING" "hi"))))
              (list
               (list "0-NUMBER" 0)
               (list "1-BOOLEAN" #true)
               (list "2-0-STRING" "hello")
               (list "2-1-STRING" "hi")))
(check-error (combine-indices '() '("not-empty")) "Something went wrong")

;; [List-of JSON] -> [List-of Number]
;; creates a list of indices
(define (count-level l)
  (build-list
   (length l)
   identity))
(check-expect (count-level (list 1 "b" #f 'null)) (list 0 1 2 3))
(check-expect (count-level (list (list (list "NUMBER" 0))
                                 (list (list "BOOLEAN" #true))
                                 (list (list "0-STRING" "hello")
                                       (list "1-STRING" "hi"))))
              (list 0 1 2))