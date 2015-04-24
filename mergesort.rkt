;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;; *************************************
;; Mergesort for generic comparable type
;; @author Kunal Khamar
;; *************************************
;;


;; Constants for testing
(define loc-ate (string->list "ate"))
(define loc-tea (string->list "tea"))
(define loc-aet (string->list "aet"))
(define loc-forest (string->list "forest"))
(define loc-foster (string->list "foster"))
(define loc-eforst (string->list "eforst"))


;; (mergesort lst cmp)
;;   Produces a list that contains the elements
;;   of lst, sorted in order defined by cmp
;; mergesort: (listof X) (X X -> Bool) -> (listof X)
;; Examples:
(check-expect (mergesort loc-ate char<?) loc-aet)
(check-expect (mergesort loc-forest char<?) loc-eforst)
(check-expect (mergesort '(3 1 2) >) '(3 2 1))

(define (mergesort lst cmp)
  (local
    [;; (integer-divide a b)
     ;;    Produces the result of integer division
     ;;    of a and b, that is, the quotient upon
     ;;    a/b
     ;; integer-divide: Nat Nat -> Nat
     ;; requires: b > 0
     (define (integer-divide a b)
       (inexact->exact (floor (/ a b))))
     
     ;; (first-n list n)
     ;;   Produces the first n elements of the
     ;;   given list
     ;; first-n: (listof Any) Nat -> (listof Any)
     (define (first-n list n)
       (cond
         [(or (= n 0) (empty? list)) empty]
         [else
          (cons (first list) (first-n (rest list) (sub1 n)))]))
     
     ;; (rest-n list n)
     ;;   Produces the elements of the list following
     ;;   the n-th element. In other words, discards
     ;;   the first n elements from the given list
     ;;   and returns the result
     ;; rest-n: (listof Any) Nat -> (listof Any)
     (define (rest-n list n)
       (cond
         [(or (= n 0) (empty? list)) list]
         [else
          (rest-n (rest list) (sub1 n))]))
     
     ;; (merge lst1 lst2 cmp)
     ;;   Produces a single list that contains the
     ;;   elements of (already sorted) lst1 and lst2 
     ;;   in order enforced by cmp
     ;; merge: (listof X) (listof X) (X X -> Bool) -> (listof X)
     ;; requires: lst1 and lst2 are sorted as per cmp
     (define (merge lst1 lst2 cmp)
       (cond
         [(empty? lst1) lst2]
         [(empty? lst2) lst1]
         [(cmp (first lst1) (first lst2))
          (cons (first lst1) (merge (rest lst1) lst2 cmp))]
         [else
          (cons (first lst2) (merge lst1 (rest lst2) cmp))]))]
    
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) lst]
      [else
       (local
         [(define break-index (integer-divide (length lst) 2))]
         (merge (mergesort (first-n lst break-index) cmp)
                (mergesort (rest-n lst break-index) cmp)
                cmp))])))

;; Tests:
(check-expect (mergesort empty char<?) empty)
(check-expect (mergesort loc-eforst char<?) loc-eforst)
(check-expect (mergesort (string->list "tsrofe") char<?) loc-eforst)
(check-expect (mergesort (string->list "a") char<?) (string->list "a"))
(check-expect (mergesort (string->list "aba") char<?) (string->list "aab"))