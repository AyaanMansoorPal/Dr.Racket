;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;**********************************************
;; Ayaan Mansoor Pal 
;;**********************************************
;;










;; Points is a nonempty list of natural numbers.
;; Useful examples for Points:
(define a (list (list 14 0 10 9)
                (list 7 7 3 5)
                (list 8 8 5 3)))

(define b (list (list 1 4)
                (list 5 2)))
(define c (list (list 0 0 0 0)
                (list 0 0 0 0)))
(define d (list (list 2 3 4 )))
(define f (list (list 3 4 9 8)
                (list 3 2 4 5)
                (list 0 1 2 3)))
(define h (list (list 0 0 0 0)
                (list 0 0 0 0)
                (list 0 0 0 0)))


;; (mini item other) consumes an item, and a list other
;; and outputs the minimum valu of the list
;; mini: Nat (listof Nat)-> (listof Num)
;; For Tests and Examples see (check-for-min first-item other-item)

(define (mini item other)
    (min item other))

;; (check-for-min first-item other-item) consumes an item from
;; a list and rest of the list and outputs a list of minimum values
;; check-for-min (Anyof (listof Nat) Nat) -> (listof Nat)
;; For Tests and Examples see (find-min alist)

(define (check-for-min first-item other-item)
  (cond
      [(cons? first-item) (cons (foldr mini
          (first first-item) (rest first-item)) other-item)]))

;; (find-min alist) consumes a nested or regular alist
;; and outputs a list with the minimum value from the
;; nested alist or the minimum value in the regular alist
;; find-min: (listof Nat)-> (listof Nat)
;; requires: non-empty list

(define (find-min alist)
 (foldr check-for-min empty alist))
    
;; Examples and Tests:
(check-expect(find-min a) (list 0 3 3))
(check-expect(find-min b) (list 1 2))
(check-expect(find-min c) (list 0 0))
(check-expect(find-min d) (list 2))
(check-expect(find-min f) (list 3 2 0))
(check-expect(find-min h) (list 0 0 0))


;; (find-max nlist) consumes a list nlist
;; and outputs the max value in that list
;; find-max: (listof Nat)-> Nat
;; See examples and tests for (winning-score all-points)
;; for this function

(define (find-max nlist)
    (foldr (lambda (x y) (max x y)) (first nlist) nlist))

;; (winning-score all-points) consumes a list all-points
;; and outputs the max value in that list.
;; winning-score: (listof Nat)-> Nat
;; requires: non-empty list

(define (winning-score all-points)
  (find-max (find-min all-points)))

(check-expect (winning-score a) 3)
(check-expect (winning-score b) 2)
(check-expect (winning-score c) 0)
(check-expect (winning-score d) 2)
(check-expect (winning-score f) 3)
(check-expect (winning-score h) 0)